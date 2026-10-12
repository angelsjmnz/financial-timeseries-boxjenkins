# =============================================================
# bj_forecast.R
# MODULO 4 del motor Box-Jenkins: PREDICCION
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Tres rutas de prediccion, segun el modelo y la diagnosis:
#   - predecir_garch()  : ARMA-GARCH; media + volatilidad condicional,
#                         intervalos con cuantil t-Student (nu estimado).
#   - predecir_arima()  : ARIMA con normalidad no rechazada; intervalos
#                         gaussianos clasicos.
#   - predecir_bootstrap_prr() : ARIMA con normalidad rechazada;
#                         bootstrap de Pascual, Romo & Ruiz (JTSA 2004),
#                         sin supuesto gaussiano, incorporando la
#                         incertidumbre de parametros y de innovaciones.
#
# Todas transforman al espacio de precios: P_{t+h} = P_t * exp(sum r).
# Para las rutas parametricas la varianza del retorno acumulado se
# suma por horizonte (exacto con media ARMA(0,0); aproximado con
# AR/MA). Para PRR los intervalos de precio salen directamente de los
# cuantiles de las trayectorias bootstrap (sin ninguna aproximacion).
# =============================================================

# -------------------------------------------------------------
# 4.A  PREDICCION ARMA-GARCH
# -------------------------------------------------------------

#' Predice media y volatilidad condicional con ARMA-GARCH; intervalos t
#'
#' @param garch_fit uGARCHfit.
#' @param h          integer.
#' @param levels     numeric. Niveles de confianza.
#' @return data.frame por horizonte con media, sigma e intervalos de retorno.
predecir_garch <- function(garch_fit, h, levels = c(80, 95)) {
  
  fc  <- rugarch::ugarchforecast(garch_fit, n.ahead = h)
  mu  <- as.numeric(rugarch::fitted(fc))
  sig <- as.numeric(rugarch::sigma(fc))
  nu  <- as.numeric(rugarch::coef(garch_fit)["shape"])
  
  out <- data.frame(horizonte = seq_len(h),
                    pred_return = mu, sigma = sig,
                    stringsAsFactors = FALSE)
  for (lv in levels) {
    a  <- (1 - lv / 100) / 2
    qa <- rugarch::qdist("std", p = a, mu = 0, sigma = 1, shape = nu)  # cuantil t
    out[[sprintf("lower%d", lv)]] <- mu + qa * sig
    out[[sprintf("upper%d", lv)]] <- mu - qa * sig
  }
  attr(out, "nu")        <- nu
  attr(out, "boot_paths") <- NULL
  out
}


# -------------------------------------------------------------
# 4.B  PREDICCION ARIMA (gaussiana)
# -------------------------------------------------------------

#' Predice con ARIMA e intervalos gaussianos clasicos
#'
#' @param arima_fit objeto Arima.
#' @param h          integer.
#' @param levels     numeric.
#' @return data.frame por horizonte con media, sigma e intervalos.
predecir_arima <- function(arima_fit, h, levels = c(80, 95)) {
  
  fc <- forecast::forecast(arima_fit, h = h, level = levels)
  out <- data.frame(horizonte = seq_len(h),
                    pred_return = as.numeric(fc$mean),
                    sigma = NA_real_, stringsAsFactors = FALSE)
  for (j in seq_along(levels)) {
    lv <- levels[j]
    out[[sprintf("lower%d", lv)]] <- as.numeric(fc$lower[, j])
    out[[sprintf("upper%d", lv)]] <- as.numeric(fc$upper[, j])
  }
  lv_max <- max(levels)
  z <- qnorm(1 - (1 - lv_max / 100) / 2)
  out$sigma <- (out[[sprintf("upper%d", lv_max)]] - out$pred_return) / z
  attr(out, "boot_paths") <- NULL
  out
}


# -------------------------------------------------------------
# 4.C  PREDICCION BOOTSTRAP PRR (Pascual, Romo & Ruiz, 2004)
# -------------------------------------------------------------

#' Intervalos de prediccion bootstrap PRR para ARIMA, sin supuesto normal
#'
#' Algoritmo de 5 pasos:
#'   1. Residuos centrados y reescalados por el factor de Stine (1987).
#'   2. Serie bootstrap Y*_t = fitted + residuos remuestreados.
#'   3. Reestimar el modelo en Y*_t -> parametros bootstrap.
#'   4. Generar la trayectoria futura condicionando en las ULTIMAS obs.
#'      ORIGINALES, con parametros bootstrap e innovaciones futuras
#'      remuestreadas (simulate future = TRUE). Esto incorpora tanto la
#'      incertidumbre de parametros como la de las innovaciones.
#'   5. Intervalos = cuantiles de la distribucion bootstrap por horizonte.
#'
#' Nota: a diferencia de los intervalos gaussianos, los PRR pueden ser
#' asimetricos, capturando asimetria y colas pesadas de las innovaciones.
#'
#' @param arima_fit objeto Arima.
#' @param serie_ts  numeric/ts. Serie estacionaria usada en la estimacion.
#' @param h          integer.
#' @param levels     numeric.
#' @param B          integer. Replicas bootstrap (minimo recomendado 999).
#' @return data.frame por horizonte; attr "boot_paths" = matriz B x h.
predecir_bootstrap_prr <- function(arima_fit, serie_ts, h,
                                   levels = c(80, 95), B = 999L) {
  
  ord <- forecast::arimaorder(arima_fit)
  x   <- as.numeric(serie_ts); x <- x[is.finite(x)]
  Tn  <- length(x)
  k   <- length(coef(arima_fit))
  tiene_media <- "intercept" %in% names(coef(arima_fit))
  frec <- frequency(serie_ts)
  
  # Paso 1: residuos centrados y reescalados (Stine)
  res   <- as.numeric(residuals(arima_fit)); res <- res[is.finite(res)]
  F_hat <- (res - mean(res)) * sqrt(Tn / max(Tn - k, 1))
  fitted_orig <- as.numeric(fitted(arima_fit))
  
  mat <- matrix(NA_real_, nrow = B, ncol = h)
  for (b in seq_len(B)) {
    # Paso 2: serie bootstrap con parametros originales
    y_star <- stats::ts(fitted_orig + sample(F_hat, length(fitted_orig), replace = TRUE),
                        frequency = frec)
    # Paso 3: reestimar en la serie bootstrap
    fit_star <- tryCatch(
      forecast::Arima(y_star, order = ord, include.mean = tiene_media, method = "ML"),
      error = function(e) NULL)
    if (is.null(fit_star)) { mat[b, ] <- NA_real_; next }
    
    # Paso 4: trayectoria futura condicionando en los datos ORIGINALES,
    # con parametros bootstrap e innovaciones futuras remuestreadas
    fit_cond <- tryCatch(forecast::Arima(serie_ts, model = fit_star),
                         error = function(e) NULL)
    if (is.null(fit_cond)) { mat[b, ] <- NA_real_; next }
    innov <- sample(F_hat, h, replace = TRUE)
    path  <- tryCatch(
      as.numeric(simulate(fit_cond, nsim = h, future = TRUE, innov = innov)),
      error = function(e) as.numeric(forecast::forecast(fit_cond, h = h)$mean))
    mat[b, ] <- path
  }
  
  # Paso 5: cuantiles por horizonte
  out <- data.frame(horizonte = seq_len(h),
                    pred_return = apply(mat, 2, mean, na.rm = TRUE),
                    sigma = apply(mat, 2, sd, na.rm = TRUE),
                    stringsAsFactors = FALSE)
  for (lv in levels) {
    a <- (1 - lv / 100) / 2
    out[[sprintf("lower%d", lv)]] <- apply(mat, 2, quantile, probs = a,     na.rm = TRUE)
    out[[sprintf("upper%d", lv)]] <- apply(mat, 2, quantile, probs = 1 - a, na.rm = TRUE)
  }
  attr(out, "boot_paths") <- mat   # trayectorias por-paso, para precios
  out
}


# -------------------------------------------------------------
# 4.D  TRANSFORMACION AL ESPACIO DE PRECIOS
# -------------------------------------------------------------

#' Convierte predicciones de retorno en trayectoria de precios
#'
#' Rutas parametricas (GARCH/ARIMA): media y sd del retorno ACUMULADO
#' (var acumulada = suma de sigma^2), cuantil normal (CLT sobre la suma).
#' Ruta PRR (si hay boot_paths): precios por cuantiles de las
#' trayectorias bootstrap acumuladas, sin aproximacion.
#'
#' @param ret_df     data.frame de una funcion predecir_*.
#' @param last_price numeric.
#' @param levels     numeric.
#' @return ret_df con columnas de precio.
to_price_space <- function(ret_df, last_price, levels = c(80, 95)) {
  
  mp <- attr(ret_df, "boot_paths")
  
  if (is.null(mp)) {
    # Parametrico: acumular media y varianza
    mu_cum <- cumsum(ret_df$pred_return)
    sd_cum <- sqrt(cumsum(ret_df$sigma^2))
    ret_df$pred_price <- last_price * exp(mu_cum)
    for (lv in levels) {
      z <- qnorm(1 - (1 - lv / 100) / 2)
      ret_df[[sprintf("price_lower%d", lv)]] <- last_price * exp(mu_cum - z * sd_cum)
      ret_df[[sprintf("price_upper%d", lv)]] <- last_price * exp(mu_cum + z * sd_cum)
    }
  } else {
    # PRR: trayectorias de precio bootstrap = last_price * exp(cumsum ret)
    price_paths <- last_price * exp(t(apply(mp, 1, cumsum)))  # B x h
    ret_df$pred_price <- apply(price_paths, 2, mean, na.rm = TRUE)
    for (lv in levels) {
      a <- (1 - lv / 100) / 2
      ret_df[[sprintf("price_lower%d", lv)]] <- apply(price_paths, 2, quantile, probs = a,     na.rm = TRUE)
      ret_df[[sprintf("price_upper%d", lv)]] <- apply(price_paths, 2, quantile, probs = 1 - a, na.rm = TRUE)
    }
  }
  ret_df
}


# -------------------------------------------------------------
# ORQUESTADOR DEL MODULO 4
# -------------------------------------------------------------

#' Genera la prediccion completa a horizonte h (dispatch por ruta)
#'
#' @param est        list. Salida de bj_estimate().
#' @param serie      numeric/xts. Serie de log-retornos (para PRR).
#' @param last_price numeric. Ultimo precio observado.
#' @param last_date  Date. Fecha de la ultima observacion.
#' @param h          integer. Horizonte.
#' @param levels     numeric. Niveles de confianza.
#' @param diag       list o NULL. Salida de bj_diagnose (flag PRR).
#' @param B_prr      integer. Replicas bootstrap PRR.
#' @param save       logical.
#' @param verbose    logical.
#' @return list con df de prediccion y metadatos.
bj_forecast <- function(est, serie, last_price, last_date, h = 30L,
                        levels = c(80, 95), diag = NULL, B_prr = 999L,
                        save = TRUE, verbose = TRUE) {
  
  ticker <- est$ticker
  if (verbose) cat(sprintf("\n[PREDICCION] %s  |  h = %d  |  modelo: %s\n",
                           ticker, h, est$modelo_principal))
  
  if (est$modelo_principal == "arma_garch" && !is.null(est$garch)) {
    ret_df <- predecir_garch(est$garch$fit, h = h, levels = levels)
    metodo <- sprintf("ARMA-GARCH t-Student (nu=%.2f)", attr(ret_df, "nu"))
    
  } else if (!is.null(diag) && isTRUE(diag$usar_bootstrap_prr)) {
    ret_df <- predecir_bootstrap_prr(est$arima$fit, serie_ts = as.numeric(serie),
                                     h = h, levels = levels, B = B_prr)
    metodo <- sprintf("Bootstrap PRR (B=%d, sin supuesto normal)", B_prr)
    
  } else {
    ret_df <- predecir_arima(est$arima$fit, h = h, levels = levels)
    metodo <- "ARIMA gaussiano"
  }
  
  ret_df <- to_price_space(ret_df, last_price = last_price, levels = levels)
  
  ret_df$date   <- last_date + ret_df$horizonte
  ret_df$ticker <- ticker
  keep_front <- c("date", "ticker", "horizonte", "pred_return", "sigma")
  ret_df <- ret_df[, c(keep_front, setdiff(names(ret_df), keep_front))]
  attr(ret_df, "boot_paths") <- NULL   # no persistir la matriz en el CSV
  
  if (verbose) {
    lv <- max(levels)
    cat(sprintf("  Metodo: %s\n", metodo))
    cat(sprintf("  P_t = %.4f (%s)\n", last_price, last_date))
    cat(sprintf("  Precio h=%d: %.4f  [IC%d: %.4f, %.4f]\n", h,
                ret_df$pred_price[h], lv,
                ret_df[[sprintf("price_lower%d", lv)]][h],
                ret_df[[sprintf("price_upper%d", lv)]][h]))
  }
  
  if (save) {
    tag <- gsub("-", "", ticker)
    write.csv(ret_df, here::here("outputs", "tables",
                                 sprintf("forecast_%s.csv", tag)), row.names = FALSE)
    
    outdir <- here::here("outputs", "figures", "forecast")
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    lv <- max(levels)
    png(file.path(outdir, sprintf("%s_forecast.png", tag)),
        width = 1000, height = 600, res = 110)
    op <- par(mar = c(4, 4, 3, 1))
    yl <- range(ret_df[[sprintf("price_lower%d", lv)]],
                ret_df[[sprintf("price_upper%d", lv)]], last_price)
    plot(ret_df$date, ret_df$pred_price, type = "l", col = "#1f4e79", lwd = 2,
         ylim = yl, xlab = "Fecha", ylab = "Precio",
         main = sprintf("%s - Prediccion %d dias (%s)", ticker, h,
                        ifelse(est$modelo_principal == "arma_garch", "GARCH-t", "PRR/BJ")))
    polygon(c(ret_df$date, rev(ret_df$date)),
            c(ret_df[[sprintf("price_lower%d", lv)]],
              rev(ret_df[[sprintf("price_upper%d", lv)]])),
            col = adjustcolor("#1f4e79", 0.15), border = NA)
    points(last_date, last_price, pch = 19, col = "#c00000")
    par(op); dev.off()
  }
  
  list(ticker = ticker, df = ret_df, metodo = metodo,
       h = h, last_price = last_price, last_date = last_date)
}