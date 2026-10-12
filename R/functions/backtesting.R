# =============================================================
# backtesting.R
# FASE 5 - Backtesting out-of-sample one-step (parametros fijos)
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Validacion del modelo final mediante prediccion a un paso fuera
# de muestra con PARAMETROS FIJOS:
#   - Los parametros se estiman UNA sola vez sobre la ventana
#     inicial (prop_train del inicio de la serie).
#   - Se predice un paso cada vez a traves del tramo de test,
#     filtrando con los datos reales (el conjunto de informacion
#     se expande), SIN reestimar los coeficientes.
#   - Para GARCH se usa out.sample + n.roll de rugarch, que hace
#     exactamente esto de forma nativa.
#
# Se evalua en dos escalas:
#   - Retornos (1 paso): cobertura de intervalos, Kupiec, hit
#     ratio de signo, RMSE/MAE.
#   - Precio (1 paso): P_hat_t = P_{t-1} * exp(mu_t); RMSE y MAPE.
#
# La cobertura empirica de los intervalos es la metrica central:
# valida que un IC nominal del 95% contiene ~95% de los retornos
# realizados (modelo bien calibrado).
# =============================================================

#' Test de Kupiec (proportion of failures) de cobertura incondicional
#'
#' Contrasta si la tasa de fallo observada (puntos fuera del intervalo)
#' coincide con la nominal esperada p = 1 - nivel.
#'
#' @param n        integer. Observaciones de test.
#' @param failures integer. Nº de puntos fuera del intervalo.
#' @param p        numeric. Tasa de fallo nominal esperada.
#' @return list con estadistico LR y p-valor (chi-cuadrado, 1 gl).
kupiec_pof <- function(n, failures, p) {
  if (n <= 0) return(list(LR = NA_real_, p_value = NA_real_))
  x  <- failures
  pi <- x / n
  ll0 <- x * log(p) + (n - x) * log(1 - p)
  ll1 <- if (x == 0) n * log(1 - pi + 1e-12)
  else if (x == n) n * log(pi)
  else x * log(pi) + (n - x) * log(1 - pi)
  LR <- -2 * (ll0 - ll1)
  list(LR = LR, p_value = as.numeric(1 - pchisq(LR, df = 1)))
}


#' Backtesting one-step out-of-sample con parametros fijos
#'
#' @param serie_ret        xts. Serie de log-retornos del activo.
#' @param px               numeric. Serie de precios limpios (1 obs mas que retornos).
#' @param orden            integer(3). c(p,d,q) del modelo.
#' @param modelo_principal character. "arma_garch" o "arima".
#' @param dist             character. Distribucion GARCH ("std").
#' @param prop_train       numeric. Fraccion de estimacion inicial.
#' @param levels           numeric. Niveles de confianza.
#' @param ticker           character.
#' @param save             logical. Guardar CSV punto a punto.
#' @param verbose          logical.
#' @return list con serie punto a punto (df) y resumen (summary).
backtest_oos <- function(serie_ret, px, orden,
                         modelo_principal = "arma_garch",
                         dist = "std", prop_train = 0.80,
                         levels = c(80, 95), ticker = "serie",
                         save = TRUE, verbose = TRUE) {
  
  y       <- as.numeric(serie_ret);
  dates   <- as.Date(zoo::index(serie_ret))
  n       <- length(y)
  n_train <- floor(n * prop_train)
  n_test  <- n - n_train
  p <- orden[1]; q <- orden[3]
  oos <- (n_train + 1L):n
  
  if (n_test < 30L) stop("Muestra de test demasiado pequena para cobertura.")
  
  # --- Prediccion one-step OOS, parametros fijos ---
  if (modelo_principal == "arma_garch") {
    spec <- rugarch::ugarchspec(
      mean.model     = list(armaOrder = c(p, q), include.mean = TRUE),
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      distribution.model = dist)
    fit <- rugarch::ugarchfit(spec, data = y, out.sample = n_test, solver = "hybrid")
    fcr <- rugarch::ugarchforecast(fit, n.ahead = 1, n.roll = n_test - 1)
    mu    <- as.numeric(rugarch::fitted(fcr))
    sigma <- as.numeric(rugarch::sigma(fcr))
    nu    <- as.numeric(rugarch::coef(fit)["shape"])
    qfun  <- function(a) rugarch::qdist("std", a, 0, 1, shape = nu)
    metodo <- sprintf("ARMA-GARCH(1,1)-t one-step OOS (nu=%.2f)", nu)
    
  } else {
    # ARIMA: estima en train; filtra la serie completa con coef fijos
    fit_tr   <- forecast::Arima(y[seq_len(n_train)], order = orden,
                                include.mean = TRUE, method = "ML")
    fit_full <- forecast::Arima(y, model = fit_tr)
    mu    <- as.numeric(fitted(fit_full))[oos]
    sigma <- rep(sqrt(fit_tr$sigma2), n_test)
    qfun  <- function(a) qnorm(a)
    metodo <- "ARIMA one-step OOS (gaussiano)"
  }
  
  actual <- y[oos]
  d_oos  <- dates[oos]
  
  # --- Intervalos de retorno y cobertura ---
  df <- data.frame(date = d_oos, actual_return = actual,
                   pred_return = mu, sigma = sigma, stringsAsFactors = FALSE)
  cobertura <- list()
  for (lv in levels) {
    a  <- (1 - lv / 100) / 2
    lo <- mu + qfun(a) * sigma
    hi <- mu - qfun(a) * sigma
    inside <- actual >= lo & actual <= hi
    df[[sprintf("lower%d", lv)]]  <- lo
    df[[sprintf("upper%d", lv)]]  <- hi
    df[[sprintf("inside%d", lv)]] <- inside
    cobertura[[as.character(lv)]] <- mean(inside)
  }
  df$z <- (actual - mu) / sigma
  
  # --- Espacio de precios (one-step) ---
  # Retorno r_k usa px[k+1]/px[k]; para oos idx k, P_prev = px[k], P_curr = px[k+1]
  P_prev <- px[oos]          # precio en t-1 (alineado: px tiene 1 obs mas al inicio)
  P_curr <- px[oos + 1L]     # precio realizado en t
  df$actual_price <- P_curr
  df$pred_price   <- P_prev * exp(mu)
  for (lv in levels) {
    a <- (1 - lv / 100) / 2
    df[[sprintf("price_lower%d", lv)]] <- P_prev * exp(mu + qfun(a) * sigma)
    df[[sprintf("price_upper%d", lv)]] <- P_prev * exp(mu - qfun(a) * sigma)
  }
  df$ticker <- ticker
  
  # --- Metricas resumen ---
  err_r  <- mu - actual
  err_p  <- df$pred_price - df$actual_price
  hit    <- mean(sign(mu) == sign(actual))
  kup <- lapply(levels, function(lv) {
    fails <- sum(!df[[sprintf("inside%d", lv)]])
    kupiec_pof(n_test, fails, p = 1 - lv / 100)
  })
  names(kup) <- as.character(levels)
  
  resumen <- data.frame(
    ticker        = ticker,
    metodo        = metodo,
    n_test        = n_test,
    rmse_ret      = sqrt(mean(err_r^2)),
    mae_ret       = mean(abs(err_r)),
    rmse_price    = sqrt(mean(err_p^2)),
    mape_price    = mean(abs(err_p / df$actual_price)) * 100,
    hit_ratio     = hit,
    cov80         = cobertura[["80"]],
    cov95         = cobertura[["95"]],
    kupiec80_p    = kup[["80"]]$p_value,
    kupiec95_p    = kup[["95"]]$p_value,
    stringsAsFactors = FALSE)
  
  if (verbose) {
    cat(sprintf("\n  %-10s [%s]\n", ticker, metodo))
    cat(sprintf("    Cobertura IC80: %.1f%% (nominal 80) | Kupiec p=%.3f\n",
                100 * resumen$cov80, resumen$kupiec80_p))
    cat(sprintf("    Cobertura IC95: %.1f%% (nominal 95) | Kupiec p=%.3f\n",
                100 * resumen$cov95, resumen$kupiec95_p))
    cat(sprintf("    Hit ratio signo: %.1f%% | RMSE_ret=%.5f | MAPE_precio=%.2f%%\n",
                100 * resumen$hit_ratio, resumen$rmse_ret, resumen$mape_price))
  }
  
  if (save) {
    tag <- gsub("-", "", ticker)
    write.csv(df, here::here("outputs", "tables",
                             sprintf("backtest_oos_%s.csv", tag)), row.names = FALSE)
  }
  
  invisible(list(df = df, summary = resumen, kupiec = kup))
}