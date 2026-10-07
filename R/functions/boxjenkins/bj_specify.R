# =============================================================
# bj_specify.R
# MODULO 1 del motor Box-Jenkins: ESPECIFICACION
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Etapa de identificacion completa: determina d, detecta
# estacionalidad, identifica la forma funcional ARIMA(p,d,q)
# comparando candidatos por BIC, AICc y RMSE de backtesting, y
# decide un modelo por consenso. No usa auto.arima: implementa
# su propia automatizacion de la metodologia.
# =============================================================

# -------------------------------------------------------------
# FASE 1A: Orden de integracion d
# -------------------------------------------------------------

#' Determina d por consenso de ndiffs (ADF, KPSS, PP)
#'
#' Aplica ndiffs() con los tres contrastes y toma la moda como
#' d de consenso. Para log-retornos el resultado esperado es d = 0.
#'
#' @param x     numeric. Serie.
#' @param alpha numeric. Nivel de significacion.
#' @return list con d (integer) y el detalle por contraste.
determinar_d <- function(x, alpha = 0.05) {
  
  x <- as.numeric(x); x <- x[is.finite(x)]
  
  nd_adf  <- tryCatch(forecast::ndiffs(x, test = "adf",  alpha = alpha), error = function(e) NA)
  nd_kpss <- tryCatch(forecast::ndiffs(x, test = "kpss", alpha = alpha), error = function(e) NA)
  nd_pp   <- tryCatch(forecast::ndiffs(x, test = "pp",   alpha = alpha), error = function(e) NA)
  
  votos <- c(nd_adf, nd_kpss, nd_pp)
  votos <- votos[!is.na(votos)]
  d <- if (length(votos) == 0) 0L else as.integer(names(sort(table(votos), decreasing = TRUE))[1])
  
  list(d = d, adf = nd_adf, kpss = nd_kpss, pp = nd_pp)
}


# -------------------------------------------------------------
# FASE 1B: Estacionalidad
# -------------------------------------------------------------

#' Detecta diferenciacion estacional con nsdiffs()
#'
#' Solo se evalua si period > 1. Para retornos diarios la
#' estacionalidad es habitualmente nula (period = 1 por defecto).
#'
#' @param x      numeric. Serie.
#' @param period integer. Periodo estacional (1 = sin estacionalidad).
#' @param alpha  numeric. Nivel de significacion.
#' @return list con seasonal (logical), D y period.
determinar_estacionalidad <- function(x, period = 1L, alpha = 0.05) {
  
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (period <= 1L) return(list(seasonal = FALSE, D = 0L, period = 1L))
  
  x_ts <- stats::ts(x, frequency = period)
  D <- tryCatch(forecast::nsdiffs(x_ts, alpha = alpha), error = function(e) 0L)
  list(seasonal = D > 0L, D = as.integer(D), period = as.integer(period))
}


# -------------------------------------------------------------
# FASE 1C: Sugerencia tentativa por ACF/PACF (informativa)
# -------------------------------------------------------------

#' Sugiere (p, q) por el ultimo lag significativo de PACF/ACF
#'
#' Heuristica clasica de Box-Jenkins: ultimo lag significativo del
#' PACF -> p; del ACF -> q. Es orientativa, no decisional.
#'
#' @param x       numeric. Serie estacionaria.
#' @param max_lag integer. Lags a inspeccionar.
#' @return list con p_sugerido y q_sugerido.
sugerir_pq_acf <- function(x, max_lag = 10L) {
  
  x <- as.numeric(x); x <- x[is.finite(x)]
  ci <- qnorm(0.975) / sqrt(length(x))
  
  acf_v  <- stats::acf(x,  lag.max = max_lag, plot = FALSE)$acf[-1]
  pacf_v <- stats::pacf(x, lag.max = max_lag, plot = FALSE)$acf
  
  sig_acf  <- which(abs(acf_v)  > ci)
  sig_pacf <- which(abs(pacf_v) > ci)
  
  list(q_sugerido = if (length(sig_acf)  > 0) max(sig_acf)  else 0L,
       p_sugerido = if (length(sig_pacf) > 0) max(sig_pacf) else 0L)
}


# -------------------------------------------------------------
# FASE 1D: Identificacion de la forma funcional
# -------------------------------------------------------------

#' AICc corregido para muestras finitas
#'
#' AICc = AIC + 2k(k+1)/(T-k-1), con k = nº parametros + 1 (sigma^2).
#'
#' @param fit objeto Arima.
#' @param T   integer. Tamano muestral.
#' @return numeric. AICc.
calc_aicc <- function(fit, T) {
  k <- length(fit$coef) + 1L
  fit$aic + 2 * k * (k + 1) / max(T - k - 1L, 1L)
}


#' Backtesting de un candidato ARIMA sobre la media condicional
#'
#' Estima en el 80% inicial y evalua sobre el 20% final. En modo
#' "multistep" predice todo el test de una vez (rapido). En modo
#' "rolling" predice recursivamente a h=1 reutilizando coeficientes
#' (mas discriminante, mas lento).
#'
#' @param y            numeric. Serie completa.
#' @param pdq          integer(3).
#' @param include_mean logical.
#' @param prop_train   numeric.
#' @param mode         character. "multistep" (default por menor coste computacional) o "rolling".
#' @return list con rmse y mae.
backtest_candidato <- function(y, pdq, include_mean = TRUE,
                               prop_train = 0.80,
                               mode = c("multistep", "rolling")) {
  
  mode    <- match.arg(mode)
  n_total <- length(y)
  n_train <- floor(n_total * prop_train)
  n_test  <- n_total - n_train
  if (n_test < 2L) return(list(rmse = Inf, mae = Inf))
  
  train <- y[seq_len(n_train)]
  test  <- y[(n_train + 1L):n_total]
  
  fit_tr <- tryCatch(
    forecast::Arima(train, order = pdq, include.mean = include_mean, method = "CSS-ML"),
    error = function(e) NULL)
  if (is.null(fit_tr)) return(list(rmse = Inf, mae = Inf))
  
  if (mode == "multistep") {
    pred <- tryCatch(forecast::forecast(fit_tr, h = n_test), error = function(e) NULL)
    if (is.null(pred)) return(list(rmse = Inf, mae = Inf))
    err <- as.numeric(pred$mean) - test
    
  } else {
    preds <- numeric(n_test)
    for (i in seq_len(n_test)) {
      current <- y[seq_len(n_train + i - 1L)]
      fit_i <- tryCatch(forecast::Arima(current, model = fit_tr), error = function(e) NULL)
      preds[i] <- if (is.null(fit_i)) NA_real_ else
        as.numeric(forecast::forecast(fit_i, h = 1L)$mean)
    }
    err <- preds - test
  }
  
  list(rmse = sqrt(mean(err^2, na.rm = TRUE)),
       mae  = mean(abs(err),   na.rm = TRUE))
}


#' Compara candidatos ARIMA por BIC, AICc y RMSE de backtesting
#'
#' Para cada orden (p,d,q): ajusta el modelo completo (BIC, AICc) y
#' lo evalua fuera de muestra (RMSE/MAE). Imprime una tabla con
#' marcadores del mejor en cada criterio y devuelve los resultados
#' ordenados por AICc.
#'
#' @param serie_ts     numeric/ts. Serie estacionaria.
#' @param candidatos   list de c(p,d,q).
#' @param include_mean logical.
#' @param prop_train   numeric.
#' @param bt_mode      character. "multistep" (default) o "rolling".
#' @return data.frame con una fila por candidato, ordenado por AICc.
identificar_forma_funcional <- function(serie_ts, candidatos,
                                        include_mean = TRUE,
                                        prop_train   = 0.80,
                                        bt_mode      = "multistep") {
  
  y       <- as.numeric(serie_ts); y <- y[is.finite(y)]
  T_total <- length(y)
  n_train <- floor(T_total * prop_train)
  n_test  <- T_total - n_train
  
  cat(sprintf("  Serie: T = %d  |  Train: %d (%.0f%%)  |  Test: %d  |  bt: %s\n\n",
              T_total, n_train, 100 * prop_train, n_test, bt_mode))
  
  res <- lapply(candidatos, function(pdq) {
    etq <- sprintf("ARIMA(%d,%d,%d)", pdq[1], pdq[2], pdq[3])
    
    fit <- tryCatch(
      forecast::Arima(y, order = pdq, include.mean = include_mean, method = "CSS-ML"),
      error = function(e) NULL)
    if (is.null(fit)) {
      return(data.frame(modelo = etq, p = pdq[1], d = pdq[2], q = pdq[3],
                        bic = Inf, aicc = Inf, rmse = Inf, mae = Inf, k = NA_integer_,
                        stringsAsFactors = FALSE))
    }
    
    bt <- backtest_candidato(y, pdq, include_mean = include_mean,
                             prop_train = prop_train, mode = bt_mode)
    
    data.frame(modelo = etq, p = pdq[1], d = pdq[2], q = pdq[3],
               bic = fit$bic, aicc = calc_aicc(fit, T_total),
               rmse = bt$rmse, mae = bt$mae, k = length(fit$coef) + 1L,
               stringsAsFactors = FALSE)
  })
  
  tab <- do.call(rbind, res)
  
  best_bic  <- which.min(tab$bic)
  best_aicc <- which.min(tab$aicc)
  best_rmse <- if (all(is.infinite(tab$rmse))) NA else which.min(tab$rmse)
  
  cat(sprintf("  %-18s %10s %10s %12s %12s %4s\n",
              "Modelo", "BIC", "AICc", "RMSE(test)", "MAE(test)", "k"))
  cat("  ", strrep("-", 72), "\n", sep = "")
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    marca <- paste0(
      if (i == best_bic)  " <BIC"  else "",
      if (i == best_aicc) " <AICc" else "",
      if (!is.na(best_rmse) && i == best_rmse) " <RMSE" else "")
    cat(sprintf("  %-18s %10.2f %10.2f %12.7f %12.7f %4d%s\n",
                r$modelo, r$bic, r$aicc, r$rmse, r$mae, r$k, marca))
  }
  
  tab <- tab[order(tab$aicc), ]
  rownames(tab) <- NULL
  tab
}


# -------------------------------------------------------------
# FASE 1E: Decision por consenso
# -------------------------------------------------------------

#' Decide un modelo a partir de la tabla de candidatos
#'
#' Regla de consenso: si un modelo es el mejor en >= 2 de los 3
#' criterios (BIC, AICc, RMSE), se elige. Si los tres discrepan,
#' se prioriza BIC (parsimonia) salvo que penalice el RMSE mas de
#' un 5% frente al mejor en RMSE, en cuyo caso se prioriza la
#' capacidad predictiva.
#'
#' @param tab data.frame de candidatos (output de identificar_forma_funcional).
#' @return list con decision (character), orden c(p,d,q) y motivo.
decidir_modelo <- function(tab) {
  
  best_bic  <- tab$modelo[which.min(tab$bic)]
  best_aicc <- tab$modelo[which.min(tab$aicc)]
  best_rmse <- if (all(is.infinite(tab$rmse))) NA_character_ else
    tab$modelo[which.min(tab$rmse)]
  
  cands <- c(best_bic, best_aicc, best_rmse)
  cands <- cands[!is.na(cands)]
  votos <- table(cands)
  ganador   <- names(votos)[which.max(votos)]
  max_votos <- max(votos)
  
  if (max_votos >= 2) {
    decision <- ganador
    motivo   <- sprintf("consenso en %d/3 criterios", max_votos)
  } else {
    rmse_bic  <- tab$rmse[tab$modelo == best_bic]
    rmse_best <- min(tab$rmse, na.rm = TRUE)
    if (is.finite(rmse_bic) && is.finite(rmse_best) &&
        (rmse_bic - rmse_best) / rmse_best > 0.05) {
      decision <- best_rmse
      motivo   <- "sin consenso; BIC penaliza RMSE > 5%, se prioriza prediccion"
    } else {
      decision <- best_bic
      motivo   <- "sin consenso; se prioriza parsimonia (BIC)"
    }
  }
  
  fila  <- tab[tab$modelo == decision, ][1, ]
  orden <- as.integer(c(fila$p, fila$d, fila$q))
  
  list(decision = decision, orden = orden, motivo = motivo,
       best_bic = best_bic, best_aicc = best_aicc, best_rmse = best_rmse)
}


# -------------------------------------------------------------
# ORQUESTADOR
# -------------------------------------------------------------

#' Ejecuta la especificacion Box-Jenkins completa
#'
#' Encadena: determinacion de d, estacionalidad, sugerencia ACF/PACF,
#' generacion del grid de candidatos, identificacion por BIC/AICc/RMSE
#' y decision por consenso.
#'
#' @param serie        numeric/xts/ts. Serie de log-retornos.
#' @param ticker       character. Etiqueta del activo.
#' @param max_p,max_q  integer. Ordenes maximos del grid.
#' @param d            integer o NULL. Si NULL se determina por consenso.
#' @param period       integer. Periodo estacional (1 = ninguno).
#' @param prop_train   numeric. Fraccion de entrenamiento del backtesting.
#' @param bt_mode      character. "multistep" (default) o "rolling".
#' @param verbose      logical.
#' @return list con d, estacionalidad, sugerencia, candidatos y decision.
#'
#' @examples
#' returns <- readRDS(here::here("data","processed","log_returns.rds"))
#' spec <- bj_specify(returns[["GLD"]], ticker = "GLD")
#' spec$decision   # modelo elegido por consenso
bj_specify <- function(serie, ticker = "serie",
                       max_p = 3L, max_q = 3L, d = NULL,
                       period = 1L, prop_train = 0.80,
                       bt_mode = "multistep", verbose = TRUE) {
  
  y <- as.numeric(serie); y <- y[is.finite(y)]
  stopifnot(length(y) >= 50L)
  
  if (verbose) cat(sprintf("\n[ESPECIFICACION] %s  (n = %d)\n", ticker, length(y)))
  
  # d
  if (is.null(d)) {
    dd <- determinar_d(y); d_use <- dd$d
  } else {
    dd <- list(d = as.integer(d)); d_use <- as.integer(d)
  }
  if (verbose) cat(sprintf("  d (consenso ndiffs) = %d\n", d_use))
  
  y_diff <- if (d_use > 0) diff(y, differences = d_use) else y
  
  # estacionalidad
  seas <- determinar_estacionalidad(y, period = period)
  if (verbose) cat(sprintf("  Estacionalidad: %s\n", ifelse(seas$seasonal, "SI", "NO")))
  
  # sugerencia ACF/PACF
  sugg <- sugerir_pq_acf(y_diff, max_lag = max(max_p, max_q))
  if (verbose) cat(sprintf("  Sugerencia ACF/PACF: p~%d, q~%d\n\n",
                           sugg$p_sugerido, sugg$q_sugerido))
  
  # grid de candidatos
  grid <- expand.grid(p = 0:max_p, q = 0:max_q)
  candidatos <- lapply(seq_len(nrow(grid)), function(i)
    as.integer(c(grid$p[i], d_use, grid$q[i])))
  
  # identificacion por BIC / AICc / RMSE
  tab <- identificar_forma_funcional(y, candidatos,
                                     include_mean = (d_use == 0L),
                                     prop_train   = prop_train,
                                     bt_mode      = bt_mode)
  
  # decision por consenso
  dec <- decidir_modelo(tab)
  if (verbose) {
    cat(sprintf("\n  Mejor BIC : %-16s | Mejor AICc: %-16s | Mejor RMSE: %s\n",
                dec$best_bic, dec$best_aicc, ifelse(is.na(dec$best_rmse), "n/d", dec$best_rmse)))
    cat(sprintf("  >> DECISION: %s  (%s)\n", dec$decision, dec$motivo))
  }
  
  list(ticker       = ticker,
       d            = d_use,
       d_detalle    = dd,
       estacionalidad = seas,
       sugerencia   = sugg,
       candidatos   = tab,
       decision     = dec$decision,
       orden        = dec$orden,
       motivo       = dec$motivo,
       n_obs        = length(y))
}