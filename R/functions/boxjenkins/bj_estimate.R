# =============================================================
# bj_estimate.R
# MODULO 2 del motor Box-Jenkins: ESTIMACION
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Estima el modelo de la media (ARMA) elegido en bj_specify y,
# si los residuos presentan efectos ARCH, ajusta un modelo
# conjunto ARMA-GARCH(1,1) por maxima verosimilitud con
# innovaciones t-Student. Estimacion conjunta para calibrar 
# correctamente los errores estandar de la media en presencia 
# de heterocedasticidad.
# =============================================================

# -------------------------------------------------------------
# 2a. ESTIMACION ARIMA (media)
# -------------------------------------------------------------

#' Estima un modelo ARIMA de la media por maxima verosimilitud
#'
#' Ajuste con forecast::Arima y method = "CSS-ML". Devuelve el objeto
#' y una tabla de coeficientes con error estandar, estadistico t y
#' p-valor (test de Wald, dos colas).
#'
#' @param y            numeric. Serie de log-retornos.
#' @param orden        integer(3). c(p, d, q) del Modulo 1.
#' @param include_mean logical. TRUE si d = 0.
#' @return list con fit (objeto Arima) y coef_table (data.frame).
estimar_arima <- function(y, orden, include_mean = NULL) {
  
  y <- as.numeric(y); y <- y[is.finite(y)]
  if (is.null(include_mean)) include_mean <- (orden[2] == 0L)
  
  fit <- forecast::Arima(y, order = orden, include.mean = include_mean,
                         method = "ML")
  
  # Tabla de coeficientes: estimacion, SE, t, p-valor (Wald)
  est <- fit$coef
  se  <- sqrt(diag(fit$var.coef))
  tval <- est / se
  pval <- 2 * (1 - pnorm(abs(tval)))
  
  coef_table <- data.frame(
    parametro  = names(est),
    estimacion = as.numeric(est),
    std_error  = as.numeric(se),
    t_value    = as.numeric(tval),
    p_value    = as.numeric(pval),
    signif     = ifelse(pval < 0.01, "***",
                        ifelse(pval < 0.05, "**",
                               ifelse(pval < 0.10, "*", ""))),
    stringsAsFactors = FALSE
  )
  
  list(fit = fit, coef_table = coef_table)
}


# -------------------------------------------------------------
# 2b. CONTRASTE ARCH SOBRE RESIDUOS DEL ARIMA
# -------------------------------------------------------------

#' Contrasta efectos ARCH en los residuos de un ARIMA
#'
#' Test LM de Engle sobre los residuos. Si es significativo, la
#' varianza condicional esta estructurada y procede la rama GARCH.
#'
#' @param fit      objeto Arima.
#' @param arch_lag integer. Lag del test.
#' @param alpha    numeric.
#' @return list con statistic, p_value y garch_required (logical).
test_arch_residuos <- function(fit, arch_lag = 12L, alpha = 0.05) {
  res  <- as.numeric(residuals(fit))
  res  <- res[is.finite(res)]
  arch <- FinTS::ArchTest(res, lags = arch_lag)
  list(statistic      = as.numeric(arch$statistic),
       p_value        = as.numeric(arch$p.value),
       garch_required = arch$p.value < alpha)
}


# -------------------------------------------------------------
# 2c. ESTIMACION CONJUNTA ARMA-GARCH(1,1)
# -------------------------------------------------------------

#' Estima un modelo conjunto ARMA(p,q)-GARCH(1,1) con innovaciones t
#'
#' Especifica y estima en una sola etapa con rugarch. La media sigue
#' el orden ARMA del Modulo 1 (el componente d ya se aplico: la serie
#' es estacionaria, se modela en niveles de retorno). La varianza es
#' GARCH(1,1) y las innovaciones t-Student, para capturar las colas
#' gruesas documentadas en el EDA.
#'
#' @param y     numeric. Serie de log-retornos.
#' @param orden integer(3). c(p, d, q); se usan p y q (d debe ser 0).
#' @return list con fit (uGARCHfit), coef_table y spec, o NULL si falla.
estimar_arma_garch <- function(y, orden) {
  
  y <- as.numeric(y); y <- y[is.finite(y)]
  p <- orden[1]; q <- orden[3]
  
  spec <- rugarch::ugarchspec(
    mean.model     = list(armaOrder = c(p, q), include.mean = TRUE),
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    distribution.model = "std"   # t-Student
  )
  
  fit <- tryCatch(
    rugarch::ugarchfit(spec, data = y, solver = "hybrid"),
    error = function(e) NULL)
  
  if (is.null(fit) || fit@fit$convergence != 0) return(NULL)
  
  # Tabla de coeficientes (rugarch ya da SE robustos y p-valores)
  mc <- fit@fit$matcoef
  coef_table <- data.frame(
    parametro  = rownames(mc),
    estimacion = mc[, 1],
    std_error  = mc[, 2],
    t_value    = mc[, 3],
    p_value    = mc[, 4],
    signif     = ifelse(mc[, 4] < 0.01, "***",
                        ifelse(mc[, 4] < 0.05, "**",
                               ifelse(mc[, 4] < 0.10, "*", ""))),
    stringsAsFactors = FALSE
  )
  rownames(coef_table) <- NULL
  
  list(fit = fit, coef_table = coef_table, spec = spec)
}


# -------------------------------------------------------------
# ORQUESTADOR DEL MODULO 2
# -------------------------------------------------------------

#' Ejecuta la etapa de estimacion Box-Jenkins
#'
#' Estima el ARIMA de la media, contrasta ARCH en sus residuos y, si
#' procede, estima el modelo conjunto ARMA-GARCH(1,1)-t. Devuelve
#' ambos ajustes (ARIMA puro como baseline y ARMA-GARCH como modelo
#' principal cuando hay heterocedasticidad), con metricas de ajuste.
#'
#' @param serie    numeric/xts. Serie de log-retornos.
#' @param orden    integer(3). c(p,d,q) del Modulo 1 (spec$orden).
#' @param ticker   character.
#' @param arch_lag integer. Lag del test ARCH.
#' @param alpha    numeric.
#' @param verbose  logical.
#' @return list con arima, arch, garch, modelo_principal y metadatos.
#'
#' @examples
#' spec <- bj_specify(returns[["GLD"]], ticker = "GLD")
#' est  <- bj_estimate(returns[["GLD"]], spec$orden, ticker = "GLD")
bj_estimate <- function(serie, orden, ticker = "serie",
                        arch_lag = 12L, alpha = 0.05, verbose = TRUE) {
  
  y <- as.numeric(serie); y <- y[is.finite(y)]
  stopifnot(length(orden) == 3L)
  
  if (verbose) cat(sprintf("\n[ESTIMACION] %s  |  ARIMA(%d,%d,%d)\n",
                           ticker, orden[1], orden[2], orden[3]))
  
  # --- Media: ARIMA ---
  arima_res <- estimar_arima(y, orden)
  if (verbose) {
    cat("  Coeficientes ARIMA (media):\n")
    print(arima_res$coef_table, row.names = FALSE, digits = 5)
    cat(sprintf("  AIC = %.2f | BIC = %.2f | loglik = %.2f\n",
                arima_res$fit$aic, arima_res$fit$bic,
                as.numeric(arima_res$fit$loglik)))
  }
  
  # --- Contraste ARCH ---
  arch_res <- test_arch_residuos(arima_res$fit, arch_lag = arch_lag, alpha = alpha)
  if (verbose) cat(sprintf("\n  Test ARCH (lag %d): stat = %.2f, p = %.4g -> GARCH %s\n",
                           arch_lag, arch_res$statistic, arch_res$p_value,
                           ifelse(arch_res$garch_required, "REQUERIDO", "no necesario")))
  
  # --- Varianza: GARCH (si procede) ---
  garch_res <- NULL
  modelo_principal <- "arima"
  if (arch_res$garch_required) {
    garch_res <- estimar_arma_garch(y, orden)
    if (is.null(garch_res)) {
      if (verbose) cat("  [AVISO] El GARCH no convergio; se mantiene ARIMA puro.\n")
    } else {
      modelo_principal <- "arma_garch"
      if (verbose) {
        cat("\n  Coeficientes ARMA-GARCH(1,1)-t:\n")
        print(garch_res$coef_table, row.names = FALSE, digits = 5)
        ic <- rugarch::infocriteria(garch_res$fit)
        cat(sprintf("  AIC = %.4f | BIC = %.4f  (por observacion, escala rugarch)\n",
                    ic[1], ic[2]))
      }
    }
  }
  
  if (verbose) cat(sprintf("\n  >> Modelo principal: %s\n",
                           ifelse(modelo_principal == "arma_garch",
                                  "ARMA-GARCH(1,1)-t", "ARIMA")))
  
  list(ticker           = ticker,
       orden            = orden,
       arima            = arima_res,
       arch             = arch_res,
       garch            = garch_res,
       modelo_principal = modelo_principal,
       n_obs            = length(y))
}