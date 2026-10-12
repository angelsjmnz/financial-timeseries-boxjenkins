# =============================================================
# bj_diagnose.R
# MODULO 3 del motor Box-Jenkins: DIAGNOSIS
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Dos rutas de diagnosis segun el modelo principal:
#   - ARIMA puro  -> diagnosis_residuos(): bateria completa sobre
#     residuos ordinarios, incl. normalidad (CvM/AD), media nula,
#     ARCH y bootstrap de significatividad (Pascual-Romo-Ruiz).
#   - ARMA-GARCH  -> diagnosis_garch(): bateria sobre residuos
#     ESTANDARIZADOS z_t = e_t / sigma_t (LB, LB^2, ARCH).
# El rechazo de normalidad activa el flag de intervalos bootstrap
# PRR para la Fase 4 (prediccion sin supuesto gaussiano).
#
# Dependencias opcionales: goftest (cvm.test, ad.test), lmtest
# (coeftest). Si faltan, la diagnosis degrada con tryCatch.
# =============================================================

# -------------------------------------------------------------
# 3.A  BOOTSTRAP DE SIGNIFICATIVIDAD (Pascual, Romo & Ruiz, 2004)
# -------------------------------------------------------------

#' IC bootstrap de los coeficientes ARIMA por remuestreo de residuos
#'
#' Algoritmo de 5 pasos: (1) muestra bootstrap de residuos recentrados
#' y reescalados por el factor de Stine; (2) reconstruye y*_t con los
#' parametros FIJOS (fitted + e*); (3) reestima el modelo en y*_t;
#' (4) repite B veces; (5) IC por cuantiles. Un parametro es
#' significativo si 0 no cae en [IC_inf, IC_sup]. Valido aunque las
#' innovaciones no sean normales.
#'
#' @param fit objeto Arima.
#' @param B   integer. Replicas bootstrap.
#' @return data.frame con estimacion, IC95, media_boot, sesgo y signif.
bootstrap_significancia <- function(fit, B = 499L) {
  
  res     <- as.numeric(residuals(fit))
  res     <- res[is.finite(res)]
  Tn      <- length(res)
  nombres <- names(coef(fit))
  k       <- length(nombres)
  if (k == 0) return(NULL)
  
  ord <- forecast::arimaorder(fit)
  p_f <- ord[1]; d_f <- ord[2]; q_f <- ord[3]
  tiene_media <- "intercept" %in% nombres
  fitted_vals <- as.numeric(fitted(fit))
  frec        <- frequency(fitted(fit))
  
  # Residuos recentrados y reescalados (factor de Stine, 1987)
  res_c  <- res - mean(res)
  factor <- sqrt(Tn / max(Tn - k, 1))
  F_hat  <- res_c * factor
  
  mat <- matrix(NA_real_, nrow = B, ncol = k)
  colnames(mat) <- nombres
  
  for (b in seq_len(B)) {
    e_star <- sample(F_hat, size = length(fitted_vals), replace = TRUE)
    y_star <- stats::ts(fitted_vals + e_star, frequency = frec)
    fit_b  <- tryCatch(
      forecast::Arima(y_star, order = c(p_f, d_f, q_f),
                      include.mean = tiene_media, method = "ML"),
      error = function(e) NULL)
    if (!is.null(fit_b) && length(coef(fit_b)) == k) mat[b, ] <- coef(fit_b)
  }
  
  coef_orig <- coef(fit)
  out <- data.frame()
  for (j in seq_along(nombres)) {
    v <- mat[, j]; v <- v[!is.na(v)]
    ic_inf <- as.numeric(quantile(v, 0.025))
    ic_sup <- as.numeric(quantile(v, 0.975))
    media_b <- mean(v)
    out <- rbind(out, data.frame(
      parametro  = nombres[j],
      estimacion = as.numeric(coef_orig[j]),
      ic_inf_95  = round(ic_inf, 6),
      ic_sup_95  = round(ic_sup, 6),
      media_boot = round(media_b, 6),
      sesgo      = round(media_b - coef_orig[j], 6),
      signif     = ifelse(ic_inf > 0 | ic_sup < 0, "SI", "no"),
      stringsAsFactors = FALSE))
  }
  rownames(out) <- NULL
  out
}


# -------------------------------------------------------------
# 3.B  DIAGNOSIS DE RESIDUOS ARIMA
# -------------------------------------------------------------

#' Diagnosis completa de residuos de un modelo ARIMA
#'
#' Ljung-Box (incorrelacion en media, fitdf = nº parametros), tests de
#' normalidad compuesta (CvM y AD contra pnorm con parametros estimados;
#' degradan a Jarque-Bera si goftest no esta), media nula (t-test), ARCH-LM
#' y, opcionalmente, bootstrap de significatividad. Devuelve un flag
#' usar_bootstrap_prr para la Fase 4.
#'
#' @param fit       objeto Arima.
#' @param ticker    character.
#' @param alpha     numeric.
#' @param boot_sig  logical. Ejecutar bootstrap de significatividad.
#' @param B_sig     integer. Replicas bootstrap.
#' @param save_plot logical.
#' @param verbose   logical.
#' @return list con tests, flags y tabla bootstrap.
diagnosis_residuos <- function(fit, ticker = "serie", alpha = 0.05,
                               boot_sig = TRUE, B_sig = 499L,
                               save_plot = TRUE, verbose = TRUE) {
  
  res     <- as.numeric(residuals(fit))
  res     <- res[is.finite(res)]
  Tn      <- length(res)
  lag_lb  <- max(10L, round(sqrt(Tn)))
  n_param <- sum(coef(fit) != 0)
  
  # --- Ljung-Box (media) ---
  lb <- Box.test(res, lag = lag_lb, type = "Ljung-Box",
                 fitdf = min(n_param, lag_lb - 1L))
  lb_ok <- lb$p.value >= alpha
  
  # --- Normalidad compuesta (CvM / AD), degrada a Jarque-Bera ---
  mu <- mean(res); sdr <- sd(res)
  cvm <- tryCatch(goftest::cvm.test(res, null = "pnorm", mean = mu, sd = sdr),
                  error = function(e) NULL)
  ad  <- tryCatch(goftest::ad.test(res, null = "pnorm", mean = mu, sd = sdr),
                  error = function(e) NULL)
  jb  <- tseries::jarque.bera.test(res)
  
  if (!is.null(cvm) && !is.null(ad)) {
    normalidad_ok <- cvm$p.value >= alpha && ad$p.value >= alpha
    norm_src <- "CvM + AD"
  } else {
    normalidad_ok <- jb$p.value >= alpha   # fallback
    norm_src <- "Jarque-Bera (fallback; instalar goftest)"
  }
  
  # --- Media nula ---
  tm <- t.test(res, mu = 0)
  media_ok <- tm$p.value >= alpha
  
  # --- ARCH-LM ---
  arch <- FinTS::ArchTest(res, lags = 5)
  arch_ok <- arch$p.value >= alpha
  
  sk <- as.numeric(PerformanceAnalytics::skewness(res))
  ku <- as.numeric(PerformanceAnalytics::kurtosis(res, method = "moment"))
  
  if (verbose) {
    cat(sprintf("\n  Residuos ARIMA (n = %d, fitdf = %d)\n", Tn, n_param))
    cat(sprintf("  Ljung-Box Q(%d)   : p = %.6f  -> %s\n",
                lag_lb, lb$p.value, ifelse(lb_ok, "OK media", "FALLA media")))
    if (!is.null(cvm))
      cat(sprintf("  Cramer-von Mises  : p = %.6f  -> %s\n",
                  cvm$p.value, ifelse(cvm$p.value >= alpha, "normal", "no normal")))
    if (!is.null(ad))
      cat(sprintf("  Anderson-Darling  : p = %.6f  -> %s\n",
                  ad$p.value, ifelse(ad$p.value >= alpha, "normal", "no normal")))
    cat(sprintf("  Jarque-Bera       : p = %.6f  (asimetria %.3f, curtosis %.3f)\n",
                jb$p.value, sk, ku))
    cat(sprintf("  Media nula (t)    : p = %.6f  -> %s\n",
                tm$p.value, ifelse(media_ok, "media ~ 0", "media != 0")))
    cat(sprintf("  ARCH-LM(5)        : p = %.6f  -> %s\n",
                arch$p.value, ifelse(arch_ok, "sin ARCH", "ARCH -> GARCH")))
  }
  
  # --- Flag para Fase 4 ---
  usar_bootstrap_prr <- !normalidad_ok
  if (verbose) {
    if (usar_bootstrap_prr) {
      cat("\n  [!] Normalidad rechazada -> intervalos gaussianos no validos.\n")
      cat("      Fase 4: usar bootstrap PRR (Pascual, Romo & Ruiz, 2004).\n")
    } else {
      cat("\n  [ok] Normalidad no rechazada -> intervalos gaussianos validos.\n")
    }
  }
  
  # --- Bootstrap de significatividad ---
  boot_tab <- NULL
  if (boot_sig) {
    if (verbose) cat(sprintf("\n  Bootstrap de significatividad (B = %d)...\n", B_sig))
    boot_tab <- bootstrap_significancia(fit, B = B_sig)
    if (verbose && !is.null(boot_tab)) {
      cat("  IC bootstrap 95% de los coeficientes (0 fuera del IC -> signif.):\n")
      print(boot_tab, row.names = FALSE)
    }
  }
  
  if (save_plot) {
    outdir <- here::here("outputs", "figures", "modeling")
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    tag <- gsub("-", "", ticker)
    png(file.path(outdir, sprintf("%s_diagnosis_arima.png", tag)),
        width = 1000, height = 800, res = 110)
    op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    plot(res, type = "l", col = "#1f4e79", main = paste("Residuos -", ticker),
         xlab = "t", ylab = "e"); abline(h = 0, col = "grey60", lty = 2)
    qqnorm(res, main = "Q-Q Normal", col = "#1f4e79"); qqline(res, col = "#c00000", lwd = 2)
    acf(res,   lag.max = lag_lb + 5, main = "FAC residuos")
    acf(res^2, lag.max = lag_lb + 5, main = "FAC residuos^2")
    par(op); dev.off()
  }
  
  # Veredicto: ruido blanco en media + sin ARCH (normalidad es informativa)
  pass <- lb_ok && arch_ok
  
  list(ruta = "arima", pass = pass,
       lb_ok = lb_ok, arch_ok = arch_ok, media_ok = media_ok,
       normalidad_ok = normalidad_ok, norm_src = norm_src,
       usar_bootstrap_prr = usar_bootstrap_prr,
       p_values = list(lb = lb$p.value, arch = arch$p.value,
                       media = tm$p.value, jb = jb$p.value),
       bootstrap = boot_tab, skewness = sk, kurtosis = ku)
}


# -------------------------------------------------------------
# 3.C  DIAGNOSIS DE RESIDUOS GARCH (estandarizados)
# -------------------------------------------------------------

#' Diagnosis de un modelo ARMA-GARCH sobre residuos estandarizados
#'
#' Sobre z_t = e_t / sigma_t: Ljung-Box en media, Ljung-Box en z^2 y
#' ARCH-LM. El control clave es que el ARCH sobre z_t YA NO sea
#' significativo: el GARCH ha capturado la heterocedasticidad.
#'
#' @param garch_fit uGARCHfit.
#' @param ticker    character.
#' @param alpha     numeric.
#' @param save_plot logical.
#' @param verbose   logical.
#' @return list con tests y veredicto.
diagnosis_garch <- function(garch_fit, ticker = "serie", alpha = 0.05,
                            save_plot = TRUE, verbose = TRUE) {
  
  z   <- as.numeric(rugarch::residuals(garch_fit, standardize = TRUE))
  z   <- z[is.finite(z)]
  Tn  <- length(z)
  lag_lb <- max(10L, round(sqrt(Tn)))
  
  lb_z  <- Box.test(z,   lag = lag_lb, type = "Ljung-Box")
  lb_z2 <- Box.test(z^2, lag = lag_lb, type = "Ljung-Box")
  arch  <- FinTS::ArchTest(z, lags = 5)
  
  lb_ok   <- lb_z$p.value  >= alpha
  lb2_ok  <- lb_z2$p.value >= alpha
  arch_ok <- arch$p.value  >= alpha
  
  if (verbose) {
    cat(sprintf("\n  Residuos estandarizados z_t (n = %d)\n", Tn))
    cat(sprintf("  Ljung-Box z   Q(%d) : p = %.6f -> %s\n",
                lag_lb, lb_z$p.value,  ifelse(lb_ok,  "OK media", "estructura en media")))
    cat(sprintf("  Ljung-Box z^2 Q(%d) : p = %.6f -> %s\n",
                lag_lb, lb_z2$p.value, ifelse(lb2_ok, "OK varianza", "ARCH residual")))
    cat(sprintf("  ARCH-LM(5)         : p = %.6f -> %s\n",
                arch$p.value, ifelse(arch_ok, "LIMPIO (GARCH capturo volatilidad)",
                                     "AUN hay ARCH")))
  }
  
  if (save_plot) {
    outdir <- here::here("outputs", "figures", "modeling")
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    tag <- gsub("-", "", ticker)
    png(file.path(outdir, sprintf("%s_diagnosis_garch.png", tag)),
        width = 1000, height = 800, res = 110)
    op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    plot(z, type = "l", col = "#1f4e79", main = paste("z_t -", ticker),
         xlab = "t", ylab = "z"); abline(h = c(-2, 2), lty = 2, col = "#c00000")
    qqnorm(z, main = "Q-Q z_t", col = "#1f4e79"); qqline(z, col = "#c00000", lwd = 2)
    acf(z,   lag.max = lag_lb, main = "FAC z_t")
    acf(z^2, lag.max = lag_lb, main = "FAC z_t^2")
    par(op); dev.off()
  }
  
  pass <- lb_ok && lb2_ok && arch_ok
  
  list(ruta = "garch", pass = pass,
       lb_ok = lb_ok, lb2_ok = lb2_ok, arch_ok = arch_ok,
       p_values = list(lb_z = lb_z$p.value, lb_z2 = lb_z2$p.value,
                       arch = arch$p.value))
}


# -------------------------------------------------------------
# ORQUESTADOR DEL MODULO 3
# -------------------------------------------------------------

#' Ejecuta la diagnosis del modelo estimado (dispatch ARIMA / GARCH)
#'
#' Elige la ruta segun est$modelo_principal y emite un veredicto global.
#' Para ARIMA puro devuelve ademas usar_bootstrap_prr, que la Fase 4
#' usara para decidir el metodo de intervalos de prediccion.
#'
#' @param est       list. Salida de bj_estimate().
#' @param alpha     numeric.
#' @param boot_sig  logical. Bootstrap de significatividad (ruta ARIMA).
#' @param B_sig     integer. Replicas bootstrap.
#' @param save_plot logical.
#' @param verbose   logical.
#' @return list con pass, ruta y el detalle de la ruta aplicada.
#'
#' @examples
#' est <- bj_estimate(returns[["GLD"]], spec$orden, ticker = "GLD")
#' dg  <- bj_diagnose(est)
#' dg$pass
bj_diagnose <- function(est, alpha = 0.05, boot_sig = TRUE, B_sig = 499L,
                        save_plot = TRUE, verbose = TRUE) {
  
  ticker <- est$ticker
  if (verbose) cat(sprintf("\n[DIAGNOSIS] %s  |  modelo: %s\n",
                           ticker, est$modelo_principal))
  
  if (est$modelo_principal == "arma_garch" && !is.null(est$garch)) {
    det <- diagnosis_garch(est$garch$fit, ticker = ticker, alpha = alpha,
                           save_plot = save_plot, verbose = verbose)
  } else {
    det <- diagnosis_residuos(est$arima$fit, ticker = ticker, alpha = alpha,
                              boot_sig = boot_sig, B_sig = B_sig,
                              save_plot = save_plot, verbose = verbose)
  }
  
  if (verbose)
    cat(sprintf("\n  >> VEREDICTO: %s\n",
                ifelse(det$pass, "APROBADO (residuos ~ ruido blanco)",
                       "NO APROBADO (queda estructura; refinar modelo)")))
  
  c(list(ticker = ticker, modelo = est$modelo_principal), det)
}