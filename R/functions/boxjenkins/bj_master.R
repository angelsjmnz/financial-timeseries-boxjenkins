# =============================================================
# bj_master.R
# ORQUESTADOR del motor Box-Jenkins: run_boxjenkins()
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Encadena los cuatro modulos de la metodologia Box-Jenkins en
# una sola llamada por activo:
#   1. bj_specify   -> especificacion (d, estacionalidad, orden ARMA)
#   2. bj_estimate  -> estimacion (ARIMA + rama ARMA-GARCH-t)
#   3. bj_diagnose  -> diagnosis de residuos (ruido blanco)
#   4. bj_forecast  -> prediccion a h dias + espacio de precios
# Devuelve una estructura unica con los resultados de cada etapa
# y un resumen, y persiste el objeto modelo en outputs/models/.
#
# Requiere que los cuatro modulos esten cargados previamente
# (source de bj_specify, bj_estimate, bj_diagnose, bj_forecast).
# =============================================================

#' Ejecuta el pipeline Box-Jenkins completo para un activo
#'
#' @param serie       numeric/xts. Serie de log-retornos del activo.
#' @param ticker      character. Etiqueta del activo.
#' @param last_price  numeric. Ultimo precio observado (para espacio de precios).
#' @param last_date   Date. Fecha de la ultima observacion.
#' @param h           integer. Horizonte de prediccion (dias naturales).
#' @param levels      numeric. Niveles de confianza de los intervalos.
#' @param max_p,max_q integer. Ordenes maximos del grid de especificacion.
#' @param alpha       numeric. Nivel de significacion de los contrastes.
#' @param boot_sig    logical. Bootstrap de significatividad (ruta ARIMA).
#' @param B_prr       integer. Replicas del bootstrap PRR de prediccion.
#' @param save        logical. Persistir modelo, tablas y figuras.
#' @param verbose     logical. Log de progreso por etapa.
#' @return list con spec, estimation, diagnosis, forecast y summary.
#'
#' @examples
#' returns <- readRDS(here::here("data","processed","log_returns.rds"))
#' prices  <- readRDS(here::here("data","processed","prices_clean.rds"))
#' px <- prices[["GLD"]]
#' res <- run_boxjenkins(returns[["GLD"]], ticker = "GLD",
#'                       last_price = as.numeric(xts::last(px)),
#'                       last_date  = as.Date(zoo::index(xts::last(px))))
run_boxjenkins <- function(serie, ticker,
                           last_price, last_date,
                           h = 30L, levels = c(80, 95),
                           max_p = 3L, max_q = 3L, alpha = 0.05,
                           boot_sig = FALSE, B_prr = 999L,
                           save = TRUE, verbose = TRUE) {
  
  t_ini <- Sys.time()
  if (verbose) {
    cat("\n=============================================================\n")
    cat(sprintf("  BOX-JENKINS  |  %s\n", ticker))
    cat("=============================================================\n")
  }
  
  # --- 1. Especificacion ---
  if (verbose) cat("\n[1/4] Especificacion ...\n")
  spec <- bj_specify(serie, ticker = ticker,
                     max_p = max_p, max_q = max_q,
                     prop_train = 0.80, verbose = verbose)
  
  # --- 2. Estimacion ---
  if (verbose) cat("\n[2/4] Estimacion ...\n")
  est <- bj_estimate(serie, orden = spec$orden, ticker = ticker,
                     alpha = alpha, verbose = verbose)
  
  # --- 3. Diagnosis ---
  if (verbose) cat("\n[3/4] Diagnosis ...\n")
  dg <- bj_diagnose(est, alpha = alpha, boot_sig = boot_sig,
                    save_plot = save, verbose = verbose)
  
  # --- 4. Prediccion ---
  if (verbose) cat("\n[4/4] Prediccion ...\n")
  fc <- bj_forecast(est, serie = serie,
                    last_price = last_price, last_date = last_date,
                    h = h, levels = levels, diag = dg,
                    B_prr = B_prr, save = save, verbose = verbose)
  
  secs <- as.numeric(difftime(Sys.time(), t_ini, units = "secs"))
  
  # --- Resumen ---
  resumen <- data.frame(
    ticker        = ticker,
    orden_arma    = sprintf("(%d,%d,%d)", spec$orden[1], spec$orden[2], spec$orden[3]),
    modelo        = ifelse(est$modelo_principal == "arma_garch",
                           "ARMA-GARCH(1,1)-t", "ARIMA"),
    diagnosis     = ifelse(dg$pass, "APROBADO", "NO APROBADO"),
    metodo_pred   = fc$metodo,
    last_price    = last_price,
    pred_price_h  = fc$df$pred_price[h],
    segundos      = round(secs, 2),
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    cat("\n-------------------------------------------------------------\n")
    cat(sprintf("  RESUMEN %s\n", ticker))
    cat(sprintf("  Modelo       : %s %s\n", resumen$modelo, resumen$orden_arma))
    cat(sprintf("  Diagnosis    : %s\n", resumen$diagnosis))
    cat(sprintf("  Prediccion   : P(h=%d) = %.4f  [%s]\n",
                h, resumen$pred_price_h, fc$metodo))
    cat(sprintf("  Tiempo total : %.2f s\n", secs))
    cat("-------------------------------------------------------------\n")
  }
  
  # --- Persistencia del objeto modelo ---
  resultado <- list(
    ticker     = ticker,
    spec       = spec,
    estimation = est,
    diagnosis  = dg,
    forecast   = fc,
    summary    = resumen,
    run_time   = secs
  )
  
  if (save) {
    tag <- gsub("-", "", ticker)
    saveRDS(resultado,
            here::here("outputs", "models", sprintf("boxjenkins_%s.rds", tag)))
    if (verbose) cat(sprintf("  Modelo guardado: outputs/models/boxjenkins_%s.rds\n", tag))
  }
  
  invisible(resultado)
}