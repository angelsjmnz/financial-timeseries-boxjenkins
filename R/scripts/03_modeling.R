# =============================================================
# 03_modeling.R
# FASE 4 - Aplicacion del motor Box-Jenkins a los 4 activos
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Carga los modulos, ejecuta run_boxjenkins() sobre SPY, TLT, GLD
# y BTC-USD, y consolida resumenes, predicciones y tiempos.
# =============================================================

source("R/scripts/00_config.R")
source("R/functions/boxjenkins/bj_specify.R")
source("R/functions/boxjenkins/bj_estimate.R")
source("R/functions/boxjenkins/bj_diagnose.R")
source("R/functions/boxjenkins/bj_forecast.R")
source("R/functions/boxjenkins/bj_master.R")

library(forecast); library(rugarch); library(FinTS)
library(tseries); library(xts); library(zoo); library(here)

set.seed(SEED)

# --- Carga de datos ---
returns <- readRDS(here::here("data", "processed", "log_returns.rds"))
prices  <- readRDS(here::here("data", "processed", "prices_clean.rds"))

cat("\n#############################################################\n")
cat("   FASE 4 - MODELIZACION BOX-JENKINS DE LOS 4 ACTIVOS\n")
cat("#############################################################\n")

# --- Ejecucion del pipeline por activo ---
modelos <- list()
for (tkr in TICKERS) {
  px <- prices[[tkr]]
  modelos[[tkr]] <- run_boxjenkins(
    serie      = returns[[tkr]],
    ticker     = tkr,
    last_price = as.numeric(xts::last(px)),
    last_date  = as.Date(zoo::index(xts::last(px))),
    h          = HORIZON,
    levels     = CI_LEVELS,
    max_p      = 3L, max_q = 3L,
    alpha      = ALPHA,
    boot_sig   = FALSE,
    save       = TRUE,
    verbose    = TRUE
  )
}

# --- Consolidacion de resumenes ---
resumen_global <- do.call(rbind, lapply(modelos, function(m) m$summary))
rownames(resumen_global) <- NULL
write.csv(resumen_global,
          here::here("outputs", "tables", "model_summary.csv"),
          row.names = FALSE)

# --- Consolidacion de predicciones (todas en un CSV) ---
all_pred <- do.call(rbind, lapply(modelos, function(m) m$forecast$df))
write.csv(all_pred,
          here::here("outputs", "tables", "all_predictions.csv"),
          row.names = FALSE)

# --- Resumen final en consola ---
cat("\n#############################################################\n")
cat("   RESUMEN GLOBAL DE LA MODELIZACION\n")
cat("#############################################################\n\n")
print(resumen_global[, c("ticker", "orden_arma", "modelo",
                         "diagnosis", "pred_price_h", "segundos")],
      row.names = FALSE)

cat(sprintf("\nTiempo total: %.2f s\n", sum(resumen_global$segundos)))
cat("Modelos guardados en outputs/models/\n")
cat("Predicciones consolidadas en outputs/tables/all_predictions.csv\n")
cat("\n✔ Fase 4 completada.\n")
