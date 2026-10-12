# =============================================================
# 01_data_acquisition.R
# Orquestador: descarga + validacion + log-retornos
# =============================================================

source("R/scripts/00_config.R")
source("R/functions/download_assets.R")
source("R/functions/preprocess_data.R")

library(tidyquant)
library(xts)
library(zoo)
library(here)

set.seed(SEED)

# --- 1. Descarga ---
datos_raw <- download_assets(
  tickers  = TICKERS,
  from     = DATE_FROM,
  to       = as.character(DATE_TO),
  save_raw = TRUE
)

# --- 2. Preprocesamiento ---
datos <- preprocess_data(
  price_list     = datos_raw,
  na_strategy    = "remove",
  gap_threshold  = 7L,
  save_processed = TRUE
)

# --- 3. Reporte de calidad en consola ---
cat("\n=============== REPORTE DE CALIDAD ===============\n")
print(datos$quality)

# --- 4. Inspeccion de log-retornos ---
cat("\n=============== LOG-RETORNOS ===============\n")
for (tkr in names(datos$returns)) {
  r <- as.numeric(datos$returns[[tkr]])
  cat(sprintf(
    "\n%-8s | n = %5d | media = %+9.6f | sd = %.6f | min = %+.4f | max = %+.4f\n",
    tkr, length(r), mean(r), sd(r), min(r), max(r)
  ))
}
