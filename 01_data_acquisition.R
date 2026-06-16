# =============================================================
# 01_data_acquisition.R
# Orquestador de descarga y primer vistazo a los datos
# =============================================================

source("R/scripts/00_config.R")
source("R/functions/download_assets.R")

library(tidyquant)
library(xts)
library(here)

# Descarga completa de los 4 activos
datos <- download_assets(
  tickers  = TICKERS,
  from     = DATE_FROM,
  to       = as.character(DATE_TO),
  save_raw = TRUE
)

# Inspección básica de cada activo
for (tkr in names(datos)) {
  cat(sprintf("\n--- %s ---\n", tkr))
  cat(sprintf("Observaciones : %d\n", nrow(datos[[tkr]])))
  cat(sprintf("Desde         : %s\n", min(index(datos[[tkr]]))))
  cat(sprintf("Hasta         : %s\n", max(index(datos[[tkr]]))))
  cat(sprintf("Precio inicio : %.2f\n", as.numeric(first(datos[[tkr]]))))
  cat(sprintf("Precio final  : %.2f\n", as.numeric(last(datos[[tkr]]))))
}
