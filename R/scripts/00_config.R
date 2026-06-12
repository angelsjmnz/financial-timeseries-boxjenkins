# =============================================================
# 00_config.R
# Parámetros globales del proyecto financial-timeseries-boxjenkins
# Autor: Angel Sarria Jimenez
# =============================================================

# Tickers de los activos financieros
TICKERS    <- c("SPY", "TLT", "GLD", "BTC-USD")

# Rango temporal
DATE_FROM  <- "2017-01-01"
DATE_TO    <- Sys.Date()

# Frecuencia y horizonte de predicción
FREQ       <- "daily"
HORIZON    <- 30L

# Semilla de reproducibilidad
SEED       <- 42L

# Niveles de confianza para intervalos de predicción
CI_LEVELS  <- c(80, 95)

# Umbral p-valor para tests estadísticos
ALPHA      <- 0.05
