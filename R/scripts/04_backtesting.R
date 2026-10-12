# =============================================================
# 04_backtesting.R
# FASE 5 - Backtesting OOS one-step de los 4 modelos finales
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Lee los modelos Box-Jenkins guardados (Fase 4) y valida cada uno
# mediante prediccion a un paso fuera de muestra con parametros
# fijos. Produce series punto a punto (para el dashboard) y un
# resumen de cobertura, Kupiec, hit ratio y errores.
# =============================================================

source("R/scripts/00_config.R")
source("R/functions/backtesting.R")
library(forecast); library(rugarch); library(xts); library(zoo); library(here)

set.seed(SEED)

# --- Carga de retornos, precios y modelos de la Fase 4 ---
returns <- readRDS(here::here("data", "processed", "log_returns.rds"))
prices  <- readRDS(here::here("data", "processed", "prices_clean.rds"))

modelos <- list()
for (tkr in TICKERS) {
  tag <- gsub("-", "", tkr)
  f   <- here::here("outputs", "models", sprintf("boxjenkins_%s.rds", tag))
  if (file.exists(f)) modelos[[tkr]] <- readRDS(f)
}
stopifnot(length(modelos) == length(TICKERS))

cat("\n#############################################################\n")
cat("   FASE 5 - BACKTESTING OOS ONE-STEP (parametros fijos)\n")
cat("#############################################################\n")
cat("  Parametros estimados en el 80% inicial; prediccion a 1 paso\n")
cat("  rodando por el 20% final SIN reestimar. Cobertura de intervalos\n")
cat("  como metrica central de calibracion.\n")

# --- Backtesting por activo ---
bt <- list()
for (tkr in TICKERS) {
  m <- modelos[[tkr]]
  bt[[tkr]] <- backtest_oos(
    serie_ret        = returns[[tkr]],
    px               = as.numeric(prices[[tkr]]),
    orden            = m$spec$orden,
    modelo_principal = m$estimation$modelo_principal,
    dist             = "std",
    prop_train       = 0.80,
    levels           = CI_LEVELS,
    ticker           = tkr,
    save             = TRUE,
    verbose          = TRUE
  )
}

# --- Consolidacion del resumen ---
resumen <- do.call(rbind, lapply(bt, function(x) x$summary))
rownames(resumen) <- NULL
write.csv(resumen, here::here("outputs", "tables", "backtest_summary.csv"),
          row.names = FALSE)

# --- Serie consolidada punto a punto (para el dashboard) ---
all_bt <- do.call(rbind, lapply(bt, function(x) x$df))
write.csv(all_bt, here::here("outputs", "tables", "backtest_all.csv"),
          row.names = FALSE)

# --- Resumen e interpretacion ---
cat("\n#############################################################\n")
cat("   RESUMEN DE CALIBRACION\n")
cat("#############################################################\n\n")
print(resumen[, c("ticker", "cov80", "cov95", "kupiec95_p",
                  "hit_ratio", "mape_price")], row.names = FALSE, digits = 4)

cat("\nInterpretacion:\n")
cat("  - cov80 / cov95 cercanas a 0.80 / 0.95  -> intervalos bien calibrados.\n")
cat("  - Kupiec p > 0.05 -> no se rechaza que la cobertura sea la nominal (OK).\n")
cat("  - hit_ratio ~ 0.50 es esperable: la DIRECCION del retorno es casi\n")
cat("    impredecible (mercados eficientes); el valor esta en la VOLATILIDAD.\n")
cat("  - mape_price: error de la prediccion de precio a 1 dia (escala util).\n")

# Diagnostico rapido de cobertura
for (i in seq_len(nrow(resumen))) {
  r <- resumen[i, ]
  estado95 <- if (r$kupiec95_p >= 0.05) "CALIBRADO" else "mal calibrado"
  cat(sprintf("\n  %-8s IC95 cubre %.1f%% (Kupiec p=%.3f) -> %s",
              r$ticker, 100 * r$cov95, r$kupiec95_p, estado95))
}
cat("\n\n  Series punto a punto: outputs/tables/backtest_oos_<ticker>.csv\n")
cat("  Consolidado dashboard: outputs/tables/backtest_all.csv\n")
cat("\n✔ Fase 5 (backtesting OOS one-step) completada.\n")