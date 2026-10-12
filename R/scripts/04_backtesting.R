# =============================================================
# 04_backtesting.R
# FASE 5 - Backtesting confirmatorio de los 4 modelos finales
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Lee los modelos Box-Jenkins guardados (Fase 4), aplica el
# backtesting confirmatorio 80/20 a cada activo y produce un
# resumen ejecutivo con las metricas y la ficha de cada modelo.
# =============================================================

source("R/scripts/00_config.R")
source("R/functions/backtesting.R")
library(forecast); library(rugarch); library(xts); library(zoo); library(here)

set.seed(SEED)

# --- Carga de retornos y de los modelos de la Fase 4 ---
returns <- readRDS(here::here("data", "processed", "log_returns.rds"))

modelos <- list()
for (tkr in TICKERS) {
  tag <- gsub("-", "", tkr)
  f   <- here::here("outputs", "models", sprintf("boxjenkins_%s.rds", tag))
  if (file.exists(f)) modelos[[tkr]] <- readRDS(f)
}
stopifnot(length(modelos) == length(TICKERS))

cat("\n#############################################################\n")
cat("   FASE 5 - BACKTESTING CONFIRMATORIO (80/20)\n")
cat("#############################################################\n")
cat("  Modelo final estimado sobre el 80% inicial; evaluado en el 20% final.\n\n")

# --- Backtesting por activo ---
bt <- list()
for (tkr in TICKERS) {
  m <- modelos[[tkr]]
  dist <- "std"   # todos los GARCH del proyecto usan t-Student
  bt[[tkr]] <- backtesting_confirmatorio(
    serie            = returns[[tkr]],
    orden            = m$spec$orden,
    modelo_principal = m$estimation$modelo_principal,
    dist             = dist,
    usar_bootstrap   = isTRUE(m$diagnosis$usar_bootstrap_prr),
    prop_train       = 0.80,
    ticker           = tkr,
    verbose          = TRUE
  )
}

# --- Consolidacion de metricas ---
bt_tab <- do.call(rbind, lapply(bt, function(x)
  data.frame(ticker = x$ticker, metodo = x$metodo,
             rmse = round(x$rmse, 6), mae = round(x$mae, 6),
             mape = ifelse(is.na(x$mape), NA, round(x$mape, 2)),
             n_test = x$n_test, stringsAsFactors = FALSE)))
rownames(bt_tab) <- NULL
write.csv(bt_tab, here::here("outputs", "tables", "backtesting_results.csv"),
          row.names = FALSE)

# --- Resumen ejecutivo ---
cat("\n#############################################################\n")
cat("   RESUMEN EJECUTIVO - MODELOS FINALES\n")
cat("#############################################################\n\n")

ficha <- do.call(rbind, lapply(TICKERS, function(tkr) {
  m <- modelos[[tkr]]
  ct <- m$estimation$garch$coef_table
  nu <- if (!is.null(ct) && "shape" %in% ct$parametro)
    round(ct$estimacion[ct$parametro == "shape"], 2) else NA
  data.frame(
    activo    = tkr,
    orden     = m$summary$orden_arma,
    varianza  = ifelse(m$estimation$modelo_principal == "arma_garch",
                       "GARCH(1,1)-t", "constante"),
    nu_t      = nu,
    diagnosis = m$summary$diagnosis,
    rmse_bt   = bt_tab$rmse[bt_tab$ticker == tkr],
    stringsAsFactors = FALSE)
}))
print(ficha, row.names = FALSE)

cat("\nNotas metodologicas para el informe:\n")
cat("  1. Series financieras modelizadas en log-retornos (rendimientos continuos).\n")
cat("  2. GARCH(1,1)-t como modelo de varianza; distribucion t justificada por\n")
cat("     la curtosis y el rechazo de normalidad (Fase 3).\n")
cat("  3. La prediccion puntual de la media a varios pasos es muy incierta en\n")
cat("     activos financieros; el valor esta en los intervalos y la volatilidad.\n")
cat("  4. Limitaciones: modelo univariante (ignora correlaciones), parametros\n")
cat("     constantes (sin cambios de regimen), hipotesis de estabilidad futura.\n")

cat("\n  Siguiente iteracion del backtesting: walk-forward, cobertura empirica\n")
cat("  de intervalos, Diebold-Mariano vs random walk y hit ratio de signo.\n")
cat("\n✔ Fase 5 (backtesting confirmatorio) completada.\n")