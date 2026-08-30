# =============================================================
# 02_eda.R
# Orquestador del analisis exploratorio de series temporales
# =============================================================

source("R/scripts/00_config.R")
source("R/functions/eda_timeseries.R")

library(xts); library(zoo); library(here); library(ggplot2)

set.seed(SEED)

# --- 0. Carga de datos procesados (Fase 2) ---
prices  <- readRDS(here::here("data", "processed", "prices_clean.rds"))
returns <- readRDS(here::here("data", "processed", "log_returns.rds"))

cat("\n=================================================\n")
cat("        FASE 3 - ANALISIS EXPLORATORIO\n")
cat("=================================================\n")

# --- 1. Estadisticos descriptivos ---
cat("\n[1/5] Estadisticos descriptivos ...\n")
desc <- compute_descriptive_stats(returns)
write.csv(desc, here::here("outputs", "tables", "descriptive_stats.csv"),
          row.names = FALSE)
print(desc[, c("ticker", "n", "mean", "sd", "skewness",
               "excess_kurtosis", "ann_volatility", "normality")])

# --- 2. Estacionariedad: precios vs log-retornos ---
cat("\n[2/5] Tests de estacionariedad ...\n")
stat_prices  <- run_stationarity_tests(prices,  label = "prices",  alpha = ALPHA)
stat_returns <- run_stationarity_tests(returns, label = "returns", alpha = ALPHA)
stationarity <- rbind(stat_prices, stat_returns)
write.csv(stationarity, here::here("outputs", "tables", "stationarity_tests.csv"),
          row.names = FALSE)
print(stationarity[, c("series", "ticker", "adf_pvalue",
                       "kpss_pvalue", "pp_pvalue", "conclusion")])

# --- 3. Autocorrelacion y efectos ARCH ---
cat("\n[3/5] Tests de autocorrelacion y ARCH ...\n")
autocorr <- run_autocorr_tests(returns, lags = c(5L, 10L, 20L),
                               arch_lag = 12L, alpha = ALPHA)
write.csv(autocorr, here::here("outputs", "tables", "autocorrelation_tests.csv"),
          row.names = FALSE)
print(subset(autocorr, test == "ARCH LM (Engle)")[
  , c("ticker", "statistic", "p_value", "interpretation")])

# --- 4. Outliers y rupturas estructurales ---
cat("\n[4/5] Outliers y rupturas estructurales ...\n")
ob <- detect_outliers_breaks(returns, z_threshold = 3, iqr_factor = 1.5)
write.csv(ob$summary,  here::here("outputs", "tables", "outliers_summary.csv"),
          row.names = FALSE)
write.csv(ob$extremes, here::here("outputs", "tables", "extreme_observations.csv"),
          row.names = FALSE)
print(ob$summary)

# --- 5. Visualizaciones ---
cat("\n[5/5] Generando visualizaciones ...\n")
plot_eda(prices, returns)
corr_matrix <- plot_correlation_matrix(returns)
write.csv(round(corr_matrix, 4),
          here::here("outputs", "tables", "correlation_matrix.csv"))
print(round(corr_matrix, 3))

# --- Sintesis para la Fase 4 ---
cat("\n=================================================\n")
cat("        SINTESIS PARA LA FASE 4\n")
cat("=================================================\n")
for (tkr in names(returns)) {
  d_ret  <- subset(stationarity, ticker == tkr & series == "returns")
  arch_t <- subset(autocorr, ticker == tkr & test == "ARCH LM (Engle)")
  lb_r   <- subset(autocorr, ticker == tkr &
                     test == "Ljung-Box (r_t)" & lag == 10L)
  cat(sprintf(
    "\n%-8s | d sugerido: %s | ARMA justificado: %-3s | GARCH requerido: %s\n",
    tkr,
    ifelse(grepl("^I\\(0\\)", d_ret$conclusion), "0", "1 (revisar)"),
    ifelse(lb_r$significant, "SI", "NO"),
    ifelse(arch_t$significant, "SI", "NO")
  ))
}
cat("\n✔ Fase 3 completada.\n")