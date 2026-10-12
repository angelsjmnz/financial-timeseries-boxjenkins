# =============================================================
# global.R
# Dashboard Shiny - Carga de datos y utilidades compartidas
# Proyecto: financial-timeseries-boxjenkins
# -------------------------------------------------------------
# Se ejecuta una vez al arrancar la app. Carga los outputs ya
# generados por el pipeline (Fases 3-5). NO reestima modelos:
# el dashboard solo consume CSV precomputados.
# =============================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)

# --- Localizacion robusta de los outputs ---
.root <- tryCatch(here::here(), error = function(e) normalizePath(".."))
tbl_path <- function(f) file.path(.root, "outputs", "tables", f)
fig_path <- function(f) file.path(.root, "outputs", "figures", f)
proc_path <- function(f) file.path(.root, "data", "processed", f)

# --- Lectura segura (devuelve NULL si falta el fichero) ---
read_safe <- function(path, ...) {
  if (file.exists(path)) read.csv(path, stringsAsFactors = FALSE, ...) else NULL
}

# --- Carga de datos del proyecto ---
DATA <- list(
  descriptivos  = read_safe(tbl_path("descriptive_stats.csv")),
  estacionaried = read_safe(tbl_path("stationarity_tests.csv")),
  autocorr      = read_safe(tbl_path("autocorrelation_tests.csv")),
  correlaciones = read_safe(tbl_path("correlation_matrix.csv")),
  modelo        = read_safe(tbl_path("model_summary.csv")),
  predicciones  = read_safe(tbl_path("all_predictions.csv")),
  bt_resumen    = read_safe(tbl_path("backtest_summary.csv")),
  bt_serie      = read_safe(tbl_path("backtest_all.csv"))
)

# --- Series de precios/retornos procesadas (una por activo) ---
TICKERS <- if (!is.null(DATA$modelo)) DATA$modelo$ticker else
  c("SPY", "TLT", "GLD", "BTC-USD")

load_processed <- function(tkr) {
  tag <- gsub("-", "", tkr)
  df  <- read_safe(proc_path(sprintf("%s_processed.csv", tag)))
  if (!is.null(df)) df$date <- as.Date(df$date)
  df
}
SERIES <- setNames(lapply(TICKERS, load_processed), TICKERS)

# --- Normaliza fechas de las tablas temporales ---
if (!is.null(DATA$predicciones)) DATA$predicciones$date <- as.Date(DATA$predicciones$date)
if (!is.null(DATA$bt_serie))     DATA$bt_serie$date     <- as.Date(DATA$bt_serie$date)

# --- Paleta neutra de la interfaz (botones, pills, tarjetas, KPIs) ---
AZUL_OSCURO     <- "#16324f"
AMARILLO_OSCURO <- "#b8860b"
BLANCO          <- "#ffffff"

# --- Paleta de los graficos (se mantiene la seleccion anterior) ---
PAL <- list(azul = "#1f4e79", rojo = "#c00000", verde = "#2e8b57",
            gris = "#7f7f7f", banda = "rgba(31,78,121,0.15)")

# Color por clase de activo (para consistencia visual en graficos)
COL_ACTIVO <- c("SPY" = "#1f4e79", "TLT" = "#2e8b57",
                "GLD" = "#b8860b", "BTC-USD" = "#c00000")

# --- Temas de value_box con la paleta neutra ---
VB <- list(
  azul     = value_box_theme(bg = AZUL_OSCURO,     fg = BLANCO),
  amarillo = value_box_theme(bg = AMARILLO_OSCURO, fg = BLANCO),
  blanco   = value_box_theme(bg = BLANCO,          fg = AZUL_OSCURO)
)

# --- Utilidad: semaforo de p-valores para tablas DT ---
semaforo_pvalor <- function(dt, col) {
  DT::formatStyle(dt, col,
                  backgroundColor = DT::styleInterval(
                    c(0.05, 0.10),
                    c("#f8d7da", "#fff3cd", "#d4edda")))  # rojo / amarillo / verde
}

# --- Tema visual (Bootstrap 5 via bslib), neutro ---
TEMA <- bs_theme(
  version = 5,
  primary = AZUL_OSCURO,
  secondary = AMARILLO_OSCURO,
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "card-cap-bg" = AZUL_OSCURO
)

# --- CSS a medida: pills a la izquierda, cabeceras y botones neutros ---
CSS <- "
.card-header { background-color:#16324f; color:#fff; font-weight:600; }
.nav-pills .nav-link { color:#16324f; border:1px solid #16324f;
  margin-bottom:6px; text-align:left; font-weight:500; }
.nav-pills .nav-link.active { background-color:#16324f; color:#fff; }
.nav-pills .nav-link:hover { background-color:#b8860b; color:#fff; }
.dt-button { background-color:#16324f !important; color:#fff !important;
  border:none !important; }
.app-title { color:#16324f; font-weight:700; }
.app-title-bar { border-bottom:3px solid #b8860b; padding-bottom:8px;
  margin-bottom:16px; }
"