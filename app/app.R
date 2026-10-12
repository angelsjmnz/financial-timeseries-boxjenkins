# =============================================================
# app.R - Dashboard Box-Jenkins de activos financieros
# Proyecto: financial-timeseries-boxjenkins
# Autor: Angel Sarria Jimenez
# -------------------------------------------------------------
# Punto de entrada de la app Shiny. Navegacion de pestanas a la
# IZQUIERDA (navset_pill_list), paleta neutra (azul oscuro, blanco,
# amarillo oscuro). Los graficos conservan su paleta.
#
# Ejecutar desde la raiz del proyecto:  shiny::runApp("app")
# =============================================================

# runApp("app") fija el directorio de trabajo en app/, por lo que las
# rutas son relativas a app/ (sin prefijo "app/").
source("global.R", local = FALSE)
source("modules/mod_overview_eda.R", local = FALSE)
source("modules/mod_model_forecast_backtest.R", local = FALSE)

ui <- page_fluid(
  theme = TEMA,
  tags$head(tags$style(HTML(CSS))),
  
  div(class = "app-title-bar",
      h3("Box-Jenkins | Analisis Cuantitativo de Activos Financieros",
         class = "app-title")),
  
  navset_pill_list(
    id = "nav", widths = c(2, 10), well = TRUE,
    
    nav_panel("Vision general", icon = bsicons::bs_icon("house"),
              mod_overview_ui("ov")),
    nav_panel("Exploratorio", icon = bsicons::bs_icon("bar-chart"),
              mod_eda_ui("eda")),
    nav_panel("Modelo", icon = bsicons::bs_icon("diagram-3"),
              mod_model_ui("mod")),
    nav_panel("Prediccion", icon = bsicons::bs_icon("graph-up-arrow"),
              mod_forecast_ui("fc")),
    nav_panel("Backtesting", icon = bsicons::bs_icon("shield-check"),
              mod_backtest_ui("bt"))
  )
)

server <- function(input, output, session) {
  mod_overview_server("ov")
  mod_eda_server("eda")
  mod_model_server("mod")
  mod_forecast_server("fc")
  mod_backtest_server("bt")
}

shinyApp(ui, server)