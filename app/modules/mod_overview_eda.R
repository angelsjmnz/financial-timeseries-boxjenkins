# =============================================================
# mod_overview.R - Pestana Vision General
# =============================================================

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      value_box(title = "Activos modelizados", value = length(TICKERS),
                showcase = bsicons::bs_icon("collection"), theme = VB$azul),
      value_box(title = "Metodologia", value = "Box-Jenkins",
                showcase = bsicons::bs_icon("graph-up"), theme = VB$blanco),
      value_box(title = "Modelo de varianza", value = "GARCH(1,1)-t",
                showcase = bsicons::bs_icon("activity"), theme = VB$azul),
      value_box(title = "Horizonte", value = "30 dias",
                showcase = bsicons::bs_icon("calendar-range"), theme = VB$amarillo)
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Precios normalizados (base 100)"),
           plotlyOutput(ns("precios"), height = "420px")),
      card(full_screen = TRUE,
           card_header("Correlacion de log-retornos"),
           plotlyOutput(ns("corr"), height = "420px"))
    ),
    card(card_header("Resumen de modelos"),
         DTOutput(ns("tabla_modelos")))
  )
}

mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$precios <- renderPlotly({
      p <- plot_ly()
      for (tkr in TICKERS) {
        s <- SERIES[[tkr]]
        if (is.null(s)) next
        base100 <- 100 * s$price / s$price[1]
        p <- add_lines(p, x = s$date, y = base100, name = tkr,
                       line = list(color = unname(COL_ACTIVO[tkr])))
      }
      layout(p, xaxis = list(title = ""), yaxis = list(title = "Base 100"),
             legend = list(orientation = "h", y = -0.15),
             hovermode = "x unified")
    })
    
    output$corr <- renderPlotly({
      cm <- DATA$correlaciones
      if (is.null(cm)) return(NULL)
      rn <- cm[[1]]; m <- as.matrix(cm[, -1]); rownames(m) <- rn
      plot_ly(x = colnames(m), y = rownames(m), z = m, type = "heatmap",
              colorscale = list(c(0, PAL$rojo), c(0.5, "white"), c(1, PAL$azul)),
              zmin = -1, zmax = 1,
              text = round(m, 3), texttemplate = "%{text}",
              hovertemplate = "%{y} - %{x}: %{z:.3f}<extra></extra>") |>
        layout(xaxis = list(title = ""), yaxis = list(title = ""))
    })
    
    output$tabla_modelos <- renderDT({
      if (is.null(DATA$modelo)) return(NULL)
      datatable(DATA$modelo, rownames = FALSE, options = list(dom = "t", pageLength = 10),
                colnames = c("Activo","Orden ARMA","Modelo","Diagnosis",
                             "Metodo pred.","Ultimo precio","Pred. h=30","Seg."))
    })
  })
}


# =============================================================
# mod_eda.R - Pestana Analisis Exploratorio
# =============================================================

mod_eda_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("activo"), "Activo", choices = TICKERS, selected = TICKERS[1])
      ),
      layout_columns(
        fill = FALSE,
        value_box(title = "Volatilidad anualizada", value = textOutput(ns("vol")),
                  theme = VB$azul),
        value_box(title = "Asimetria", value = textOutput(ns("skew")), theme = VB$blanco),
        value_box(title = "Curtosis en exceso", value = textOutput(ns("kurt")), theme = VB$azul),
        value_box(title = "Normalidad (JB)", value = textOutput(ns("norm")), theme = VB$amarillo)
      ),
      card(full_screen = TRUE, card_header("Log-retornos diarios"),
           plotlyOutput(ns("retornos"), height = "320px")),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE, card_header("Distribucion de retornos"),
             plotlyOutput(ns("hist"), height = "320px")),
        card(card_header("Tests de estacionariedad"),
             DTOutput(ns("estac")))
      )
    )
  )
}

mod_eda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    desc <- reactive({
      d <- DATA$descriptivos
      if (is.null(d)) return(NULL)
      d[d$ticker == input$activo, ]
    })
    
    output$vol  <- renderText(sprintf("%.1f%%", 100 * desc()$ann_volatility))
    output$skew <- renderText(sprintf("%.2f", desc()$skewness))
    output$kurt <- renderText(sprintf("%.1f", desc()$excess_kurtosis))
    output$norm <- renderText(ifelse(desc()$jb_pvalue < 0.05, "Rechaza", "No rechaza"))
    
    output$retornos <- renderPlotly({
      s <- SERIES[[input$activo]]
      if (is.null(s)) return(NULL)
      plot_ly(x = s$date, y = s$log_return, type = "scatter", mode = "lines",
              line = list(color = PAL$gris, width = 0.6)) |>
        layout(xaxis = list(title = ""), yaxis = list(title = "r_t"))
    })
    
    output$hist <- renderPlotly({
      s <- SERIES[[input$activo]]
      if (is.null(s)) return(NULL)
      r <- s$log_return[is.finite(s$log_return)]
      dn <- density(r)
      xs <- seq(min(r), max(r), length.out = 200)
      nd <- dnorm(xs, mean(r), sd(r))
      plot_ly() |>
        add_histogram(x = r, histnorm = "probability density",
                      name = "Empirica", marker = list(color = PAL$azul), opacity = 0.6) |>
        add_lines(x = xs, y = nd, name = "Normal", line = list(color = PAL$rojo)) |>
        layout(xaxis = list(title = "r_t"), yaxis = list(title = "Densidad"),
               legend = list(orientation = "h", y = -0.2), bargap = 0.02)
    })
    
    output$estac <- renderDT({
      e <- DATA$estacionaried
      if (is.null(e)) return(NULL)
      er <- e[e$ticker == input$activo,
              c("adf_pvalue", "kpss_pvalue", "pp_pvalue", "conclusion")]
      datatable(er, rownames = FALSE, options = list(dom = "t"),
                colnames = c("ADF p", "KPSS p", "PP p", "Conclusion")) |>
        semaforo_pvalor("adf_pvalue")
    })
  })
}