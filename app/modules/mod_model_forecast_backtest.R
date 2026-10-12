# =============================================================
# mod_model.R - Pestana Modelo
# =============================================================

mod_model_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("activo"), "Activo", choices = TICKERS, selected = TICKERS[1])
    ),
    layout_columns(
      fill = FALSE,
      value_box(title = "Orden ARMA", value = textOutput(ns("orden")), theme = VB$azul),
      value_box(title = "Modelo de varianza", value = textOutput(ns("var")), theme = VB$blanco),
      value_box(title = "Diagnosis", value = textOutput(ns("diag")), theme = VB$amarillo)
    ),
    card(full_screen = TRUE,
         card_header("Diagnosis de residuos estandarizados"),
         imageOutput(ns("diag_plot"), height = "520px"))
  )
}

mod_model_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    fila <- reactive({
      m <- DATA$modelo
      if (is.null(m)) return(NULL)
      m[m$ticker == input$activo, ]
    })
    output$orden <- renderText(fila()$orden_arma)
    output$var   <- renderText("GARCH(1,1)-t")
    output$diag  <- renderText(fila()$diagnosis)
    
    output$diag_plot <- renderImage({
      tag <- gsub("-", "", input$activo)
      f <- fig_path(file.path("modeling", sprintf("%s_diagnosis_garch.png", tag)))
      if (!file.exists(f))
        f <- fig_path(file.path("modeling", sprintf("%s_diagnosis_arima.png", tag)))
      list(src = f, width = "100%",
           alt = paste("Diagnosis", input$activo))
    }, deleteFile = FALSE)
  })
}


# =============================================================
# mod_forecast.R - Pestana Prediccion
# =============================================================

mod_forecast_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("activo"), "Activo", choices = TICKERS, selected = TICKERS[1]),
      radioButtons(ns("escala"), "Escala", choices = c("Precio" = "price", "Retorno" = "ret"))
    ),
    card(full_screen = TRUE,
         card_header(textOutput(ns("titulo"))),
         plotlyOutput(ns("fc_plot"), height = "460px")),
    card(card_header("Tabla de prediccion"),
         DTOutput(ns("fc_tabla")))
  )
}

mod_forecast_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    pred <- reactive({
      p <- DATA$predicciones
      if (is.null(p)) return(NULL)
      p[p$ticker == input$activo, ]
    })
    
    output$titulo <- renderText(sprintf("Prediccion a 30 dias - %s", input$activo))
    
    output$fc_plot <- renderPlotly({
      d <- pred(); if (is.null(d)) return(NULL)
      col <- unname(COL_ACTIVO[input$activo])
      if (input$escala == "price") {
        centro <- d$pred_price; lo95 <- d$price_lower95; hi95 <- d$price_upper95
        lo80 <- d$price_lower80; hi80 <- d$price_upper80; ytit <- "Precio"
      } else {
        centro <- d$pred_return; lo95 <- d$lower95; hi95 <- d$upper95
        lo80 <- d$lower80; hi80 <- d$upper80; ytit <- "Retorno"
      }
      plot_ly(x = d$date) |>
        add_ribbons(ymin = lo95, ymax = hi95, name = "IC95",
                    line = list(width = 0), fillcolor = "rgba(31,78,121,0.12)") |>
        add_ribbons(ymin = lo80, ymax = hi80, name = "IC80",
                    line = list(width = 0), fillcolor = "rgba(31,78,121,0.22)") |>
        add_lines(y = centro, name = "Prediccion", line = list(color = col, width = 2)) |>
        layout(xaxis = list(title = ""), yaxis = list(title = ytit),
               hovermode = "x unified", legend = list(orientation = "h", y = -0.15))
    })
    
    output$fc_tabla <- renderDT({
      d <- pred(); if (is.null(d)) return(NULL)
      tt <- d[, c("date", "pred_return", "pred_price",
                  "price_lower95", "price_upper95")]
      datatable(tt, rownames = FALSE, extensions = "Buttons",
                options = list(dom = "Bfrtip", buttons = c("csv"), pageLength = 10),
                colnames = c("Fecha","Pred. retorno","Pred. precio",
                             "Precio inf 95","Precio sup 95")) |>
        formatRound(c("pred_return"), 6) |>
        formatRound(c("pred_price","price_lower95","price_upper95"), 2)
    })
  })
}


# =============================================================
# mod_backtest.R - Pestana Backtesting (validacion)
# =============================================================

mod_backtest_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      selectInput(ns("activo"), "Activo", choices = TICKERS, selected = TICKERS[1])
    ),
    layout_columns(
      fill = FALSE,
      value_box(title = "Cobertura IC95", value = textOutput(ns("cov95")),
                showcase = bsicons::bs_icon("check-circle"), theme = VB$amarillo),
      value_box(title = "Cobertura IC80", value = textOutput(ns("cov80")), theme = VB$azul),
      value_box(title = "Kupiec IC95 (p)", value = textOutput(ns("kup")), theme = VB$blanco),
      value_box(title = "Hit ratio signo", value = textOutput(ns("hit")), theme = VB$azul),
      value_box(title = "MAPE precio", value = textOutput(ns("mape")), theme = VB$blanco)
    ),
    card(full_screen = TRUE,
         card_header("Precio real vs prediccion a 1 paso (fuera de muestra)"),
         plotlyOutput(ns("bt_plot"), height = "420px")),
    card(card_header("Excepciones del IC95 (retornos fuera del intervalo)"),
         plotlyOutput(ns("exc_plot"), height = "260px"))
  )
}

mod_backtest_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    res <- reactive({
      r <- DATA$bt_resumen
      if (is.null(r)) return(NULL)
      r[r$ticker == input$activo, ]
    })
    serie <- reactive({
      s <- DATA$bt_serie
      if (is.null(s)) return(NULL)
      s[s$ticker == input$activo, ]
    })
    
    output$cov95 <- renderText(sprintf("%.1f%%", 100 * res()$cov95))
    output$cov80 <- renderText(sprintf("%.1f%%", 100 * res()$cov80))
    output$kup   <- renderText(sprintf("%.3f", res()$kupiec95_p))
    output$hit   <- renderText(sprintf("%.1f%%", 100 * res()$hit_ratio))
    output$mape  <- renderText(sprintf("%.2f%%", res()$mape_price))
    
    output$bt_plot <- renderPlotly({
      d <- serie(); if (is.null(d)) return(NULL)
      col <- unname(COL_ACTIVO[input$activo])
      plot_ly(x = d$date) |>
        add_ribbons(ymin = d$price_lower95, ymax = d$price_upper95, name = "IC95",
                    line = list(width = 0), fillcolor = "rgba(31,78,121,0.12)") |>
        add_lines(y = d$actual_price, name = "Precio real",
                  line = list(color = PAL$gris, width = 1.2)) |>
        add_lines(y = d$pred_price, name = "Prediccion 1 paso",
                  line = list(color = col, width = 1)) |>
        layout(xaxis = list(title = ""), yaxis = list(title = "Precio"),
               hovermode = "x unified", legend = list(orientation = "h", y = -0.15))
    })
    
    output$exc_plot <- renderPlotly({
      d <- serie(); if (is.null(d)) return(NULL)
      dentro <- d[d$inside95, ]; fuera <- d[!d$inside95, ]
      plot_ly() |>
        add_markers(x = dentro$date, y = dentro$z, name = "Dentro IC95",
                    marker = list(color = PAL$azul, size = 4, opacity = 0.5)) |>
        add_markers(x = fuera$date, y = fuera$z, name = "Excepcion",
                    marker = list(color = PAL$rojo, size = 7)) |>
        layout(xaxis = list(title = ""), yaxis = list(title = "Residuo estand. z"),
               shapes = list(
                 list(type = "line", x0 = min(d$date), x1 = max(d$date),
                      y0 = 0, y1 = 0, line = list(color = "grey", dash = "dot"))),
               legend = list(orientation = "h", y = -0.2))
    })
  })
}