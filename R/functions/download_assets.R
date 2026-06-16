# =============================================================
# download_assets.R
# Función de descarga de datos financieros desde Yahoo Finance
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# =============================================================

library(tidyquant)
library(xts)
library(here)
library(lubridate)

#' Descarga datos históricos de una cesta de activos desde Yahoo Finance
#'
#' @param tickers  character. Vector de tickers a descargar.
#' @param from     character. Fecha de inicio en formato "YYYY-MM-DD".
#' @param to       character. Fecha de fin en formato "YYYY-MM-DD".
#' @param save_raw logical. Si TRUE guarda los datos crudos en data/raw/.
#' @return Lista nombrada con un xts por ticker.
#'
#' @examples
#' datos <- download_assets(c("SPY", "TLT"), from = "2017-01-01")
download_assets <- function(tickers,
                            from     = "2017-01-01",
                            to       = as.character(Sys.Date()),
                            save_raw = TRUE) {

  stopifnot(is.character(tickers), length(tickers) >= 1)
  stopifnot(is.character(from), is.character(to))

  results  <- list()
  download_log <- data.frame(
    ticker      = character(),
    date_from   = character(),
    date_to     = character(),
    n_obs       = integer(),
    status      = character(),
    timestamp   = character(),
    stringsAsFactors = FALSE
  )

  for (tkr in tickers) {

    cat(sprintf("\n→ Descargando %s ...", tkr))

    tryCatch({

      raw <- tidyquant::tq_get(
        x    = tkr,
        get  = "stock.prices",
        from = from,
        to   = to
      )

      if (is.null(raw) || nrow(raw) == 0) {
        stop("Sin datos devueltos por Yahoo Finance")
      }

      # Seleccionar precio de cierre ajustado
      # Para BTC-USD no existe adjusted; se usa close
      price_col <- if ("adjusted" %in% names(raw) &&
                       !all(is.na(raw$adjusted))) "adjusted" else "close"

      price_xts <- xts::xts(
        x         = raw[[price_col]],
        order.by  = as.Date(raw$date)
      )
      names(price_xts) <- tkr

      results[[tkr]] <- price_xts

      # Guardar dato crudo en data/raw/
      if (save_raw) {
        fname <- here::here(
          "data", "raw",
          sprintf("%s_%s_raw.rds", tkr, format(Sys.Date(), "%Y%m%d"))
        )
        saveRDS(price_xts, file = fname)
        cat(sprintf(" guardado en %s", basename(fname)))
      }

      download_log <- rbind(download_log, data.frame(
        ticker    = tkr,
        date_from = as.character(min(index(price_xts))),
        date_to   = as.character(max(index(price_xts))),
        n_obs     = nrow(price_xts),
        status    = "OK",
        timestamp = as.character(Sys.time()),
        stringsAsFactors = FALSE
      ))

      cat(" [OK]\n")

    }, error = function(e) {
      cat(sprintf(" [ERROR: %s]\n", conditionMessage(e)))
      download_log <<- rbind(download_log, data.frame(
        ticker    = tkr,
        date_from = NA_character_,
        date_to   = NA_character_,
        n_obs     = 0L,
        status    = paste("ERROR:", conditionMessage(e)),
        timestamp = as.character(Sys.time()),
        stringsAsFactors = FALSE
      ))
    })

  }

  # Guardar log de descarga
  log_path <- here::here("outputs", "tables", "download_log.csv")
  write.csv(download_log, log_path, row.names = FALSE)
  cat(sprintf("\n✔ Log de descarga guardado en %s\n", log_path))

  # Resumen final
  ok  <- sum(download_log$status == "OK")
  err <- nrow(download_log) - ok
  cat(sprintf("\nResumen: %d/%d activos descargados correctamente",
              ok, nrow(download_log)))
  if (err > 0) cat(sprintf(" | %d con errores", err))
  cat("\n")

  return(invisible(results))
}

