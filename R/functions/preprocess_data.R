# =============================================================
# preprocess_data.R
# Validación, limpieza y cálculo de log-retornos
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# =============================================================

#' Valida, limpia y transforma series de precios en log-retornos
#'
#' Aplica una batería de controles de calidad sobre cada serie de precios
#' (NA, Inf, duplicados, valores no positivos, gaps de calendario) y calcula
#' los log-retornos diarios r_t = ln(P_t / P_{t-1}), que constituyen la serie
#' objeto de modelizacion Box-Jenkins.
#'
#' Supuesto metodológico: por defecto las observaciones con NA se ELIMINAN en
#' lugar de imputarse. La imputación por arrastre (na.locf) genera retornos
#' artificiales nulos que sesgan a la baja la autocorrelación muestral y, por
#' tanto, contaminan la fase de identificación del orden ARMA. El parámetro
#' na_strategy permite auditar el impacto de la alternativa.
#'
#' @param price_list   list. Lista nombrada de objetos xts de precios (output
#'                     de download_assets()).
#' @param na_strategy  character. "remove" (default) o "locf".
#' @param gap_threshold integer. Nº de días naturales a partir del cual un hueco
#'                     de calendario se registra como advertencia. Default 7.
#' @param save_processed logical. Si TRUE persiste los resultados en
#'                     data/processed/.
#' @return Lista con tres elementos:
#'   \describe{
#'     \item{prices}{lista de xts de precios limpios}
#'     \item{returns}{lista de xts de log-retornos}
#'     \item{quality}{data.frame con el reporte de calidad por activo}
#'   }
#'
#' @examples
#' source("R/scripts/00_config.R")
#' datos <- download_assets(TICKERS, from = DATE_FROM)
#' clean <- preprocess_data(datos)
preprocess_data <- function(price_list,
                            na_strategy     = c("remove", "locf"),
                            gap_threshold   = 7L,
                            save_processed  = TRUE) {
  
  # --- Validaciones de entrada ---
  stopifnot(is.list(price_list), length(price_list) >= 1)
  stopifnot(!is.null(names(price_list)))
  na_strategy <- match.arg(na_strategy)
  
  prices_clean <- list()
  returns_list <- list()
  
  quality <- data.frame(
    ticker          = character(),
    n_raw           = integer(),
    n_na            = integer(),
    n_inf           = integer(),
    n_nonpositive   = integer(),
    n_duplicated    = integer(),
    n_clean         = integer(),
    n_returns       = integer(),
    na_strategy     = character(),
    max_gap_days    = integer(),
    n_gaps_flagged  = integer(),
    pct_data_loss   = numeric(),
    date_from       = character(),
    date_to         = character(),
    stringsAsFactors = FALSE
  )
  
  for (tkr in names(price_list)) {
    
    cat(sprintf("\n→ Procesando %s ...\n", tkr))
    x <- price_list[[tkr]]
    
    if (!xts::is.xts(x)) {
      warning(sprintf("%s no es un objeto xts. Se omite.", tkr))
      next
    }
    
    n_raw <- nrow(x)
    
    # --- 1. Duplicados en el índice temporal ---
    dup_idx      <- duplicated(zoo::index(x))
    n_duplicated <- sum(dup_idx)
    if (n_duplicated > 0) {
      x <- x[!dup_idx, ]
      cat(sprintf("   · %d fecha(s) duplicada(s) eliminada(s)\n", n_duplicated))
    }
    
    # --- 2. Valores no finitos (Inf, NaN) → se convierten a NA ---
    vals   <- as.numeric(x)
    n_inf  <- sum(is.infinite(vals) | is.nan(vals))
    if (n_inf > 0) {
      x[is.infinite(vals) | is.nan(vals)] <- NA
      cat(sprintf("   · %d valor(es) no finito(s) marcado(s) como NA\n", n_inf))
    }
    
    # --- 3. Precios no positivos → inválidos para escala logarítmica ---
    vals          <- as.numeric(x)
    nonpos        <- !is.na(vals) & vals <= 0
    n_nonpositive <- sum(nonpos)
    if (n_nonpositive > 0) {
      x[nonpos] <- NA
      cat(sprintf("   · %d precio(s) <= 0 marcado(s) como NA\n", n_nonpositive))
    }
    
    # --- 4. Tratamiento de NA según estrategia declarada ---
    n_na <- sum(is.na(as.numeric(x)))
    if (n_na > 0) {
      cat(sprintf("   · %d NA detectado(s) (%.2f%%) → estrategia '%s'\n",
                  n_na, 100 * n_na / nrow(x), na_strategy))
      if (na_strategy == "remove") {
        x <- x[!is.na(as.numeric(x)), ]
      } else {
        x <- zoo::na.locf(x, na.rm = FALSE)
        x <- x[!is.na(as.numeric(x)), ]   # elimina NA iniciales sin previo
      }
    }
    
    n_clean <- nrow(x)
    
    if (n_clean < 100L) {
      warning(sprintf("%s: solo %d observaciones limpias. Muestra insuficiente.",
                      tkr, n_clean))
    }
    
    # --- 5. Continuidad del calendario ---
    # Los ETFs siguen el calendario NYSE (~252 sesiones/ano) y presentan huecos
    # de fin de semana y festivos por construccion. BTC-USD cotiza 24/7. Por
    # ello los gaps se registran como advertencia informativa, nunca como error.
    dates      <- zoo::index(x)
    gaps       <- as.integer(diff(dates))
    max_gap    <- if (length(gaps) > 0) max(gaps) else NA_integer_
    n_gaps     <- sum(gaps > gap_threshold)
    if (n_gaps > 0) {
      cat(sprintf("   · %d hueco(s) > %d dias naturales (max: %d dias)\n",
                  n_gaps, gap_threshold, max_gap))
    }
    
    # --- 6. Log-retornos: r_t = ln(P_t) - ln(P_{t-1}) ---
    # Se calculan sobre el indice observado, no sobre calendario natural.
    # Los log-retornos son aditivos en el tiempo y aproximadamente simetricos,
    # propiedades requeridas por la modelizacion ARMA.
    log_prices <- log(x)
    ret        <- diff(log_prices)
    ret        <- ret[-1, ]                # descarta el primer NA estructural
    names(ret) <- tkr
    
    prices_clean[[tkr]] <- x
    returns_list[[tkr]] <- ret
    
    # --- 7. Registro de calidad ---
    quality <- rbind(quality, data.frame(
      ticker         = tkr,
      n_raw          = n_raw,
      n_na           = n_na,
      n_inf          = n_inf,
      n_nonpositive  = n_nonpositive,
      n_duplicated   = n_duplicated,
      n_clean        = n_clean,
      n_returns      = nrow(ret),
      na_strategy    = na_strategy,
      max_gap_days   = max_gap,
      n_gaps_flagged = n_gaps,
      pct_data_loss  = round(100 * (n_raw - n_clean) / n_raw, 4),
      date_from      = as.character(min(dates)),
      date_to        = as.character(max(dates)),
      stringsAsFactors = FALSE
    ))
    
    cat(sprintf("   ✔ %d precios limpios | %d log-retornos\n",
                n_clean, nrow(ret)))
  }
  
  # --- 8. Persistencia ---
  if (save_processed) {
    saveRDS(prices_clean,
            here::here("data", "processed", "prices_clean.rds"))
    saveRDS(returns_list,
            here::here("data", "processed", "log_returns.rds"))
    
    # Export CSV para interoperabilidad (Power BI, inspeccion manual)
    for (tkr in names(returns_list)) {
      df <- data.frame(
        date       = zoo::index(returns_list[[tkr]]),
        log_return = as.numeric(returns_list[[tkr]]),
        price      = as.numeric(prices_clean[[tkr]][-1, ])
      )
      write.csv(
        df,
        here::here("data", "processed",
                   sprintf("%s_processed.csv", gsub("-", "", tkr))),
        row.names = FALSE
      )
    }
    cat("\n✔ Datos procesados guardados en data/processed/\n")
  }
  
  # --- 9. Reporte de calidad ---
  qpath <- here::here("outputs", "tables", "data_quality_report.csv")
  write.csv(quality, qpath, row.names = FALSE)
  cat(sprintf("✔ Reporte de calidad guardado en %s\n", qpath))
  
  return(invisible(list(
    prices  = prices_clean,
    returns = returns_list,
    quality = quality
  )))
}