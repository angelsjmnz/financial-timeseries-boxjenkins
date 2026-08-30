# =============================================================
# eda_timeseries.R
# Modulos de analisis exploratorio de series temporales
# Autor: Angel Sarria Jimenez
# Proyecto: financial-timeseries-boxjenkins
# =============================================================

# -------------------------------------------------------------
# 1. ESTADISTICOS DESCRIPTIVOS
# -------------------------------------------------------------

#' Calcula estadisticos descriptivos de series de log-retornos
#'
#' Incluye momentos de primer a cuarto orden, cuantiles de cola,
#' anualizacion de media y volatilidad, y test de normalidad de
#' Jarque-Bera.
#'
#' Nota sobre anualizacion: se emplea el factor 252 (sesiones NYSE) para
#' activos de mercado regulado y 365 para criptomonedas, que cotizan de
#' forma continua. Aplicar 252 a BTC-USD subestimaria sistematicamente
#' su volatilidad anualizada.
#'
#' @param returns_list list. Lista nombrada de xts de log-retornos.
#' @return data.frame con una fila por activo.
compute_descriptive_stats <- function(returns_list) {
  
  stopifnot(is.list(returns_list), length(returns_list) >= 1)
  out <- data.frame()
  
  for (tkr in names(returns_list)) {
    
    r <- as.numeric(returns_list[[tkr]])
    r <- r[is.finite(r)]
    
    # Factor de anualizacion segun tipo de mercado
    af <- if (grepl("-USD$", tkr)) 365 else 252
    
    jb <- tseries::jarque.bera.test(r)
    qs <- unname(quantile(r, probs = c(0.01, 0.05, 0.95, 0.99)))
    
    out <- rbind(out, data.frame(
      ticker          = tkr,
      n               = length(r),
      mean            = mean(r),
      median          = median(r),
      sd              = sd(r),
      skewness        = as.numeric(PerformanceAnalytics::skewness(r)),
      excess_kurtosis = as.numeric(PerformanceAnalytics::kurtosis(r, method = "excess")),
      min             = min(r),
      max             = max(r),
      q01             = qs[1],
      q05             = qs[2],
      q95             = qs[3],
      q99             = qs[4],
      ann_factor      = af,
      ann_return      = mean(r) * af,
      ann_volatility  = sd(r) * sqrt(af),
      jb_stat         = as.numeric(jb$statistic),
      jb_pvalue       = as.numeric(jb$p.value),
      normality       = ifelse(jb$p.value < 0.05, "RECHAZA normalidad", "No rechaza"),
      stringsAsFactors = FALSE
    ))
  }
  
  rownames(out) <- NULL
  return(out)
}


# -------------------------------------------------------------
# 2. TESTS DE ESTACIONARIEDAD
# -------------------------------------------------------------

#' Aplica la bateria conjunta ADF / KPSS / Phillips-Perron
#'
#' Los tres tests se interpretan de forma conjunta. ADF y PP tienen
#' H0 = raiz unitaria (no estacionariedad); KPSS tiene H0 = estacionariedad.
#' La columna 'conclusion' resuelve la matriz de decision.
#'
#' Advertencia tecnica: tseries acota los p-valores al rango tabulado
#' [0.01, 0.10] y emite un warning cuando el estadistico cae fuera. Se
#' suprime el warning y se registra el p-valor acotado, que es la practica
#' habitual; un p-valor de 0.01 debe leerse como "<= 0.01".
#'
#' @param series_list list. Lista nombrada de xts.
#' @param label       character. Etiqueta descriptiva ("prices" o "returns").
#' @param alpha       numeric. Nivel de significacion.
#' @return data.frame con resultados y conclusion por activo.
run_stationarity_tests <- function(series_list, label = "returns", alpha = 0.05) {
  
  stopifnot(is.list(series_list), length(series_list) >= 1)
  out <- data.frame()
  
  for (tkr in names(series_list)) {
    
    x <- as.numeric(series_list[[tkr]])
    x <- x[is.finite(x)]
    
    adf  <- suppressWarnings(tseries::adf.test(x))
    kpss <- suppressWarnings(tseries::kpss.test(x, null = "Level"))
    pp   <- suppressWarnings(tseries::pp.test(x))
    
    adf_stationary  <- adf$p.value  < alpha    # rechaza H0 raiz unitaria
    kpss_stationary <- kpss$p.value >= alpha   # no rechaza H0 estacionariedad
    pp_stationary   <- pp$p.value   < alpha
    
    conclusion <- if (adf_stationary && kpss_stationary) {
      "I(0) - Estacionaria (consenso)"
    } else if (!adf_stationary && !kpss_stationary) {
      "I(1) - No estacionaria (consenso)"
    } else if (!adf_stationary && kpss_stationary) {
      "Poco informativa - revisar tamano muestral"
    } else {
      "Discrepancia - posible tendencia o ruptura estructural"
    }
    
    out <- rbind(out, data.frame(
      series          = label,
      ticker          = tkr,
      n               = length(x),
      adf_stat        = as.numeric(adf$statistic),
      adf_pvalue      = as.numeric(adf$p.value),
      adf_stationary  = adf_stationary,
      kpss_stat       = as.numeric(kpss$statistic),
      kpss_pvalue     = as.numeric(kpss$p.value),
      kpss_stationary = kpss_stationary,
      pp_stat         = as.numeric(pp$statistic),
      pp_pvalue       = as.numeric(pp$p.value),
      pp_stationary   = pp_stationary,
      conclusion      = conclusion,
      stringsAsFactors = FALSE
    ))
  }
  
  rownames(out) <- NULL
  return(out)
}


# -------------------------------------------------------------
# 3. TESTS DE AUTOCORRELACION Y EFECTOS ARCH
# -------------------------------------------------------------

#' Tests de Ljung-Box sobre retornos y retornos al cuadrado, y test ARCH
#'
#' El contraste sobre r_t detecta estructura en la MEDIA condicional
#' (justifica el componente ARMA). El contraste sobre r_t^2 detecta
#' estructura en la VARIANZA condicional (justifica la extension GARCH).
#' Es habitual que el primero sea debil y el segundo fuertemente
#' significativo en retornos financieros diarios: esa asimetria es
#' precisamente el argumento tecnico para ARIMA-GARCH.
#'
#' Se usa fitdf = 0 porque los contrastes se aplican sobre la serie
#' observada, no sobre residuos de un modelo estimado.
#'
#' @param returns_list list. Lista nombrada de xts de log-retornos.
#' @param lags         integer. Vector de lags para Ljung-Box.
#' @param arch_lag     integer. Lag para el test ARCH de Engle.
#' @param alpha        numeric. Nivel de significacion.
#' @return data.frame en formato largo con un test por fila.
run_autocorr_tests <- function(returns_list,
                               lags     = c(5L, 10L, 20L),
                               arch_lag = 12L,
                               alpha    = 0.05) {
  
  out <- data.frame()
  
  for (tkr in names(returns_list)) {
    
    r <- as.numeric(returns_list[[tkr]])
    r <- r[is.finite(r)]
    
    for (L in lags) {
      
      lb_r  <- Box.test(r,   lag = L, type = "Ljung-Box", fitdf = 0)
      lb_r2 <- Box.test(r^2, lag = L, type = "Ljung-Box", fitdf = 0)
      
      out <- rbind(out, data.frame(
        ticker      = tkr,
        test        = "Ljung-Box (r_t)",
        lag         = L,
        statistic   = as.numeric(lb_r$statistic),
        p_value     = as.numeric(lb_r$p.value),
        significant = lb_r$p.value < alpha,
        interpretation = ifelse(lb_r$p.value < alpha,
                                "Autocorrelacion en media - justifica ARMA",
                                "Sin evidencia de autocorrelacion en media"),
        stringsAsFactors = FALSE
      ))
      
      out <- rbind(out, data.frame(
        ticker      = tkr,
        test        = "Ljung-Box (r_t^2)",
        lag         = L,
        statistic   = as.numeric(lb_r2$statistic),
        p_value     = as.numeric(lb_r2$p.value),
        significant = lb_r2$p.value < alpha,
        interpretation = ifelse(lb_r2$p.value < alpha,
                                "Heterocedasticidad condicional - justifica GARCH",
                                "Sin evidencia de heterocedasticidad"),
        stringsAsFactors = FALSE
      ))
    }
    
    # Test ARCH de Engle
    arch <- FinTS::ArchTest(r, lags = arch_lag)
    out <- rbind(out, data.frame(
      ticker      = tkr,
      test        = "ARCH LM (Engle)",
      lag         = arch_lag,
      statistic   = as.numeric(arch$statistic),
      p_value     = as.numeric(arch$p.value),
      significant = arch$p.value < alpha,
      interpretation = ifelse(arch$p.value < alpha,
                              "Efectos ARCH presentes - activar GARCH",
                              "Sin efectos ARCH detectados"),
      stringsAsFactors = FALSE
    ))
  }
  
  rownames(out) <- NULL
  return(out)
}


# -------------------------------------------------------------
# 4. OUTLIERS Y RUPTURAS ESTRUCTURALES
# -------------------------------------------------------------

#' Detecta outliers por z-score e IQR, y contrasta rupturas estructurales
#'
#' Los outliers NO se eliminan: en series financieras los valores extremos
#' son informacion economica legitima (crashes, shocks de politica monetaria)
#' y su eliminacion sesgaria a la baja la estimacion del riesgo de cola.
#' La deteccion es diagnostica y sirve para documentar la muestra.
#'
#' La ruptura estructural se contrasta mediante OLS-CUSUM sobre la media,
#' que detecta cambios de nivel acumulados a lo largo de la muestra.
#'
#' @param returns_list list. Lista nombrada de xts de log-retornos.
#' @param z_threshold  numeric. Umbral en desviaciones tipicas.
#' @param iqr_factor   numeric. Multiplicador del rango intercuartilico.
#' @return list con data.frame resumen y data.frame de fechas extremas.
detect_outliers_breaks <- function(returns_list,
                                   z_threshold = 3,
                                   iqr_factor  = 1.5) {
  
  summary_df <- data.frame()
  extremes_df <- data.frame()
  
  for (tkr in names(returns_list)) {
    
    ser <- returns_list[[tkr]]
    r   <- as.numeric(ser)
    d   <- zoo::index(ser)
    ok  <- is.finite(r)
    r   <- r[ok]; d <- d[ok]
    
    # z-score
    z       <- (r - mean(r)) / sd(r)
    out_z   <- abs(z) > z_threshold
    
    # IQR
    q1  <- quantile(r, 0.25); q3 <- quantile(r, 0.75)
    iqr <- q3 - q1
    out_iqr <- r < (q1 - iqr_factor * iqr) | r > (q3 + iqr_factor * iqr)
    
    # Ruptura estructural (OLS-CUSUM sobre la media)
    cusum   <- strucchange::efp(r ~ 1, type = "OLS-CUSUM")
    sct     <- strucchange::sctest(cusum)
    
    summary_df <- rbind(summary_df, data.frame(
      ticker           = tkr,
      n                = length(r),
      n_outliers_z     = sum(out_z),
      pct_outliers_z   = round(100 * sum(out_z) / length(r), 3),
      n_outliers_iqr   = sum(out_iqr),
      pct_outliers_iqr = round(100 * sum(out_iqr) / length(r), 3),
      max_abs_zscore   = round(max(abs(z)), 3),
      cusum_stat       = as.numeric(sct$statistic),
      cusum_pvalue     = as.numeric(sct$p.value),
      structural_break = sct$p.value < 0.05,
      stringsAsFactors = FALSE
    ))
    
    # Diez observaciones mas extremas por |z|
    top <- order(abs(z), decreasing = TRUE)[1:min(10L, length(z))]
    extremes_df <- rbind(extremes_df, data.frame(
      ticker     = tkr,
      date       = as.character(d[top]),
      log_return = round(r[top], 6),
      z_score    = round(z[top], 3),
      stringsAsFactors = FALSE
    ))
  }
  
  rownames(summary_df)  <- NULL
  rownames(extremes_df) <- NULL
  return(list(summary = summary_df, extremes = extremes_df))
}


# -------------------------------------------------------------
# 5. VISUALIZACIONES
# -------------------------------------------------------------

#' Genera y persiste el conjunto de graficos EDA por activo
#'
#' Produce por cada activo: serie de precios, serie de log-retornos,
#' histograma con densidad normal superpuesta, Q-Q plot, ACF y PACF de
#' retornos, y ACF de retornos al cuadrado.
#'
#' @param prices_list  list. xts de precios limpios.
#' @param returns_list list. xts de log-retornos.
#' @param outdir       character. Directorio de salida.
#' @param max_lag      integer. Lags maximos en ACF/PACF.
#' @return invisible(NULL). Efecto lateral: ficheros PNG.
plot_eda <- function(prices_list,
                     returns_list,
                     outdir  = here::here("outputs", "figures", "eda"),
                     max_lag = 40L) {
  
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  sv <- function(p, name, w = 9, h = 5) {
    ggplot2::ggsave(file.path(outdir, name), p, width = w, height = h, dpi = 150)
  }
  
  for (tkr in names(returns_list)) {
    
    tag <- gsub("-", "", tkr)
    r   <- as.numeric(returns_list[[tkr]])
    r   <- r[is.finite(r)]
    
    df_p <- data.frame(date  = zoo::index(prices_list[[tkr]]),
                       price = as.numeric(prices_list[[tkr]]))
    df_r <- data.frame(date  = zoo::index(returns_list[[tkr]]),
                       ret   = as.numeric(returns_list[[tkr]]))
    df_r <- df_r[is.finite(df_r$ret), ]
    
    # Serie de precios
    sv(ggplot2::ggplot(df_p, ggplot2::aes(date, price)) +
         ggplot2::geom_line(linewidth = 0.4, colour = "#1f4e79") +
         ggplot2::labs(title = paste("Serie de precios -", tkr),
                       x = NULL, y = "Precio ajustado") +
         ggplot2::theme_minimal(base_size = 11),
       sprintf("%s_01_precios.png", tag))
    
    # Serie de log-retornos
    sv(ggplot2::ggplot(df_r, ggplot2::aes(date, ret)) +
         ggplot2::geom_line(linewidth = 0.25, colour = "#7f7f7f") +
         ggplot2::labs(title = paste("Log-retornos diarios -", tkr),
                       subtitle = "Clustering de volatilidad visible en periodos de estres",
                       x = NULL, y = expression(r[t])) +
         ggplot2::theme_minimal(base_size = 11),
       sprintf("%s_02_retornos.png", tag))
    
    # Histograma vs normal
    sv(ggplot2::ggplot(df_r, ggplot2::aes(ret)) +
         ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                 bins = 100, fill = "#1f4e79", alpha = 0.65) +
         ggplot2::stat_function(fun = dnorm,
                                args = list(mean = mean(r), sd = sd(r)),
                                colour = "#c00000", linewidth = 0.8) +
         ggplot2::labs(title = paste("Distribucion de log-retornos -", tkr),
                       subtitle = "Curva roja: densidad normal ajustada",
                       x = expression(r[t]), y = "Densidad") +
         ggplot2::theme_minimal(base_size = 11),
       sprintf("%s_03_histograma.png", tag), w = 8, h = 5)
    
    # Q-Q plot
    sv(ggplot2::ggplot(df_r, ggplot2::aes(sample = ret)) +
         ggplot2::stat_qq(size = 0.5, colour = "#1f4e79") +
         ggplot2::stat_qq_line(colour = "#c00000") +
         ggplot2::labs(title = paste("Q-Q plot normal -", tkr),
                       subtitle = "Desviaciones en las colas indican leptocurtosis",
                       x = "Cuantiles teoricos", y = "Cuantiles muestrales") +
         ggplot2::theme_minimal(base_size = 11),
       sprintf("%s_04_qqplot.png", tag), w = 6, h = 5)
    
    # ACF y PACF de retornos
    sv(forecast::ggAcf(r, lag.max = max_lag) +
         ggplot2::labs(title = paste("ACF de log-retornos -", tkr)) +
         ggplot2::theme_minimal(base_size = 11),
       sprintf("%s_05_acf.png", tag), w = 8, h = 4)
    
    sv(forecast::ggPacf(r, lag.max = max_lag) +
         ggplot2::labs(title = paste("PACF de log-retornos -", tkr)) +
         ggplot2::theme_minimal(base_size = 11),
       sprintf("%s_06_pacf.png", tag), w = 8, h = 4)
    
    # ACF de retornos al cuadrado (diagnostico ARCH)
    sv(forecast::ggAcf(r^2, lag.max = max_lag) +
         ggplot2::labs(title = paste("ACF de log-retornos al cuadrado -", tkr),
                       subtitle = "Significacion persistente = heterocedasticidad condicional") +
         ggplot2::theme_minimal(base_size = 11),
       sprintf("%s_07_acf_cuadrados.png", tag), w = 8, h = 4)
    
    cat(sprintf("   · Graficos generados para %s\n", tkr))
  }
  
  invisible(NULL)
}


#' Matriz de correlaciones entre log-retornos
#'
#' Requiere alineacion temporal: se emplea interseccion de fechas
#' (inner join). Esto descarta los fines de semana de BTC-USD, que no
#' tienen contrapartida en los ETFs. Es el tratamiento correcto para una
#' matriz de correlaciones; forzar la union e imputar generaria
#' correlaciones artificialmente atenuadas.
#'
#' @param returns_list list. Lista nombrada de xts.
#' @param outdir       character. Directorio de salida.
#' @return matrix de correlaciones.
plot_correlation_matrix <- function(returns_list,
                                    outdir = here::here("outputs", "figures", "eda")) {
  
  merged <- Reduce(function(a, b) merge(a, b, join = "inner"), returns_list)
  merged <- na.omit(merged)
  colnames(merged) <- names(returns_list)
  
  cm <- cor(as.matrix(merged))
  
  df <- expand.grid(x = rownames(cm), y = colnames(cm),
                    stringsAsFactors = FALSE)
  df$corr <- as.vector(cm)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = corr)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", corr)),
                       size = 4, colour = "black") +
    ggplot2::scale_fill_gradient2(low = "#c00000", mid = "white",
                                  high = "#1f4e79", midpoint = 0,
                                  limits = c(-1, 1)) +
    ggplot2::labs(title = "Matriz de correlaciones de log-retornos",
                  subtitle = sprintf("Fechas comunes a los %d activos: n = %d",
                                     ncol(merged), nrow(merged)),
                  x = NULL, y = NULL, fill = "rho") +
    ggplot2::theme_minimal(base_size = 11)
  
  ggplot2::ggsave(file.path(outdir, "00_matriz_correlaciones.png"),
                  p, width = 7, height = 6, dpi = 150)
  
  cat(sprintf("   · Matriz de correlaciones sobre %d fechas comunes\n",
              nrow(merged)))
  return(cm)
}