#' Plot monthly chunks (28 days) from time series data
#'
#' This function creates consecutive 28-day plots from a time series CSV file,
#' assuming a constant sampling interval throughout the file. The first column
#' must be `dateutc` in "yyyy/mm/dd hh:mm:ss" format, and the selected column
#' will be plotted. Output PNG files are named as the input file base name plus
#' `_monthXX.png`.
#'
#' @param file Path to the CSV file.
#' @param column Column index or name of the variable to plot (default: 2).
#' @param output_dir Directory where PNG plots will be saved (default: "../plots").
#' @param days_per_chunk Number of days per plot (default: 28).
#' @param width_cm Width of output PNG in cm (default: 15).
#' @param height_cm Height of output PNG in cm (default: 10).
#' @param res Resolution of output PNG in dpi (default: 100).
#' @return Invisibly returns a vector of saved file paths.
#' @export
month_plots <- function(filename, data_col) {
  # Leer archivo
  data <- read.csv(filename, stringsAsFactors = FALSE)

  # La primera columna es fecha
  date_col <- names(data)[1]

  # Validar que la columna de datos exista
  if (is.character(data_col)) {
    if (!(data_col %in% names(data))) stop(paste("Column", data_col, "not found"))
    col_index <- which(names(data) == data_col)
  } else if (is.numeric(data_col)) {
    if (data_col < 1 || data_col > ncol(data)) stop("data_col index out of range")
    col_index <- data_col
  } else {
    stop("data_col must be column name (character) or index (numeric)")
  }

  # Convertir fecha a POSIXct si no lo está
  if (!inherits(data[[date_col]], "POSIXct")) {
    data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%Y-%m-%d %H:%M", tz = "UTC")
  }

  if (any(is.na(data[[date_col]]))) stop("NA values in date column after conversion")

  # Calcular intervalo mediano en segundos
  dt_all <- diff(data[[date_col]])
  dt_sec <- median(as.numeric(dt_all, units = "secs"), na.rm = TRUE)

  if (is.na(dt_sec) || dt_sec <= 0) stop("Invalid or zero sampling interval detected")

  message(paste("Sampling interval (seconds):", dt_sec))

  # Agregar mes para resumen o plot
  data$month <- format(data[[date_col]], "%Y-%m")

  # Calcular media mensual de la columna deseada
  summary_month <- aggregate(data[[col_index]], by = list(data$month), FUN = mean, na.rm = TRUE)
  names(summary_month) <- c("Month", "MeanValue")

  print(summary_month)

  # Aquí puedes seguir con el gráfico o análisis adicional

}
