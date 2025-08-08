#' Cleans bad data from start and end of each sonde (e.g., Hobo) record.
#'
#' This function removes extreme values from a selected numeric column in a data frame.
#' The criterion is based on the mean ± factor × standard deviation of the first and last
#' `n_check` values in the column. Records outside this range are assumed to be wrong and are deleted.
#'
#' @param fileName A character string specifying the CSV file name to parse.
#' @param column Column name or number to inspect (default = "tem").
#' @param n_check Number of rows to check at the beginning and end (default = 48).
#' @param factor Multiplier of standard deviation to define threshold (default = 3).
#'
#' @return A modified version of the input data frame with records with initial/last extreme values deleted.
#' @export
#' @examples
#'  \dontrun{
#' clean_extremes(fileName, column = "tem", n_check = 48, factor = 3)
#' }
clean_extremes <- function(fileName, column = "tem", n_check = 48, factor = 3) {
  # Remove rows with NA in the specified column
  # This includes extra hobo columns created with marks but no data and will be removed
  data <- read.csv(fileName, stringsAsFactors = FALSE)

  # Verificar si la columna es numérica o por nombre
  if (is.numeric(column)) {
    if (column > ncol(data) || column <= 0) {
      stop(paste("Column index", column, "is out of bounds."))
    }
    column_name <- names(data)[column]
  } else {
    if (!(column %in% names(data))) {
      stop(paste("Column", column, "not found in data."))
    }
    column_name <- column
  }

  # Guardar cantidad inicial de datos
  data.initial <- nrow(data)

  # Eliminar filas con NA en la columna seleccionada
  data <- data[!is.na(data[[column_name]]), ]
  data.nomarks <- nrow(data)

  # --- Validación por cabecera (head) ---
  head_vals  <- data[[column_name]][1:n_check]
  head_mean  <- mean(head_vals, na.rm = TRUE)
  head_sd    <- sd(head_vals, na.rm = TRUE)
  head_upper <- head_mean + factor * head_sd
  head_lower <- head_mean - factor * head_sd

  head_indices <- which(1:nrow(data) <= n_check &
                          (data[[column_name]] < head_lower | data[[column_name]] > head_upper))

  # --- Validación por cola (tail) ---
  tail_vals  <- tail(data[[column_name]], n_check)
  tail_mean  <- mean(tail_vals, na.rm = TRUE)
  tail_sd    <- sd(tail_vals, na.rm = TRUE)
  tail_upper <- tail_mean + factor * tail_sd
  tail_lower <- tail_mean - factor * tail_sd

  tail_indices <- which(1:nrow(data) > (nrow(data) - n_check) &
                          (data[[column_name]] < tail_lower | data[[column_name]] > tail_upper))

  # Eliminar filas extremas
  indices_to_remove <- unique(c(head_indices, tail_indices))
  data <- data[-indices_to_remove, ]

  # Imprimir resumen
  data.final <- nrow(data)
  cat(fileName, ": ", data.initial, "initial rows,", data.initial - data.nomarks, "with NA (likely marks),",
      data.nomarks - data.final, "extremes removed.\n")

  return(data)
}
