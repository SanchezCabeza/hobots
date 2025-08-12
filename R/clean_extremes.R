#' Clean extreme values from the start and end of a time series
#'
#' This function removes unrealistic extreme values from the first and last
#' `n_check` records of a time series, based on a mean ± (`factor` × standard deviation)
#' threshold computed separately for each segment. It is useful for eliminating
#' spurious measurements that occur when a sensor is out of the water at the
#' beginning or end of a deployment.
#'
#' Data will be read as a CSV file.
#' The function also #' standardizes column names so that the first column
#' is `dateutc` (date-time in `"yyyy/mm/dd hh:mm:ss"` format)
#' and the second column is `tem` (temperature).
#'
#' @param data A data frame or a character string with the path to a CSV file.
#' @param column Either the name or index of the column to check for extremes
#'        (default is `"tem"`).
#' @param n_check Integer. Number of records to check at both the start and end
#'        of the series (default: `48`).
#' @param factor Numeric. Number of standard deviations from the mean to define
#'        extremes (default: `3`).
#'
#' @return A cleaned data frame with standardized column names.
#' @details
#' The algorithm:
#' 1. Reads the file (if a path is given) and renames the first column to
#'    `"dateutc"` and the second column to `"tem"`.
#' 2. Removes `NA` values from the target column.
#' 3. Calculates mean and standard deviation of the first `n_check` values and
#'    removes points outside mean ± (`factor` × sd).
#' 4. Repeats the process independently for the last `n_check` values.
#'
#' Only data in the first and last `n_check` records can be removed.
#'
#' @examples
#' \dontrun{
#' # Clean a CSV file
#' cleaned <- clean_extremes("T_Boya24_20141126_20150211.csv")
#'
#' # Clean a data frame
#' df <- read.csv("T_Boya24_20141126_20150211.csv")
#' cleaned <- clean_extremes(df, column = "tem", n_check = 48, factor = 3)
#' }
#'
#' @export

clean_extremes <- function(data, column = "tem", n_check = 48, factor = 3) {
  # If a file name is given, read it
  if (is.character(data)) {
    data <- read.csv(data, stringsAsFactors = FALSE)
  }
  #
  # Standardize column names: first col "dateutc", second "tem"
  if (ncol(data) >= 2) {
    names(data)[1] <- "dateutc"
    names(data)[2] <- "tem"
  }

  # Validate column
  if (is.numeric(column)) {
    if (column > ncol(data) || column <= 0) stop("Invalid column index.")
    col_data <- data[[column]]
  } else if (is.character(column)) {
    if (!(column %in% names(data))) stop("Invalid column name.")
    col_data <- data[[column]]
  }

  n <- nrow(data)
  if (n_check * 2 > n) {
    warning("n_check is too large for dataset size.")
    return(data)
  }

  # Remove NAs (instrument marks)
  data.initial <- n
  data <- data[!is.na(col_data), ]
  data.nomarks <- nrow(data)
  col_data <- data[[column]]

  # If we don't have enough data left, return as is
  if (nrow(data) == 0) return(data)

  # Start indices
  idx_start <- 1:min(n_check, nrow(data))
  mean_start <- mean(col_data[idx_start], na.rm = TRUE)
  sd_start <- sd(col_data[idx_start], na.rm = TRUE)
  lower_start <- mean_start - factor * sd_start
  upper_start <- mean_start + factor * sd_start
  remove_start <- idx_start[col_data[idx_start] < lower_start | col_data[idx_start] > upper_start]

  # End indices
  idx_end <- (nrow(data) - min(n_check, nrow(data)) + 1):nrow(data)
  mean_end <- mean(col_data[idx_end], na.rm = TRUE)
  sd_end <- sd(col_data[idx_end], na.rm = TRUE)
  lower_end <- mean_end - factor * sd_end
  upper_end <- mean_end + factor * sd_end
  remove_end <- idx_end[col_data[idx_end] < lower_end | col_data[idx_end] > upper_end]

  # Remove rows safely
  remove_idx <- unique(na.omit(c(remove_start, remove_end)))
  # delete unneeded columns
  data <- data[ , 1:2] # revise for more variables
  if (length(remove_idx) == 0) {
    cleaned_data <- data
  } else {
    cleaned_data <- data[-remove_idx, , drop = FALSE]
  }

  cat(sprintf(
    "%d initial rows, %d marks removed, %d extremes removed\n",
    data.initial,
    data.initial - data.nomarks,
    length(remove_idx)
  ))

  return(cleaned_data)
}
