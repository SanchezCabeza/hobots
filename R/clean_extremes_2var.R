#' Clean extreme values from the start and end of a time series with two variables
#'
#' Removes unrealistic extreme values from the first and last `n_check` records of
#' a time series for both oxygen (`oxy`) and temperature (`tem`), based on a mean ±
#' (`factor` × standard deviation) threshold computed separately for each segment.
#' Removes entire rows if either variable is out of bounds. Useful for eliminating
#' spurious measurements when a sensor is out of the water at the start or end of a deployment.
#'
#' Data can be a CSV file or data frame. Standardizes column names to `dateutc`
#' (date-time in "yyyy/mm/dd hh:mm:ss" format), `oxy` (oxygen), and `tem` (temperature).
#'
#' @param data A data frame or a character string with the path to a CSV file.
#' @param columns Names or indices of the columns to check for extremes (default: c("oxy", "tem")).
#' @param n_check Integer. Number of records to check at both the start and end (default: 48).
#' @param factor Numeric. Number of standard deviations from the mean to define extremes (default: 3).
#'
#' @return A cleaned data frame with standardized column names (`dateutc`, `oxy`, `tem`).
#' @details
#' The algorithm:
#' 1. Reads the file (if a path is given) and renames the first column to `dateutc`,
#'    the second to `oxy`, and the third to `tem`.
#' 2. Removes rows with NA in either `oxy` or `tem`.
#' 3. Calculates mean and standard deviation for the first `n_check` values of each variable
#'    and removes rows where either `oxy` or `tem` is outside mean ± (`factor` × sd).
#' 4. Repeats the process independently for the last `n_check` values.
#' 5. Only data in the first and last `n_check` records can be removed.
#'
#' @examples
#' \dontrun{
#' # Clean a CSV file
#' cleaned <- clean_extremes("mzt.1.1m.ot.20200101.20200202.csv")
#'
#' # Clean a data frame
#' df <- read.csv("mzt.1.1m.ot.20200101.20200202.csv")
#' cleaned <- clean_extremes(df, columns = c("oxy", "tem"), n_check = 48, factor = 3)
#' }
#'
#' @export
clean_extremes <- function(data, columns = c("oxy", "tem"), n_check = 48, factor = 3) {
  # If a file name is given, read it
  if (is.character(data)) {
    data <- read.csv(data, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }

  # Standardize column names: first col "dateutc", second "oxy", third "tem"
  if (ncol(data) >= 3) {
    names(data)[1:3] <- c("dateutc", "oxy", "tem")
  } else {
    stop("Data must have at least 3 columns (dateutc, oxy, tem).")
  }

  # Validate columns
  for (col in columns) {
    if (is.numeric(col)) {
      if (col > ncol(data) || col <= 0) stop("Invalid column index: ", col)
    } else if (is.character(col)) {
      if (!(col %in% names(data))) stop("Invalid column name: ", col)
    } else {
      stop("Columns must be indices (numeric) or names (character)")
    }
  }

  # Get column data
  n <- nrow(data)
  if (n_check * 2 > n) {
    warning("n_check is too large for dataset size.")
    return(data)
  }

  # Remove rows with NA in either oxy or tem
  data_initial <- n
  data <- data[!is.na(data$oxy) & !is.na(data$tem), ]
  data_nomarks <- nrow(data)

  # If no data remains after removing NAs, return empty data frame
  if (nrow(data) == 0) {
    cat("No valid data after removing NAs.\n")
    return(data)
  }

  # Initialize indices to remove
  remove_idx <- c()

  # Check extremes for each column (oxy and tem)
  for (col in columns) {
    col_data <- data[[col]]

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

    # Combine indices to remove for this column
    remove_idx <- unique(c(remove_idx, remove_start, remove_end))
  }

  # Remove rows where either oxy or tem is extreme
  if (length(remove_idx) == 0) {
    cleaned_data <- data
  } else {
    cleaned_data <- data[-remove_idx, , drop = FALSE]
  }

  # Keep only dateutc, oxy, tem columns
  cleaned_data <- cleaned_data[, c("dateutc", "oxy", "tem"), drop = FALSE]

  # Print summary
  cat(sprintf(
    "%d initial rows, %d rows with NAs removed, %d extremes removed\n",
    data_initial,
    data_initial - data_nomarks,
    data_nomarks - nrow(cleaned_data)
  ))

  return(cleaned_data)
}
