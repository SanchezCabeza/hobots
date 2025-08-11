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
  # Read CSV if file path
  if (is.character(data)) {
    data <- read.csv(data, stringsAsFactors = FALSE)
  }

  # Standardize column names
  colnames(data) <- c("dateutc", "tem")

  # Validate column
  if (is.numeric(column)) {
    if (column < 2 || column > ncol(data)) stop("Invalid column index.")
  } else if (is.character(column)) {
    if (!(column %in% names(data))) stop("Invalid column name.")
    column <- which(names(data) == column)
  }

  # Original stats
  data.initial <- nrow(data)
  cat(data.initial, " ")

  # Remove NAs
  data <- data[!is.na(data[[column]]), ]
  data.nomarks <- nrow(data)
  cat(data.nomarks, " ")

  # Check size
  if (n_check * 2 > nrow(data)) {
    warning("n_check is too large for dataset size; skipping extremes removal.")
    return(data)
  }

  col_data <- data[[column]]
  #cat(length(col_data), " ")

  # Start block
  idx_start    <- seq_len(n_check)
  mean_start   <- mean(col_data[idx_start])
  sd_start     <- sd(col_data[idx_start])
  lower_start  <- mean_start - factor * sd_start
  upper_start  <- mean_start + factor * sd_start
  remove_start <- idx_start[col_data[idx_start] < lower_start | col_data[idx_start] > upper_start]
  cat(length(remove_start), " ")

  # End block
  idx_end    <- (nrow(data) - n_check + 1):nrow(data)
  mean_end   <- mean(col_data[idx_end])
  sd_end     <- sd(col_data[idx_end])
  lower_end  <- mean_end - factor * sd_end
  upper_end  <- mean_end + factor * sd_end
  remove_end <- idx_end[col_data[idx_end] < lower_end | col_data[idx_end] > upper_end]
  cat(length(remove_end), " ")

  # Keep all except removed start/end
  remove_idx <- unique(c(remove_start, remove_end))
  cat(length(remove_idx), " ")

  if (length(remove_idx) == 0) {
    cleaned_data <- data
  } else {
    cleaned_data <- data[-remove_idx, ]
  }

  # Avoid empty output
  if (length(remove_idx) >= nrow(data)) {
    warning("Extremes removal would empty dataset; skipping removal.")
    cleaned_data <- data
    removed_count <- 0
  } else {
    cleaned_data <- data[-remove_idx, ]
    removed_count <- length(remove_idx)
  }

  cat(sprintf(
    "%d initial rows, %d marks removed, %d extremes removed, %d final rows\n",
    data.initial,
    data.initial - data.nomarks,
    removed_count,
    nrow(cleaned_data)
  ))

  return(cleaned_data)
}
