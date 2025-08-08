#' Cleans bad data from start and end of each sonde (e.g., Hobo) record.
#'
#' This function removes extreme values from a selected numeric column in a data frame.
#' The criterion is based on the mean ± factor × standard deviation of the first and last
#' `n_check` values in the column. Records outside this range are assumed to be wrong and are deleted.
#'
#' @param data A data frame containing a time series column (e.g., temperature).
#' @param column Column name or number to inspect (default = "tem").
#' @param n_check Number of rows to check at the beginning and end (default = 48).
#' @param factor Multiplier of standard deviation to define threshold (default = 3).
#'
#' @return A modified version of the input data frame with records with initial/last extreme values deleted.
#' @export
#' @examples
#'  \dontrun{
#' clean_extremes(data, column = "tem", n_check = 48, factor = 3)
#' }
clean_extremes <- function(data, column = "tem", n_check = 48, factor = 3) {
  # Remove rows with NA in the specified column
  # This includes extra hobo columns created with marks but no data and will be removed
  if (is.numeric(column)) {
    if (column > ncol(data) | column <= 0) {
      stop(paste("Column", column, "not found in data."))
    }
  } else {
    if (!(column %in% names(data))) {
      stop(paste("Column", column, "not found in data."))
    }
  }

  # immediately delete non-existing records (e.g., instrument marks)
  data.initial <- nrow(data)
  data <- data[!is.na(data[[column]]), ]
  data.nomarks <- nrow(data)

  # Head
  head_vals    <- data[[column]][1:n_check]
  head_mean    <- mean(head_vals, na.rm = TRUE)
  head_sd      <- sd(head_vals, na.rm = TRUE)
  head_upper   <- head_mean + factor * head_sd
  head_lower   <- head_mean - factor * head_sd
  head_indices <- which(1:nrow(data) <= n_check &
                          (data[[column]] < head_lower | data[[column]] > head_upper))

  # Tail
  tail_vals    <- tail(data[[column]], n_check)
  tail_mean    <- mean(tail_vals, na.rm = TRUE)
  tail_sd      <- sd(tail_vals, na.rm = TRUE)
  tail_upper   <- tail_mean + factor * tail_sd
  tail_lower   <- tail_mean - factor * tail_sd
  tail_indices <- which(1:nrow(data) > (nrow(data) - n_check) &
                          (data[[column]] < tail_lower | data[[column]] > tail_upper))

  # Delete those records and return
  data[c(head_indices, tail_indices), ] <- NULL
  data.final <- nrow(data)
  cat(data.initial, "initial data, ", data.initial - data.nomarks, " were only marks, ", data.nomarks - data.final, " were deleted.")

  return(out)
}
