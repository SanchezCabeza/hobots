clean_extremes <- function(data, column = "tem", n_check = 48, factor = 3) {
  # If a file name is given, read it
  if (is.character(data)) {
    data <- read.csv(data, stringsAsFactors = FALSE)
  }

  # Validate column
  if (is.numeric(column)) {
    if (column > ncol(data) || column <= 0) {
      stop("Invalid column index.")
    }
    col_data <- data[[column]]
  } else if (is.character(column)) {
    if (!(column %in% names(data))) {
      stop("Invalid column name.")
    }
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

  # Start indices
  idx_start <- 1:n_check
  mean_start <- mean(col_data[idx_start], na.rm = TRUE)
  sd_start <- sd(col_data[idx_start], na.rm = TRUE)
  lower_start <- mean_start - factor * sd_start
  upper_start <- mean_start + factor * sd_start
  remove_start <- idx_start[col_data[idx_start] < lower_start | col_data[idx_start] > upper_start]

  # End indices
  idx_end <- (nrow(data) - n_check + 1):nrow(data)
  mean_end <- mean(col_data[idx_end], na.rm = TRUE)
  sd_end <- sd(col_data[idx_end], na.rm = TRUE)
  lower_end <- mean_end - factor * sd_end
  upper_end <- mean_end + factor * sd_end
  remove_end <- idx_end[col_data[idx_end] < lower_end | col_data[idx_end] > upper_end]

  # Keep all except removed start/end
  remove_idx <- unique(c(remove_start, remove_end))
  cleaned_data <- data[-remove_idx, ]

  cat(sprintf(
    "%d initial rows, %d marks removed, %d extremes removed\n",
    data.initial,
    data.initial - data.nomarks,
    length(remove_idx)
  ))

  return(cleaned_data)
}
