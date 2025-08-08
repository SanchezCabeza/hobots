clean_extremes <- function(data, column = "tem", n_check = 48, factor = 3) {
  # If file name is passed, read it
  if (is.character(data)) {
    file_path <- data
    data <- tryCatch(read.csv(file_path), error = function(e) NULL)
    if (is.null(data)) {
      warning(paste("Could not read file:", file_path))
      return(NULL)
    }
  }

  # Column selection
  if (is.numeric(column)) {
    if (column > ncol(data) || column <= 0) {
      stop("Invalid column index")
    }
    col_data <- data[[column]]
  } else if (is.character(column)) {
    if (!(column %in% names(data))) {
      stop("Invalid column name")
    }
    col_data <- data[[column]]
  } else {
    stop("Column must be a name or index")
  }

  # Total rows
  n <- nrow(data)
  if (n_check * 2 > n) {
    warning("n_check too large for dataset")
    return(data)
  }

  # Indices for start and end
  idx_start <- 1:n_check
  idx_end <- (n - n_check + 1):n

  # Compute thresholds
  mean_start <- mean(col_data[idx_start], na.rm = TRUE)
  sd_start <- sd(col_data[idx_start], na.rm = TRUE)
  lower_start <- mean_start - factor * sd_start
  upper_start <- mean_start + factor * sd_start

  mean_end <- mean(col_data[idx_end], na.rm = TRUE)
  sd_end <- sd(col_data[idx_end], na.rm = TRUE)
  lower_end <- mean_end - factor * sd_end
  upper_end <- mean_end + factor * sd_end

  # Determine which indices to keep
  keep <- rep(TRUE, n)

  # Clean start
  keep[idx_start] <- col_data[idx_start] >= lower_start & col_data[idx_start] <= upper_start

  # Clean end
  keep[idx_end] <- keep[idx_end] & col_data[idx_end] >= lower_end & col_data[idx_end] <= upper_end

  # Return cleaned data
  removed <- sum(!keep)
  cat(sprintf("Initial rows: %d | NA: %d | Extremes removed: %d\n",
              n, sum(is.na(col_data)), removed))

  return(data[keep, ])
}
