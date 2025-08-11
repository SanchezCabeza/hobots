#' Combine time series data from multiple CSV files into a single clean data frame
#'
#' Reads all CSV files matching a pattern, combines into a single time series ordered by dateutc,
#' and removes duplicate dateutc records (keeping the first occurrence). Assumes the first column
#' is dateutc (%Y/%m/%d %H:%M:%S) and the second is tem. Handles malformed dates by attempting
#' alternative formats.
#'
#' @param pattern File name pattern to match (default: ".csv" for all CSV files).
#' @param date_col Name of the date column (default: "dateutc").
#' @param value_col Name of the value column (default: "tem").
#' @return A cleaned data frame with unique, ordered dateutc records.
#' @export
#' @examples
#' \dontrun{
#' cleaned <- combine_timeseries(pattern = "mzt.*\\.csv$")
#' write.csv(cleaned, "mzt_combined_cleaned.csv", row.names = FALSE)
#' }
combine_timeseries <- function(pattern = ".csv",
                               date_col = "dateutc", value_col = "tem") {
  # # test
  # setwd(wd.clean)
  # pattern <-".csv"
  # date_col = "dateutc"
  # value_col = "tem"

  # List CSV files
  files <- list.files(pattern = pattern)
  if (length(files) == 0) stop("No files found matching pattern")
  # # test
  # i = 1
  # f = files[i]

  # Read each file
  data_list <- lapply(files, function(f) {
    cat("File:", f)
    data <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

    # Debug: Print column names
    cat(", columns: ", paste(names(data), collapse = ", "), "\n")

    # Validate columns
    if (!(date_col %in% names(data))) stop(paste("Date column", date_col, "not found in", f))
    if (!(value_col %in% names(data))) stop(paste("Value column", value_col, "not found in", f))

    # Parse dateutc, handling malformed dates
    tryCatch({
      data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
    }, error = function(e) {
      cat("Warning: Date parsing failed in", f, ", trying alternative format\n")
      data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%Y-%m-%d", tz = "UTC")
      if (any(is.na(data[[date_col]]))) {
        na_rows <- is.na(data[[date_col]])
        cat("Removed", sum(na_rows), "rows with invalid dates in", f, "\n")
        data <- data[!na_rows, ]
      }
    })

    # Keep only dateutc and tem columns
    data <- data[, c(date_col, value_col), drop = FALSE]
    names(data) <- c("dateutc", "tem")  # Standardize for consistency
    data
  })

  # Combine into one data frame
  combined <- do.call(rbind, data_list)
  if (nrow(combined) == 0) stop("No valid data after combining files")

  # Sort by dateutc
  combined <- combined[order(combined$dateutc), ]

  # Remove duplicates by dateutc, keeping first
  dup_count <- sum(duplicated(combined$dateutc))
  combined <- combined[!duplicated(combined$dateutc), ]

  # Print summary
  cat(sprintf("Combined %d files, %d unique rows after removing %d duplicates\n",
              length(files), nrow(combined), dup_count))

  # Return the clean data frame
  return(combined)
}
