#' Combine two-variable time series data from multiple CSV files
#'
#' Reads all CSV files matching a pattern, combines into a single time series ordered by dateutc,
#' and removes duplicate dateutc records (keeping the first occurrence). Assumes the first column
#' is a datetime field (`dateutc`, format: \%Y/\%m/\%d \%H:\%M:\%S) and the next two are the
#' variables of interest (e.g., `oxy` and `tem`). Handles malformed dates by attempting alternative formats.
#'
#' @param pattern File name pattern to match (default: ".csv" for all CSV files).
#' @param date_col Name of the date column (default: "dateutc").
#' @param value_cols Character vector with the names of the two value columns (default: c("oxy", "tem")).
#' @return A cleaned data frame with unique, ordered `dateutc` records and both variables.
#' @export
#' @examples
#' \dontrun{
#' cleaned <- combine_timeseries_2var(pattern = "mzt.*\\.csv$")
#' write.csv(cleaned, "mzt_combined_cleaned.csv", row.names = FALSE)
#' }
combine_timeseries_2var <- function(pattern = ".csv",
                                    date_col = "dateutc",
                                    value_cols = c("oxy", "tem")) {
  # List CSV files
  files <- list.files(pattern = pattern)
  if (length(files) == 0) stop("No files found matching pattern")

  if (length(value_cols) != 2) stop("value_cols must contain exactly two column names")

  data_list <- lapply(files, function(f) {
    cat("File:", f)
    data <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    cat(", columns: ", paste(names(data), collapse = ", "), "\n")

    # Validate columns
    if (!(date_col %in% names(data))) stop(paste("Date column", date_col, "not found in", f))
    if (!(value_cols[1] %in% names(data))) stop(paste("Value column", value_cols[1], "not found in", f))
    if (!(value_cols[2] %in% names(data))) stop(paste("Value column", value_cols[2], "not found in", f))

    # Parse date
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

    # Keep only dateutc and the two variables
    data <- data[, c(date_col, value_cols), drop = FALSE]
    names(data) <- c("dateutc", value_cols)  # keep original variable names
    data
  })

  # Combine into one data frame
  combined <- do.call(rbind, data_list)
  if (nrow(combined) == 0) stop("No valid data after combining files")

  # Sort by dateutc
  combined <- combined[order(combined$dateutc), ]

  # Remove duplicates by dateutc
  dup_count <- sum(duplicated(combined$dateutc))
  combined <- combined[!duplicated(combined$dateutc), ]

  cat(sprintf("Combined %d files, %d unique rows after removing %d duplicates\n",
              length(files), nrow(combined), dup_count))

  return(combined)
}

