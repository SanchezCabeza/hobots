#' Combine time series data from multiple CSV files into a single data frame
#'
#' Reads all CSV files in a directory matching the Mazatlan observatory naming
#' convention, combines into a single time series ordered by dateutc, removes
#' identical duplicate records (same dateutc and tem), and isolates conflicting
#' measurements (same dateutc, different tem) into a separate data frame.
#' Assumes the first column is dateutc (%Y/%m/%d %H:%M:%S) and the second is tem.
#'
#' @param dir_path Path to the directory containing CSV files (default: "data/").
#' @param pattern File name pattern to match (default: "mzt.*\\.csv$").
#' @param date_col Name of the date column (default: "dateutc").
#' @param value_col Name of the value column (default: "tem").
#' @return A list with two data frames: 'combined' (ordered, no duplicates)
#'   and 'conflicts' (records with same dateutc but different tem).
#' @export
#' @examples
#' \dontrun{
#' result <- combine_timeseries("data/")
#' write.csv(result$combined, "mzt_combined.csv", row.names = FALSE)
#' write.csv(result$conflicts, "mzt_conflicts.csv", row.names = FALSE)
#' }
combine_timeseries <- function(pattern = ".csv",
                                date_col = "dateutc", value_col = "tem") {
  # List CSV files
  files <- list.files(pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files found in directory matching pattern")

  # Read each file
  data_list <- lapply(files, function(f) {
    data <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

    # Validate columns
    if (!(date_col %in% names(data))) stop(paste("Date column", date_col, "not found in", f))
    if (!(value_col %in% names(data))) stop(paste("Value column", value_col, "not found in", f))

    # Parse dateutc as POSIXct
    data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
    if (any(is.na(data[[date_col]]))) stop(paste("Invalid dates in", f))

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

  # Identify duplicates by dateutc
  dup_indices <- duplicated(combined$dateutc) | duplicated(combined$dateutc, fromLast = TRUE)
  dup_data <- combined[dup_indices, ]

  # Separate identical and conflicting duplicates
  if (nrow(dup_data) > 0) {
    # Use data.table for efficient grouping
    library(data.table)
    dt <- as.data.table(combined)
    setkey(dt, dateutc)

    # Find unique dateutc with multiple records
    dup_dates <- dt[duplicated(dateutc) | duplicated(dateutc, fromLast = TRUE), unique(dateutc)]

    conflicts <- data.frame()
    keep_indices <- rep(TRUE, nrow(combined))

    for (d in dup_dates) {
      rows <- dt[dateutc == d]
      if (length(unique(rows$tem)) == 1) {
        # Identical tem values: keep only the first row
        keep_indices[combined$dateutc == d][-1] <- FALSE
      } else {
        # Conflicting tem values: add to conflicts
        conflicts <- rbind(conflicts, as.data.frame(rows))
        keep_indices[combined$dateutc == d] <- FALSE
      }
    }

    # Final cleaned data (no duplicates or conflicts)
    combined <- combined[keep_indices, ]
  } else {
    conflicts <- data.frame(dateutc = as.POSIXct(character()), tem = numeric())
  }

  # Ensure conflicts is sorted
  if (nrow(conflicts) > 0) {
    conflicts <- conflicts[order(conflicts$dateutc), ]
  }

  # Print summary
  cat(sprintf("Combined %d files, %d total rows, %d duplicates found, %d conflicting rows\n",
              length(files), nrow(combined) + nrow(conflicts), nrow(dup_data), nrow(conflicts)))

  # Return both data frames
  list(combined = combined, conflicts = conflicts)
}
