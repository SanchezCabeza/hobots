#' Flag bad data in a time series with continuous 30-minute intervals
#'
#' Processes a time series with dateutc and tem, transforms it into a continuous
#' 30-minute time series (filling gaps with NA for tem), and flags bad data
#' (missing, outliers, anomalies). Returns the data frame with a 'flag' column
#' (0 = good, 1 = missing, 2 = outlier, 3 = anomaly).
#'
#' @param df Data frame with 'dateutc' (POSIXct) and 'tem' (numeric) columns.
#' @param temp_range Numeric vector of min/max plausible temperatures (default: c(10, 35)).
#' @param z_threshold Numeric z-score threshold for outliers (default: 3).
#' @param jump_threshold Max allowed temp change per 30-min interval (default: 5).
#' @param flatline_threshold Max consecutive identical values (default: 5).
#' @return Data frame with continuous dateutc, tem, and flag columns.
#' @export
#' @examples
#' \dontrun{
#' cleaned <- combine_timeseries(pattern = "mzt.*\\.csv$")
#' flagged <- flag_bad_data(cleaned)
#' write.csv(flagged, "mzt_flagged.csv", row.names = FALSE)
#' }
flag_bad_data <- function(df, temp_range = c(10, 35), z_threshold = 3,
                          jump_threshold = 5, flatline_threshold = 5) {
  library(dplyr)  # For mutate, arrange, left_join

  # Validate input
  if (!all(c("dateutc", "tem") %in% names(df))) stop("Data frame must contain 'dateutc' and 'tem' columns")
  if (!inherits(df$dateutc, "POSIXct")) stop("dateutc must be POSIXct")

  # Step 1: Sort by dateutc
  df <- df %>% arrange(dateutc)

  # Step 2: Create continuous 30-minute sequence
  start_time <- min(df$dateutc, na.rm = TRUE)
  end_time <- max(df$dateutc, na.rm = TRUE)
  full_times <- seq.POSIXt(from = start_time, to = end_time, by = "30 min")
  full_df <- data.frame(dateutc = full_times)

  # Step 3: Merge with original data, filling missing tem with NA
  df <- full_df %>% left_join(df, by = "dateutc")

  # Step 4: Flag missing values (NA in tem)
  df <- df %>% mutate(flag = ifelse(is.na(tem), 1, 0))

  # Step 5: Check irregular intervals (should be ~30 min = 1800 sec)
  df$interval_sec <- c(NA, difftime(df$dateutc[-1], df$dateutc[-nrow(df)], units = "secs"))
  df <- df %>% mutate(flag = ifelse(flag == 0 & !is.na(interval_sec) & interval_sec > 3600, 3, flag))  # Flag gaps >1 hour

  # Step 6: Flag outliers (range and z-score)
  df <- df %>% mutate(z_score = (tem - mean(tem, na.rm = TRUE)) / sd(tem, na.rm = TRUE))
  df <- df %>% mutate(flag = ifelse(flag == 0 & (tem < temp_range[1] | tem > temp_range[2]), 2, flag),
                      flag = ifelse(flag == 0 & abs(z_score) > z_threshold, 2, flag))

  # Step 7: Flag anomalies (jumps)
  df$temp_diff <- c(NA, diff(df$tem))
  df <- df %>% mutate(flag = ifelse(flag == 0 & abs(temp_diff) > jump_threshold, 3, flag))

  # Step 8: Flag flatlines (consecutive identical tem)
  df$run_group <- rle(df$tem)$lengths
  df$run_group <- rep(seq_along(df$run_group), df$run_group)
  df <- df %>% group_by(run_group) %>%
    mutate(run_length = n()) %>% ungroup() %>%
    mutate(flag = ifelse(flag == 0 & run_length > flatline_threshold, 3, flag))

  # Step 9: Remove helper columns
  df <- df %>% select(-z_score, -interval_sec, -temp_diff, -run_group, -run_length)

  # Summary
  cat("Flagged rows:", table(df$flag, useNA = "ifany"), "\n")
  cat("Total rows:", nrow(df), ", Missing (flag=1):", sum(df$flag == 1), "\n")

  df
}
