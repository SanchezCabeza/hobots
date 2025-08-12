#' Flag bad data in a time series with continuous 30-minute intervals
#'
#' Creates a continuous 30-minute time series from a data frame with dateutc and tem.
#' Interpolates tem for gaps ≤30 minutes using linear interpolation, leaves gaps >30 minutes
#' as NA. Applies quality flags: 1 = Good, 2 = Quality Not Evaluated, 3 = Questionable/Suspect,
#' 4 = Bad, 9 = Missing Data. Outputs dateutc as character (YYYY-MM-DD HH:MM:SS).
#'
#' @param df Data frame with 'dateutc' (POSIXct) and 'tem' (numeric) columns.
#' @param temp_range Numeric vector of min/max plausible temperatures (default: c(10, 40)).
#' @param z_threshold Numeric z-score threshold for questionable data (default: 3).
#' @param jump_threshold Max allowed temp change per 30-min interval (default: 5).
#' @param flatline_threshold Max consecutive identical values (default: 5).
#' @return Data frame with dateutc (character), tem, and flag columns.
#' @export
#' @examples
#' \dontrun{
#' cleaned <- combine_timeseries(pattern = "mzt.*\\.csv$")
#' flagged <- flag_bad_data(cleaned)
#' write.csv(flagged, "mzt_flagged.csv", row.names = FALSE)
#' }
flag_bad_data <- function(df, temp_range = c(10, 40), z_threshold = 3,
                          derivative_thershold = 0.1) {
  # # test
  # df <- combined
  # temp_range = c(10, 40)
  # z_threshold = 3
  # derivative_threshold = 0.1

  library(dplyr)  # For mutate, arrange, left_join

  # Validate input
  if (!all(c("dateutc", "tem") %in% names(df))) stop("Data frame must contain 'dateutc' and 'tem' columns")
  if (!inherits(df$dateutc, "POSIXct")) stop("dateutc must be POSIXct")

  # Step 1: Sort by dateutc
  df <- df %>% arrange(dateutc)

  # Step 2: Identify segments (gaps > 30 min = 1800 sec)
  df$diff_sec <- c(NA, difftime(df$dateutc[-1], df$dateutc[-nrow(df)], units = "secs"))
  df$segment <- cumsum(df$diff_sec > 1800 | is.na(df$diff_sec))

  # Step 3: Interpolate within segments (gaps ≤ 30 min)
  full_df_list <- lapply(unique(df$segment), function(seg) {
    seg_df <- df %>% filter(segment == seg)
    start_time <- min(seg_df$dateutc)
    end_time <- max(seg_df$dateutc)
    full_times <- seq.POSIXt(from = start_time, to = end_time, by = "30 min")
    if (nrow(seg_df) > 1 && length(full_times) > 1) {
      interpolated <- approx(seg_df$dateutc, seg_df$tem, xout = full_times, method = "linear")
      data.frame(dateutc = full_times, tem = interpolated$y)
    } else {
      data.frame(dateutc = start_time, tem = seg_df$tem[1])
    }
  })

  # Step 4: Combine segments
  full_df <- do.call(rbind, full_df_list)

  # Step 5: Create continuous 30-minute sequence and merge
  overall_start <- min(full_df$dateutc)
  overall_end <- max(full_df$dateutc)
  overall_times <- seq.POSIXt(from = overall_start, to = overall_end, by = "30 min")
  overall_df <- data.frame(dateutc = overall_times)
  full_df <- overall_df %>% left_join(full_df, by = "dateutc")
  plot(full_df$dateutc, full_df$tem, pch = ".")

  # Step 6: Initialize flags (2 = Quality Not Evaluated, 9 = Missing)
  full_df <- full_df %>% mutate(flag = ifelse(is.na(tem), 9, 1))
  table(full_df$flag)

  ## Step 7: Flag bad data (4 = Bad, 3 = Questionable/Suspect, 1 = Good)
  # create columns for new filters: z-score, big jumps, flats
  full_df <- full_df %>%
    mutate(z_score = (tem - mean(tem, na.rm = TRUE)) / sd(tem, na.rm = TRUE),
           temp_diff  = c(NA, diff(tem)),
           # derivative in °C/min
           derivative = c(0, diff(full_df$tem)/as.numeric(diff(full_df$dateutc))))
  table(full_df$flag)

  # range interval
  full_df <- full_df %>%
    mutate(flag = ifelse(flag == 2 & (tem < temp_range[1] | tem > temp_range[2]), 4, flag))
  table(full_df$flag)

  #z-score
  summary(full_df$z_score) # no extremes 3 sigma
  hist(full_df$z_score, breaks = 30)
  full_df <- full_df %>%
    mutate(flag = ifelse(flag == 2 & abs(z_score) > z_threshold, 3, flag))
  table(full_df$flag)

  # derivative
  #plot(full_df$dateutc, full_df$derivative, pch = 20)
  #abline(h = c(-0.1, -0.05, 0, 0.05, 0.1)) # cut 0.1
  summary(full_df$derivative)
  hist(full_df$z_score, breaks = 30)
  full_df <- full_df %>%
    mutate(flag = ifelse(flag == 2 & abs(derivative) > derivative_threshold, 3, flag))
  table(full_df$flag)

  # Step 8: Convert dateutc to character
  full_df <- full_df %>% mutate(dateutc = format(dateutc, "%Y-%m-%d %H:%M:%S"))

  # Step 9: Remove helper columns
  full_df <- full_df %>% select(dateutc, tem, flag)

  # Summary
  cat("Flagged rows:", table(full_df$flag), "\n")
  cat("Total rows:", nrow(full_df), ", Missing (flag=9):", sum(full_df$flag == 9), "\n")

  return(full_df)
}
