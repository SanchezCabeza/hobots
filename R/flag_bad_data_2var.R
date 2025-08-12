#' Flag bad data for two variables in a continuous 30-minute time series
#'
#' Creates a continuous 30-minute time series from a data frame containing
#' two numeric variables (e.g., oxygen and temperature).
#' Non-physical values are removed before interpolation.
#' Interpolation is applied only to gaps ≤ 30 minutes; longer gaps remain NA.
#' Applies separate quality flags for each variable:
#' 1 = Good, 2 = Quality Not Evaluated, 3 = Questionable/Suspect,
#' 4 = Bad, 9 = Missing Data.
#'
#' @param df Data frame containing 'dateutc' (POSIXct) and two variables.
#' @param var1 Name of the first variable (default: "oxy").
#' @param var2 Name of the second variable (default: "tem").
#' @param var1_range Numeric vector of min/max plausible values for var1 (default: c(0, 20)).
#' @param var2_range Numeric vector of min/max plausible values for var2 (default: c(5, 40)).
#' @param z_threshold Absolute z-score threshold for suspect data (default: 3).
#' @param derivative_threshold Named list with derivative thresholds for each variable,
#'        units in (value units / minute). Default: list(oxy = 20, tem = 0.1).
#'
#' @return Data frame with continuous 30-min 'dateutc' (character), both variables,
#'         and their respective flag columns.
#'
#' @export
#' @examples
#' \dontrun{
#' flagged <- flag_bad_data_2var(df, var1 = "oxy", var2 = "tem")
#' write.csv(flagged, "flagged.csv", row.names = FALSE)
#' }
flag_bad_data_2var <- function(df,
                               var1 = "oxy", var2 = "tem",
                               var1_range = c(0, 20),     # mg/L
                               var2_range = c(5, 40),     # °C
                               z_threshold = 3,
                               derivative_threshold = list(oxy = 0.2, tem = 0.1)) {
  # test
  df <- combined
  var1 = "oxy"
  var2 = "tem"
  var1_range = c(0, 20)     # mg/L
  var2_range = c(5, 40)     # °C
  z_threshold = 3
  derivative_threshold = list(oxy = 0.2, tem = 0.1)

  library(dplyr)

  # Validation
  if (!all(c("dateutc", var1, var2) %in% names(df))) {
    stop("Data frame must contain 'dateutc', ", var1, " and ", var2)
  }
  if (!inherits(df$dateutc, "POSIXct")) stop("dateutc must be POSIXct")

  # Sort by date
  df <- df %>% arrange(dateutc)

  # Remove (NA) non-physical values before interpolation
  df[[var1]][df[[var1]] < var1_range[1] | df[[var1]] > var1_range[2]] <- NA
  df[[var2]][df[[var2]] < var2_range[1] | df[[var2]] > var2_range[2]] <- NA

  # Identify segments (gaps > 30 min)
  df$diff_sec <- c(NA, difftime(df$dateutc[-1], df$dateutc[-nrow(df)], units = "secs"))
  df$segment <- cumsum(df$diff_sec > 1800 | is.na(df$diff_sec))

  # Interpolate within segments for each variable
  interp_segment <- function(seg_df, var) {
    start_time <- min(seg_df$dateutc)
    end_time   <- max(seg_df$dateutc)
    full_times <- seq.POSIXt(from = start_time, to = end_time, by = "30 min")
    # Keep only non-NA values for interpolation
    non_na_idx <- !is.na(seg_df[[var]])
    if (sum(non_na_idx) >= 2) {
      # Normal interpolation
      interpolated <- approx(x = seg_df$dateutc[non_na_idx], y = seg_df[[var]][non_na_idx],
                             xout = full_times, method = "linear")
      out <- data.frame(dateutc = full_times, value = interpolated$y)
    } else {
      # Fewer than two valid points → fill with NA
      out <- data.frame(dateutc = full_times, value = NA_real_)
    }
    names(out)[2] <- var
    return(out)
  }

  # Process segments
  segs <- unique(df$segment)
  var1_interp <- do.call(rbind, lapply(segs, function(s) interp_segment(df[df$segment == s, ], var1)))
  var2_interp <- do.call(rbind, lapply(segs, function(s) interp_segment(df[df$segment == s, ], var2)))

  # Create continuous 30-min sequence
  overall_start <- min(df$dateutc)
  overall_end   <- max(df$dateutc)
  overall_times <- seq.POSIXt(from = overall_start, to = overall_end, by = "30 min")
  result <- data.frame(dateutc = overall_times)

  result <- result %>%
    left_join(var1_interp %>% rename(!!var1 := value), by = "dateutc") %>%
    left_join(var2_interp %>% rename(!!var2 := value), by = "dateutc")

  # Initialize flags
  result[[paste0(var1, ".flag")]] <- ifelse(is.na(result[[var1]]), 9, 1)
  result[[paste0(var2, ".flag")]] <- ifelse(is.na(result[[var2]]), 9, 1)

  # Apply z-score, and derivative checks to a single variable
  apply_quality_checks <- function(df, var, range, z_th, deriv_th) {
    flag_col <- paste0(var, ".flag")
    # Compute helper columns
    df <- df %>%
      mutate(
        z_score = (.[[var]] - mean(.[[var]], na.rm = TRUE)) / sd(.[[var]], na.rm = TRUE),
        derivative = c(NA, diff(.[[var]]) / as.numeric(diff(dateutc) / 60))  # change per minute
      )
    # Z-score AND derivative check → flag = 3 (Questionable)
    df <- df %>%
      mutate(!!flag_col := ifelse(
        .data[[flag_col]] == 1 &
          abs(z_score) > z_th &
          abs(derivative) > deriv_th,
        3,
        .data[[flag_col]]
      ))
    # Remove helper columns
    df <- df %>% select(-z_score, -derivative)

    return(df)
  }


  # Apply z-score and derivative thresholds
  apply_quality_checks <- function(df, var, range, z_th, deriv_th) {
    flag_col <- paste0(var, ".flag")
    df <- df %>%
      mutate(
        z_score = (.[[var]] - mean(.[[var]], na.rm = TRUE)) / sd(.[[var]], na.rm = TRUE),
        derivative = c(0, diff(.[[var]]) / as.numeric(diff(dateutc) / 60))
      ) %>%
      mutate(!!flag_col := ifelse(.data[[flag_col]] == 1 &
                                    (.[[var]] < range[1] | .[[var]] > range[2]), 4, .data[[flag_col]])) %>%
      mutate(!!flag_col := ifelse(.data[[flag_col]] == 1 &
                                    abs(z_score) > z_th, 3, .data[[flag_col]])) %>%
      mutate(!!flag_col := ifelse(.data[[flag_col]] == 1 &
                                    abs(derivative) > deriv_th, 3, .data[[flag_col]])) %>%
      select(-z_score, -derivative)
    return(df)
  }

  # Apply for both variables
  result <- apply_quality_checks(result, var1, var1_range, z_threshold, derivative_threshold[[var1]])
  result <- apply_quality_checks(result, var2, var2_range, z_threshold, derivative_threshold[[var2]])

  # Finalize
  result$dateutc <- format(result$dateutc, "%Y-%m-%d %H:%M:%S")

  return(result)
}

