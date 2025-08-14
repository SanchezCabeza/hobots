#' Flag bad data for two variables in a continuous 30-minute time series
#'
#' Creates a continuous 30-minute time series from a data frame containing
#' two numeric variables (e.g., oxygen and temperature).
#' Non-physical values are removed before interpolation.
#' Interpolation is applied only to gaps ≤ 30 minutes; longer gaps remain NA.
#' Applies separate quality flags for each variable:
#' 1 = Good, 2 = Quality Not Evaluated, 3 = Questionable/Suspect,
#' 4 = Bad, 9 = Missing Data.
#' Flag bad data in a two-variable time series with continuous 30-minute intervals
#'
#' This function:
#' 1. Creates a continuous 30-minute time series from a data frame containing two variables
#'    (e.g., oxygen and temperature) and a datetime column.
#' 2. Interpolates each variable within gaps ≤30 minutes using linear interpolation,
#'    leaving gaps >30 minutes as NA.
#' 3. Applies quality control flags to each variable:
#'    - 1 = Good
#'    - 3 = Questionable/Suspect
#'    - 4 = Bad
#'    - 9 = Missing Data
#' 4. Flags are based on plausible physical ranges, z-score thresholds, and derivative thresholds.
#'
#' @param df Data frame with 'dateutc' (POSIXct) and two variables.
#' @param var1 Name of the first variable (default: "oxy").
#' @param var2 Name of the second variable (default: "tem").
#' @param var1_range Numeric length-2 vector with min/max plausible values for var1.
#' @param var2_range Numeric length-2 vector with min/max plausible values for var2.
#' @param z_threshold Numeric, z-score threshold for questionable data (default: 3).
#' @param derivative_threshold Named list with thresholds for each variable (units per minute).
#'
#' @return Data frame with dateutc (character), both variables, and their respective flags.
#' @export
#'
#' @examples
#' \dontrun{
#' flagged <- flag_bad_data_2var(df, var1 = "oxy", var2 = "tem")
#' }
#'
flag_bad_data_2var <- function(df,
                               var1, var2 = "tem",
                               var1_range,
                               var2_range = c(5, 40),     # °C
                               z_threshold = 3,
                               derivative_threshold = list(0.2, 0.1)) {
  # # test
  # df <- combined
  # var1 = "con"
  # var2 = "tem"
  # var1_range = c(0, 60000)     #
  # var2_range = c(5, 40)     # °C
  # z_threshold = 3
  # derivative_threshold = list(4000, tem = 0.1)

  library(dplyr)

  # Ensure derivative_threshold is named
  if (is.null(names(derivative_threshold)) || any(names(derivative_threshold) == "")) {
    if (length(derivative_threshold) != 2) {
      stop("derivative_threshold must be length 2 if unnamed.")
    }
    names(derivative_threshold) <- c(var1, var2)
  }

  # Validate
  if (!inherits(df$dateutc, "POSIXct"))   stop("dateutc must be POSIXct")
  if (!all(c(var1, var2) %in% names(df))) stop("Both variables must be present in df")

  # Sort by time
  df <- df %>% arrange(dateutc)

  # NA non-physical values before interpolation
  df[[var1]][df[[var1]] < var1_range[1] | df[[var1]] > var1_range[2]] <- NA
  df[[var2]][df[[var2]] < var2_range[1] | df[[var2]] > var2_range[2]] <- NA

  # make small negative "oxy" <- 0
  if ("oxy" %in% names(df)) df$oxy <- ifelse(df$oxy < 0 & df$oxy > -0.1, 0, df$oxy)

  # Identify segments (gaps > 30 min)
  df$diff_sec <- c(NA, difftime(df$dateutc[-1], df$dateutc[-nrow(df)], units = "secs"))
  df$segment  <- cumsum(df$diff_sec > 1800 | is.na(df$diff_sec))

  # Function: interpolate safely for one variable in one segment
  interpolate_segment <- function(seg_df, var) {
    start_time <- min(seg_df$dateutc)
    start_time <- floor_date(start_time, unit = "30 minutes")
    end_time   <- max(seg_df$dateutc)
    end_time   <- ceiling_date(end_time, unit = "30 minutes")
    full_times <- seq.POSIXt(from = start_time, to = end_time, by = "30 min")
    non_na_idx <- !is.na(seg_df[[var]])

    if (sum(non_na_idx) >= 2) {
      interpolated <- approx(
        x = seg_df$dateutc[non_na_idx],
        y = seg_df[[var]][non_na_idx],
        xout = full_times,
        method = "linear"
      )
      out <- data.frame(dateutc = full_times, value = interpolated$y)
    } else {
      out <- data.frame(dateutc = full_times, value = NA_real_)
    }

    names(out)[2] <- var
    return(out)
  }

  # Interpolate each variable segment-wise
  interpolate_var <- function(var) {
    seg_list <- lapply(unique(df$segment), function(seg) {
      seg_df <- df %>% filter(segment == seg)
      interpolate_segment(seg_df, var)
    })
    do.call(rbind, seg_list)
  }
  full_v1 <- interpolate_var(var1)
  full_v2 <- interpolate_var(var2)

  # Build continuous timeline
  start_time <- min(full_v1$dateutc)
  start_time <- floor_date(start_time, unit = "30 minutes")
  end_time   <- max(full_v1$dateutc)
  end_time   <- ceiling_date(end_time, unit = "30 minutes")
  overall_times <- seq.POSIXt(from = start_time, to = end_time, by = "30 min")
  result <- data.frame(dateutc = overall_times) %>%
    left_join(full_v1, by = "dateutc") %>%
    left_join(full_v2, by = "dateutc")

  # Initialize flags (1 = Good, 9 = Missing)
  result <- result %>%
    mutate(!!paste0(var1, ".flag") := ifelse(is.na(.data[[var1]]), 9, 1),
           !!paste0(var2, ".flag") := ifelse(is.na(.data[[var2]]), 9, 1))

  # Function: apply QC checks for one variable
  apply_quality_checks <- function(df, var, range, z_th, deriv_th) {
    flag_col <- paste0(var, ".flag")
    df <- df %>%
      mutate(
        z_score = (.[[var]] - mean(.[[var]], na.rm = TRUE)) / sd(.[[var]], na.rm = TRUE),
        derivative = c(0, diff(.[[var]]) / as.numeric(diff(dateutc)))
      ) %>%
      mutate(!!flag_col := ifelse(.data[[flag_col]] == 1 &
                                    (.[[var]] < range[1] | .[[var]] > range[2]),
                                  4, .data[[flag_col]])) %>%
      mutate(!!flag_col := ifelse(.data[[flag_col]] == 1 &
                                    abs(z_score) > z_th,
                                  3, .data[[flag_col]])) %>%
      mutate(!!flag_col := ifelse(.data[[flag_col]] == 1 &
                                    abs(derivative) > deriv_th,
                                  3, .data[[flag_col]])) %>%
      select(-z_score, -derivative)
    return(df)
  }

  # Apply QC checks for both variables
  result <- apply_quality_checks(result, var1, var1_range, z_threshold, derivative_threshold[[var1]])
  result <- apply_quality_checks(result, var2, var2_range, z_threshold, derivative_threshold[[var2]])

  # Final formatting
  result <- result %>% mutate(dateutc = format(dateutc, "%Y-%m-%d %H:%M:%S"))

  return(result)
}

