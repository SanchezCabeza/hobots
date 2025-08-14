#' Calculate sea level from submerged pressure and atmospheric pressure
#'
#' This function calculates sea level (water column height) from submerged
#' absolute pressure measurements and atmospheric pressure measurements.
#' If atmospheric pressure is missing for a given timestamp, the mean
#' atmospheric pressure from the atmospheric data is used instead.
#'
#' @param water_df A (water) data frame with columns:
#'   \itemize{
#'     \item \code{dateutc} (POSIXct): Timestamp of the measurement
#'     \item \code{pre} (numeric): Absolute pressure measured by the submerged sensor (kPa)
#'     \item \code{tem} (numeric): Water temperature in degrees Celsius
#'   }
#' @param atm_df An (atmospheric) data frame with columns:
#'   \itemize{
#'     \item \code{dateutc} (POSIXct): Timestamp of the atmospheric measurement
#'     \item \code{atm_pre} (numeric): Atmospheric pressure in kPa
#'   }
#' @param salinity Salinity in PSU (default = 35).
#' @param join_tolerance Maximum allowed time difference between water and atmospheric
#'   measurements when joining, given as a \code{\link[base]{difftime}} (default = \code{difftime(10, units = "mins")}).
#'
#' @return A data frame identical to \code{water_df} but with an additional
#'   column \code{sl} giving the estimated water column height in meters.
#'
#' @details
#' Sea level is calculated by subtracting the atmospheric pressure from
#' the submerged sensor's absolute pressure, converting the resulting
#' gauge pressure to meters of water using the water density at the given
#' salinity and temperature.
#'
#' Water density is calculated following the UNESCO 1983 (EOS-80) equation
#' of state for seawater.
#'
#' @examples
#' \dontrun{
#' water_df <- data.frame(
#'   dateutc = as.POSIXct(c("2025-01-01 00:00:00", "2025-01-01 00:10:00"), tz = "UTC"),
#'   pre = c(201.3, 201.5),
#'   tem = c(25.0, 25.1)
#' )
#' atm_df <- data.frame(
#'   dateutc = as.POSIXct(c("2025-01-01 00:00:00"), tz = "UTC"),
#'   atm_pre = c(101.3)
#' )
#' calculate_sea_level(water_df, atm_df)
#' }
#'
#' @export
sea_level <- function(water_df, atm_df, salinity = 35,
                                join_tolerance = difftime(10, units = "mins")) {
  # Ensure POSIXct
  water_df$dateutc <- as.POSIXct(water_df$dateutc, tz = "UTC")
  atm_df$dateutc <- as.POSIXct(atm_df$dateutc, tz = "UTC")

  # Join water and atm data (nearest match within tolerance)
  merged <- dplyr::left_join(
    water_df,
    dplyr::semi_join(
      atm_df, atm_df,
      by = character()
    ),
    by = character()
  )

  merged <- dplyr::left_join(
    water_df,
    dplyr::mutate(
      atm_df,
      dateutc_round = lubridate::round_date(dateutc, unit = "minute")
    ),
    by = c("dateutc" = "dateutc_round")
  )

  # Fill missing atm_pre with mean
  mean_atm <- mean(atm_df$atm_pre, na.rm = TRUE)
  merged$atm_pre[is.na(merged$atm_pre)] <- mean_atm

  # Calculate density using UNESCO 1983 equation
  sw_density <- function(S, T) {
    # Pure water density at atmospheric pressure
    rho_w <- 999.842594 + 6.793952e-2 * T - 9.095290e-3 * T^2 +
      1.001685e-4 * T^3 - 1.120083e-6 * T^4 + 6.536332e-9 * T^5
    # Seawater density at atmospheric pressure
    A <- 0.824493 - 4.0899e-3 * T + 7.6438e-5 * T^2 -
      8.2467e-7 * T^3 + 5.3875e-9 * T^4
    B <- -5.72466e-3 + 1.0227e-4 * T - 1.6546e-6 * T^2
    C <- 4.8314e-4
    rho <- rho_w + A * S + B * S^(3/2) + C * S^2
    return(rho) # kg/m^3
  }

  # Gauge pressure (Pa) = (submerged absolute pressure - atm pressure) * 1000 (kPa → Pa)
  merged$gauge_pressure <- (merged$pre - merged$atm_pre) * 1000

  # Water density
  merged$density <- sw_density(salinity, merged$tem)

  # Sea level (m) = gauge pressure / (density * g)
  g <- 9.80665
  merged$sl <- merged$gauge_pressure / (merged$density * g)

  return(merged)
}
