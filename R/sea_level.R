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
#' @examples
#' # water_df <- data.frame(dateutc = as.POSIXct(...), pre = ..., tem = ...)
#' # atm_df <- data.frame(dateutc = as.POSIXct(...), atm_pre = ...)
#' # result <- calc_sea_level(water_df, atm_df)
#'
calc_sea_level <- function(water_df, atm_df, salinity = 35) {

  # Merge water and atmospheric data by exact timestamp
  merged_df <- merge(water_df, atm_df, by = "dateutc", all.x = TRUE)

  # If atmospheric pressure is missing, replace with mean atm pressure
  if (any(is.na(merged_df$atm_pre))) {
    mean_atm <- mean(merged_df$atm_pre, na.rm = TRUE)
    merged_df$atm_pre[is.na(merged_df$atm_pre)] <- mean_atm
  }

  # Calculate density using UNESCO 1983 (EOS-80) equation
  sw_density <- function(S, T) {
    # Density of pure water at atmospheric pressure
    a0 <- 999.842594
    a1 <- 6.793952e-2
    a2 <- -9.095290e-3
    a3 <- 1.001685e-4
    a4 <- -1.120083e-6
    a5 <- 6.536332e-9
    rho_w <- a0 + (a1 + (a2 + (a3 + (a4 + a5 * T) * T) * T) * T) * T

    # Seawater density at atmospheric pressure
    b0 <- 0.824493
    b1 <- -4.0899e-3
    b2 <- 7.6438e-5
    b3 <- -8.2467e-7
    b4 <- 5.3875e-9
    c0 <- -5.72466e-3
    c1 <- 1.0227e-4
    c2 <- -1.6546e-6
    d0 <- 4.8314e-4

    rho_sw <- rho_w +
      (b0 + (b1 + (b2 + (b3 + b4 * T) * T) * T) * T) * S +
      (c0 + (c1 + c2 * T) * T) * S^(3/2) +
      d0 * S^2

    return(rho_sw) # kg/m^3
  }

  # Calculate density for each row
  rho <- sw_density(salinity, merged_df$tem)

  # Convert kPa to m of water: 1 kPa = 1000 N/m²
  # Water column height h = (P_sub - P_atm) / (rho * g)
  g <- 9.80665
  merged_df$sl <- ( (merged_df$pre - merged_df$atm_pre) * 1000 ) / (rho * g)

  return(merged_df)
}

