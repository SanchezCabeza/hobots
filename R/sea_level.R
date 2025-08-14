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
#'     \item \code{pres} (numeric): Absolute pressure measured by the submerged sensor (kPa)
#'     \item \code{tem} (numeric): Water temperature in degrees Celsius
#'   }
#' @param atm_df An (atmospheric) data frame with columns:
#'   \itemize{
#'     \item \code{dateutc} (POSIXct): Timestamp of the atmospheric measurement
#'     \item \code{pres.y} (numeric): Atmospheric pressure in kPa
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
#' # atm_df <- data.frame(dateutc = as.POSIXct(...), pres.y = ...)
#' # result <- calc_sea_level(water_df, atm_df)
#'
sea_level <- function(water_df, atm_df, salinity = 35) {

  water_df = combined.flag
  atm.df = atm_df

  # Ensure POSIXct
  water_df$dateutc <- as.POSIXct(water_df$dateutc, tz = "UTC")
  atm_df$dateutc <- as.POSIXct(atm_df$dateutc, tz = "UTC")

  # Match atmospheric pressure by exact timestamp
  idx <- match(water_df$dateutc, atm_df$dateutc)
  atm_pre <- atm_df$pres[idx]

  # Replace missing atm_pre with mean
  atm_pre[is.na(atm_pre)] <- mean(atm_df$pres, na.rm = TRUE)

  # Density function (UNESCO 1983 EOS-80)
  sw_density <- function(S, T) {
    a0 <- 999.842594; a1 <- 6.793952e-2; a2 <- -9.095290e-3
    a3 <- 1.001685e-4; a4 <- -1.120083e-6; a5 <- 6.536332e-9
    rho_w <- a0 + (a1 + (a2 + (a3 + (a4 + a5 * T) * T) * T) * T) * T
    b0 <- 0.824493; b1 <- -4.0899e-3; b2 <- 7.6438e-5
    b3 <- -8.2467e-7; b4 <- 5.3875e-9
    c0 <- -5.72466e-3; c1 <- 1.0227e-4; c2 <- -1.6546e-6
    d0 <- 4.8314e-4
    rho_sw <- rho_w + (b0 + (b1 + (b2 + (b3 + b4*T)*T)*T)*T)*S +
      (c0 + (c1 + c2*T)*T)*S^(3/2) + d0*S^2
    return(rho_sw)
  }

  # Compute density for each row
  rho <- sw_density(salinity, water_df$tem)

  # Compute sea level (m)
  g <- 9.80665
  water_df$sl <- ((water_df$pres - atm_pre) * 1000 ) / (rho * g)

  return(water_df)
}
