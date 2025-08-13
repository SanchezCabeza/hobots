#' Calculate oxygen saturation concentration and percent saturation
#'
#' Computes oxygen saturation concentration (mg/L) using the Weiss (1970) formula
#' and percent saturation based on measured dissolved oxygen, temperature, and salinity.
#' Assumes sea level pressure (1013.25 hPa) unless specified.
#'
#' Weiss, R. F. (1970).
#' The solubility of nitrogen, oxygen, and argon in water and seawater.
#' Deep Sea Research and Oceanographic Abstracts, 17(4), 721–735.
#' https://doi.org/10.1016/0011-7471(70)90037-9
#'
#' @param temp Temperature in degrees Celsius.
#' @param do_measured Measured dissolved oxygen in mg/L.
#' @param salinity Salinity in psu (default: 35 for Mazatlan seawater).
#' @param pressure Atmospheric pressure in hPa (default: 1013.25 for sea level).
#' @return A list with saturation concentration (mg/L) and percent saturation.
#' @references Weiss, R. F. (1970). The solubility of nitrogen, oxygen and argon in water and seawater.
#' Deep Sea Research and Oceanographic Abstracts, 17(4), 721-735.
#' @examples
#' \dontrun{
#' # Calculate for a single measurement
#' result <- calc_oxygen_saturation(temp = 25, do_measured = 5.0, salinity = 35)
#' print(result)
#'
#' # Apply to a data frame
#' df <- data.frame(dateutc = "2020/01/01 00:00:00", oxy = 5.0, tem = 25.0)
#' df$do_saturation <- sapply(1:nrow(df), function(i) {
#'   calc_oxygen_saturation(df$tem[i], df$oxy[i], salinity = 35)$percent_saturation
#' })
#' }
oxygen_saturation <- function(temp, do_measured, salinity = 35, pressure = 1013.25) {
  # Constants from Weiss (1970)
  A1 <- -173.4292
  A2 <-  249.6339
  A3 <-  143.3483
  A4 <- -21.8492
  B1 <- -0.033096
  B2 <-  0.014259
  B3 <- -0.001700

  # Convert temperature to Kelvin
  T <- temp + 273.15

  # Calculate saturation concentration (mg/L)
  ln_Cs <- A1 + A2 * (100 / T) + A3 * log(T / 100) + A4 * (T / 100) +
    salinity * (B1 + B2 * (T / 100) + B3 * (T / 100)^2)
  Cs <- exp(ln_Cs)

  # Adjust for pressure (if not standard)
  P0 <- 1013.25  # Standard pressure in hPa
  Cs <- Cs * (pressure / P0)

  # Calculate percent saturation
  percent_saturation <- (do_measured / Cs) * 100

  # Return results
  return(percent_saturation)
}
