#' Calculate oxygen saturation concentration and percent saturation at depth
#'
#' Computes oxygen saturation concentration (mg/L) using the Weiss (1970) formula,
#' adjusted for pressure at depth, and percent saturation based on measured dissolved
#' oxygen, temperature, salinity, and depth. Assumes sea level atmospheric pressure
#' (1013.25 hPa) unless specified.
#'
#' @param temp Temperature in degrees Celsius (numeric or vector).
#' @param do_measured Measured dissolved oxygen in mg/L (numeric or vector).
#' @param salinity Salinity in psu (default: 35 for Mazatlan seawater, numeric or vector).
#' @param depth Depth in meters (default: 0 for sea level, numeric or vector).
#' @param atm_pressure Atmospheric pressure in hPa (default: 1013.25 for sea level).
#' @return A data frame with saturation concentration (mg/L) and percent saturation.
#' @references Weiss, R. F. (1970). The solubility of nitrogen, oxygen and argon in water and seawater.
#' Deep Sea Research and Oceanographic Abstracts, 17(4), 721-735.
#' @examples
#' \dontrun{
#' # Calculate for a single measurement at 20m depth
#' result <- calc_oxygen_saturation(temp = 25, do_measured = 5.0, salinity = 35, depth = 20)
#' print(result)
#'
#' # Apply to a data frame
#' df <- data.frame(dateutc = c("2020/01/01 00:00:00", "2020/01/01 00:30:00"),
#'                  oxy = c(5.0, 4.8), tem = c(25.0, 24.5), depth = c(0, 20))
#' result <- calc_oxygen_saturation(temp = df$tem, do_measured = df$oxy, salinity = 35, depth = df$depth)
#' df <- cbind(df, result)
#' }
#' @export
oxygen_saturation <- function(temp, do_measured, salinity = 35, depth = 0, atm_pressure = 1013.25) {
  # Input validation
  if (length(temp) != length(do_measured)) stop("temp and do_measured must have the same length")
  if (length(depth) == 1) depth <- rep(depth, length(temp))
  if (length(salinity) == 1) salinity <- rep(salinity, length(temp))
  if (length(depth) != length(temp) || length(salinity) != length(temp)) {
    stop("depth and salinity must be scalar or match length of temp")
  }

  # Constants from Weiss (1970)
  A1 <- -173.4292
  A2 <- 249.6339
  A3 <- 143.3483
  A4 <- -21.8492
  B1 <- -0.033096
  B2 <- 0.014259
  B3 <- -0.001700

  # Convert temperature to Kelvin
  T <- temp + 273.15

  # Calculate saturation concentration (mg/L) at sea level
  ln_Cs <- A1 + A2 * (100 / T) + A3 * log(T / 100) + A4 * (T / 100) +
    salinity * (B1 + B2 * (T / 100) + B3 * (T / 100)^2)
  Cs <- exp(ln_Cs)

  # Adjust for pressure at depth
  rho <- 1025  # Seawater density (kg/m^3, approximate for 35 psu)
  g <- 9.81    # Gravity (m/s^2)
  P0 <- 1013.25  # Standard pressure (hPa)
  pressure_total <- atm_pressure + (rho * g * depth / 100)  # Convert Pa to hPa
  Cs_adjusted <- Cs * (pressure_total / P0)

  # Calculate percent saturation
  percent_saturation <- (do_measured / Cs_adjusted) * 100

  # Return results as data frame
  return(percent_saturation)
}
