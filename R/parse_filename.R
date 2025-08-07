#' parse_filename.R
#' jasc20250806
#' Parse Observatory (e.g., Hobo) File Names
#'
#' Extracts components from file names following the pattern
#' observatory.stationNumber.depth.variables.dateStart.dateEnd.csv
#'
#' @param fileName A character string specifying the file name to parse. The file fist column must be a DateTime stamp.
#' Function and/or result parameters are:
#' \itemize{
#'   \item \code{fileName}: A hobo-derived (.csv) file name ID (e.g., "mzt.5.boya24.20220202.csv").
#'   \item \code{observatory}: a string identifier (e.g., "mzt", "cme", "pmo", "samo").
#'   \item \code{stationNumber}: station number (e.g., 1 to 11 for Mazatlan).
#'   \item \code{depth}: A string indicating the measurement depth (e.g., "1m").
#'   \item \code{variables}: A string representing measured variables (e.g., "ot" for oxygen and temperature).
#'   \item \code{dateStart}: start date (e.g., "20200101").
#'   \item \code{dateEnd}: end date (e.g., "20200202").
#'   \item \code{newName}: string, example: "mzt.1.2m.ot.20200101.20200202.csv"
#'   }
#' @return newName (string).
#' }
#' @export
# @examples parse_filename("mzt.boya24.20220202.csv", stationNumber = 5, variables = "ot")
parse_filename <- function(fileName, observatory = "mzt", stationNumber = 5,
                           depth = "1m", variables = "t") {
  # read data
  data  <- read.csv(fileName, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  dates <- data[ , 1]

  # parse dates in YYYY-MM-DD HH:MM:SS format
  dates2 <- as.POSIXct(dates, format = "%Y/%m/%d %H:%M:%S")
  if (all(is.na(dates2))) stop(paste("Failed to parse all dates in", file))

  # Get min and max dates, format as YYYYMMDD
  min_date <- format(min(dates2, na.rm = TRUE), "%Y%m%d")
  max_date <- format(max(dates2, na.rm = TRUE), "%Y%m%d")

  # Construct and return standard filename
  new_name <- paste0(observatory, ".", stationNumber, ".", depth, ".", variables, ".",
                     min_date, ".", max_date, ".csv")
}
