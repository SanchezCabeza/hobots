#' parse_filename.R
# jasc20250806
# 'Parse file names of observatory time series (e.g., Hobo .csv files).
#' Creates an homogeneous file name following the pattern:
#' #' observatory.stationNumber.depth.variables.dateStart.dateEnd.csv
#'
#' @param fileName A character string specifying the CSV file name to parse. The file's first column must be a DateTime stamp in "YYYY/MM/DD HH:MM:SS" format.
#' @param observatory A string identifier for the observatory (e.g., "mzt", "cme").
#' @param stationNumber Station number (e.g., 1 to 11 for Mazatlan).
#' @param depth Measurement depth as string (e.g., "1m").
#' @param variables String representing measured variables (e.g., "ot" for oxygen and temperature).
#'
#' @return A character string with the standardized file name including date start and end.
#' @export
#' @examples
#' \dontrun{
#' parse_filename("mzt.boya24.20220202.csv", stationNumber = 5, variables = "ot")
#' }
parse_filename <- function(fileName, observatory = "mzt", stationNumber = 5,
                           depth = "1m", variables = "t") {
  if (!file.exists(fileName)) {
    stop(paste("File does not exist:", fileName))
  }
  # read data
  data  <- read.csv(fileName, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  dates <- data[ , 1]

  # parse dates in YYYY-MM-DD HH:MM:SS format
  dates2 <- as.POSIXct(dates, format = "%Y/%m/%d %H:%M:%S")
  if (all(is.na(dates2))) stop(paste("Failed to parse all dates in", fileName))

  # Get min and max dates, format as YYYYMMDD
  min_date <- format(min(dates2, na.rm = TRUE), "%Y%m%d")
  max_date <- format(max(dates2, na.rm = TRUE), "%Y%m%d")

  # Construct and return standard filename
  new_name <- paste0(observatory, ".", stationNumber, ".", depth, ".", variables, ".",
                     min_date, ".", max_date, ".csv")
  return(new_name)
}
