#' Plot fixed-size chunks from time series data
#'
#' This function reads a CSV file containing time series data, where the first
#' column must be \code{dateutc} in the format \code{"yyyy/mm/dd hh:mm:ss"}.
#' It splits the data into consecutive chunks of \code{npoints} rows and
#' generates a PNG plot for each chunk. The x-axis always spans the same number
#' of points, so the last chunk will have empty space if there are not enough
#' rows to fill it.
#'
#' The PNG files are saved in \code{output_dir} with names formed from the base
#' name of the input file plus \code{"_chunkXX.png"}.
#'
#' @param file Path to the CSV file to read.
#' @param column Column index or name of the variable to plot (default: 2).
#' @param output_dir Directory where PNG plots will be saved
#'   (default: \code{"../plots"}). Created if it does not exist.
#' @param npoints Number of data points per plot
#'   (default: \code{28 * 24 * 2} for one month at 30-min intervals).
#' @param width_cm Width of the output PNG in centimeters (default: 15).
#' @param height_cm Height of the output PNG in centimeters (default: 10).
#' @param res Resolution of output PNG in dots per inch (default: 100).
#' @export
#'
#' @examples
#' \dontrun{
#' plot_monthly_chunks("data.csv", column = 2)
#' plot_monthly_chunks("data.csv", column = "tem", npoints = 1000)
#' }
month_plots <- function(file, column = 2,
                        output_dir = "../plots",
                        npoints = 28 * 24 * 2,   # default: one month at 30-min intervals
                        width_cm = 15, height_cm = 10, res = 100) {

  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Read CSV
  data <- read.csv(file, stringsAsFactors = FALSE)

  # First column must be dateutc in yyyy/mm/dd hh:mm:ss
  date_col <- names(data)[1]
  data[[date_col]] <- trimws(data[[date_col]])
  data[[date_col]] <- as.POSIXct(data[[date_col]], format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

  if (any(is.na(data[[date_col]]))) {
    stop("Error: dateutc column contains invalid values or wrong format (yyyy/mm/dd hh:mm:ss)")
  }

  # Select target column
  if (is.character(column)) {
    if (!(column %in% names(data))) stop(paste("Column", column, "not found"))
    col_index <- which(names(data) == column)
  } else if (is.numeric(column)) {
    if (column < 1 || column > ncol(data)) stop("Column index out of range")
    col_index <- column
  } else {
    stop("Column must be index (numeric) or name (character)")
  }

  # Base name for output files
  file_base <- tools::file_path_sans_ext(basename(file))

  total_points <- nrow(data)
  nchunks <- ceiling(total_points / npoints)

  for (i in 1:nchunks) {
    cat(" chunk ", i)
    start_idx <- (i - 1) * npoints + 1
    end_idx   <- min(i * npoints, total_points)

    chunk_data <- data[start_idx:end_idx, ]

    # Remove NA in the target column
    y <- chunk_data[[column]]
    valid_idx <- !is.na(y)

    if (sum(valid_idx) == 0) {
      message("Skipping chunk ", i, ": no valid data.")
      next
    }

    png_file <- file.path(output_dir, sprintf("%s_month%02d.png", file_base, i))
    png(filename = png_file,
        width = width_cm, height = height_cm, units = "cm", res = res)

    plot(chunk_data$dateutc[valid_idx], y[valid_idx],
         type = "l", main = paste(basename(file), "-", i),
         xlab = "Date", ylab = names(data)[column])

    # # Create empty plot for full npoints range (even if last chunk shorter)
    # plot(data[[date_col]][start_idx:(start_idx + npoints - 1)],
    #      rep(NA, npoints), type = "n",
    #      xlab = "Date", ylab = names(data)[col_index],
    #      main = sprintf("%s - %d", file_base, i))
    #
    # # Draw the data that exists
    # lines(chunk_data[[date_col]], chunk_data[[col_index]], col = "blue", lwd = 1)
    #
    dev.off()
  }
}

