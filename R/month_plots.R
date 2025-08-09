month_plots <- function(file, column = "tem", points_per_day = 48,
                        input_dir = ".", output_dir = ".") {
  # Read csv
  data <- read.csv(file, stringsAsFactors = FALSE)

  # Validate column
  if (is.numeric(column)) {
    if (column > ncol(data) || column <= 0) stop("Invalid column index.")
    col_data <- data[[column]]
  } else if (is.character(column)) {
    if (!(column %in% names(data))) stop("Invalid column name.")
    col_data <- data[[column]]
  }

  n_points_month <- points_per_day * 28
  total_points <- length(col_data)

  # Prepare output dir
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # cm to inches
  cm_to_in <- function(cm) cm / 2.54

  # Loop over chunks of 28 days
  chunk <- 1
  for (start_idx in seq(1, total_points, by = n_points_month)) {
    end_idx <- min(start_idx + n_points_month - 1, total_points)
    idx <- start_idx:end_idx

    # Time axis in days from start of chunk
    time_axis <- seq_along(idx) / points_per_day

    # Output file name
    base_name <- sub("\\.csv$", "", basename(file))
    plot_name <- sprintf("%s_month%03d.png", base_name, chunk)
    plot_path <- file.path(output_dir, plot_name)

    png(filename = plot_path,
        width = cm_to_in(15), height = cm_to_in(10), units = "in", res = 300)

    plot(time_axis, col_data[idx],
         type = "p", pch = 16, col = "blue",
         xlab = "Days", ylab = column,
         main = paste("28-day period", chunk, "-", base_name))

    dev.off()
    message("Saved: ", plot_path)

    chunk <- chunk + 1
  }
}

