#' Image Plot
#'
#' This function will create an image plot.
#'
#' @param main_plot The main plot as a ggplot object. It must contain a smoothing function as returned by geom_smooth() or stat_smooth().
#' @param breaks Either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which y axis is to be cut. This will create the intervals which the images will be defined across.
#' @param labels The image levels of the resulting breaks intervals. Needs to be an integer vector with values 0 - 10 generally specifying lowest to highest (strength, hydration, etc).
#' @param img_type The type of image plot you need. A character string of either "strength", "hydration"
#' @param n_img An integer of the number of images you want below the x-axis. Defaults to length(x-axis) / 20.
#' @param img_positions Defines the image placement under the x-axis. A character string of either "uniform", "peaks", or a user supplied integer vector of x-axis indices. Defaults to "uniform".
#' @param peaks_adjust An integer to adjust the window width for the argument setting: img_positions = "peaks". Positive values increase the window width (less sensitive to local minima and maxima). Negative values decrease the window width (more sensitive to local minima and maxima).
#' @param img_range An integer to adjust the range of values that are used to calculate the mean y-value and it's corresponding image (based on the fitted smooth curve). For example, the "uniform" img_positions method defaults to using all y-values in each y_interval, the "peaks" img_positions method defaults to a range of 0, and supplying your own img_positions values will default to a range of 5.
#' @param img_height An integer setting the number of rows (height) for the image plots. "strength" defaults to 1 and "hydration" defaults to 2.
#' @param panel \code{TRUE} or \code{FALSE}: flag to keep or remove the main plot panel. If removed you will only see the image plots and x-axis. The default value is TRUE.
#'
#' @return Returns an image plot grob as a ggplot2 grob object.
#'
#' @importFrom grid rasterGrob
#' @importFrom png readPNG
#' @importFrom gtable gtable_filter gtable_add_cols
#' @importFrom magrittr "%>%"
#' @importFrom zoo as.zoo rollapply
#' @importFrom gridExtra grid.arrange
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # img_plot example
#' #----------------------------------------------------------------------------
#' n <- 100
#' data <- data.frame(
#'   x1 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE),
#'   x2 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE),
#'   y = sort(rnorm(n)),
#'   stringsAsFactors = FALSE
#' )
#'
#' main_plot <- ggplot(data, aes(1:length(x1), y)) +
#'   geom_line() +
#'   scale_x_continuous(
#'     name = "Pairwise Sequence Alignment",
#'     breaks = 1:length(data$x1),
#'     labels = paste0(data$x1, "\n", data$x2)
#'   )
#'
#' img_type <- "strength"
#'}
#'

img_plot <- function(
  main_plot,
  breaks = 11,
  labels = 0:10,
  img_type,
  n_img = NULL,
  img_positions = "uniform",
  peaks_adjust = 0,
  img_range = NULL,
  img_height = NULL,
  panel = TRUE
) {
  #-----------------------------------------------------------------------------
  # Check arguments
  #-----------------------------------------------------------------------------
  if(!("ggplot" %in% class(main_plot))) {
    stop("You did not pass a ggplot object to the main_plot argument")
  }
  if(!is.numeric(breaks)) {
    stop(paste0("Check the breaks argument. It must be a numeric vector."))
  }
  if(!(all(labels) %in% 0:10)) {
    stop("Check the levels argument. It must be a vector with values between 0 and 10.")
  }
  if(!(img_type %in% c("strength", "hydration"))) {
    stop(paste0("Check the img_type argument. The '", img_type, "' image is not supported by this package."))
  }
  if(!is.null(n_img)) {
    if(!is.numeric(n_img)) {
      stop(paste0("Check the n_img argument. The value '", n_img, "' doesn't look like an integer."))
    } else {
      n_img <- as.integer(n_img)
    }
  }
  if(is.character(img_positions) & all(!(img_positions %in% c("uniform", "peaks")))) {
    stop(paste0("Check the img_positions argument. The '", img_positions, "' img_positions method is not supported by this package."))
  }
  if(!is.character(img_positions) & !is.numeric(img_positions)) {
    stop(paste0("Check the img_positions argument. If you are supplying x-axis indices, please make sure they are an integer vector."))
  }
  if(peaks_adjust != 0) {
    if(!is.numeric(peaks_adjust)) {
      stop(paste0("Check the peaks_adjust argument. The value '", peaks_adjust, "' doesn't look like an integer."))
    } else {
      peaks_adjust <- as.integer(peaks_adjust)
    }
  }
  if(!is.null(img_range)) {
    if(!is.numeric(img_range)) {
      stop(paste0("Check the img_range argument. The value '", img_range, "' doesn't look like an integer."))
    } else {
      img_range <- as.integer(img_range)
    }
  }
  if(!is.null(img_height)) {
    if(!is.numeric(img_height)) {
      stop(paste0("Check the img_height argument. The value '", img_height, "' doesn't look like an integer."))
    }
  }
  if(!is.null(panel)) {
    if(!is.logical(panel)) {
      stop(paste0("Check the panel argument. The value '", panel, "' doesn't look like a TRUE or FALSE."))
    }
  }

  #-----------------------------------------------------------------------------
  # Prepare images
  #-----------------------------------------------------------------------------
  imgs_dir <- system.file(
    paste0("imgs/", img_type),
    package="imgplot",
    mustWork = TRUE
  )
  img_paths <- list.files(
    imgs_dir,
    full.names = TRUE
  )
  img_grobs <- lapply(img_paths, function(x) {
    grid::rasterGrob(png::readPNG(x, TRUE), interpolate=TRUE)
  })
  # This depends on being read in 0...10. May want a better method.
  names(img_grobs) <- 0:10

  #-----------------------------------------------------------------------------
  # Extract data
  #-----------------------------------------------------------------------------
  # Original data
  x <- ggplot_build(main_plot)$data[[1]][["x"]]
  y <- ggplot_build(main_plot)$data[[1]][["y"]]

  # Smoothed data
  xs <- ggplot_build(main_plot)$data[[2]][["x"]]
  ys <- ggplot_build(main_plot)$data[[2]][["y"]]

  # Smoothed interpolated data
  # User really should call geom_smooth(n = length(x)) in main_plot to avoid this
  if(length(xs) < length(x)) {
    model <- loess(ys ~ xs, span = 0.1)
    xsi <- x
    ysi <- predict(object = model, newdata = xsi)
    # overwrite xs and ys for now
    xs <- xsi
    ys <- ysi
  }

  # A guess for n_img
  if(is.null(n_img)) {
    n_img <- ceiling(length(x) / 15)
  }

  #-----------------------------------------------------------------------------
  # Assign an image level to each ys value
  #-----------------------------------------------------------------------------
  y_img_cut <- y_cut(y, breaks = breaks, labels = labels)
  y_img_cut$breaks[1] <- -Inf
  y_img_cut$breaks[length(y_img_cut$breaks)] <- Inf
  ys_img_levels <- cut(
    ys,
    breaks = y_img_cut$breaks,
    right = FALSE,
    include.lowest = TRUE,
    labels = y_img_cut$labels
  )
  ys_img_levels <- as.integer(as.character(ys_img_levels))

  #-----------------------------------------------------------------------------
  # Determine where to place images on x-axis
  #-----------------------------------------------------------------------------
  # Fit uniformly across the x-axis
  #----------------------------------------------------------
  if(all(img_positions == "uniform")) {
    x_intervals <- cut(x, n_img)
    x_range <- x_pos(x_intervals)
    # temp fix non-uniform ranges
    diff_x <- x_range[,2] - x_range[,1]
    if(abs(max(diff_x) - min(diff_x)) > 0.5) {
      idx <- which(diff_x == min(diff_x))
      x_range[idx, 2] <- x_range[idx, 2] + 0.5
      x_range[idx, 1] <- x_range[idx, 1] - 0.5
    }
    # recalculate intervals based on img_range
    if(!is.null(img_range)) {
      x_range <- x_pos_adjusted(x_range, img_range)
      x_intervals <- cut(x, breaks = c(x_range[, 1], x_range[, 2]))
      levels(x_intervals)[seq(from = 2, to = length(levels(x_intervals)), by = 2)] <- NA
    }
    img_levels <- round(tapply(ys_img_levels, x_intervals, FUN = mean))
  }

  # Fit peaks across the x-axis
  #-------------------------------------------------
  if(all(img_positions == "peaks")) {
    # find local max and mins of smoothed data
    window <- ifelse((length(x) / n_img) %% 2 == 1, length(x) / n_img, (length(x) / n_img) - 1)
    window <- round(window + peaks_adjust) # modify the width argument for better handling of cases with slight differences between neighboring values.
    which_max <- ceiling(window/2) # the middle element / index
    ys_lmax_idx <- zoo::rollapply(zoo::as.zoo(ys), window, function(x) which.max(x) == which_max)
    ys_lmax_idx <- zoo::index(ys_lmax_idx)[zoo::coredata(ys_lmax_idx)]
    ys_lmin_idx <- zoo::rollapply(zoo::as.zoo(ys), window, function(x) which.min(x) == which_max)
    ys_lmin_idx <- zoo::index(ys_lmin_idx)[zoo::coredata(ys_lmin_idx)]

    n_locals <- length(ys_lmax_idx) + length(ys_lmin_idx)

    if (n_locals > n_img) {
      stop("More maxima and minima than number of specified images. Try modifying the window size or number of images.")
    }

    # find max and min of smoothed data. Maybe they weren't in a maxima/minima,
    # but probably important to include.
    ys_max_idx <- which(ys == max(ys))
    ys_min_idx <- which(ys == min(ys))

    # are the start and end or any other midpoints able to be fit around the peaks?
    ys_peaks_idx <- sort(unique(c(ys_lmax_idx, ys_lmin_idx, ys_max_idx, ys_min_idx)))
    if(length(ys_peaks_idx) < n_img) {
      # check starting position of x-axis
      if(ys_peaks_idx[1] > (window / 2)) {
        ys_peaks_idx <- unique(c(1, ys_peaks_idx))
      }
      # check ending position of x-axis
      if((length(ys) - ys_peaks_idx[length(ys_peaks_idx)]) > (window / 2)) {
        ys_peaks_idx <- unique(c(ys_peaks_idx, length(ys)))
      }
    }
    gap_sizes <- data.frame(NA)
    i <- 1
    while(length(ys_peaks_idx) < n_img) {
      gap_sizes <- data.frame(
        position = 1:length(diff(ys_peaks_idx)),
        gap_size = diff(ys_peaks_idx)
      )
      gap_sizes <- gap_sizes[order(-gap_sizes$gap_size), ]
      idx <- round(ys_peaks_idx[gap_sizes[1,1]] + (gap_sizes[1,2] / 2))
      ys_peaks_idx <- append(ys_peaks_idx, idx, after = gap_sizes[1,1])
      i <- i + 1
    }

    # what images and where to place them
    x_range <- x_pos(round(xs)[ys_peaks_idx])
    n_img <- nrow(x_range)
    if(!is.null(img_range)) {
      # are the start and end x values included? if so, just set them aside and
      # still use the exast ys level for them.
      x_range_adjusted <- x_range
      if(1 %in% ys_peaks_idx) {
        x_range_adjusted <- x_range[2:nrow(x_range), ]
      }
      if(length(x) %in% ys_peaks_idx) {
        x_range_adjusted <- x_range_adjusted[1:(nrow(x_range_adjusted) - 1), ]
      }
      x_range_adjusted <- x_pos_adjusted(x_range_adjusted, img_range)
      x_intervals <- cut(x, breaks = c(x_range_adjusted[, 1], x_range_adjusted[, 2]))
      levels(x_intervals)[seq(from = 2, to = length(levels(x_intervals)), by = 2)] <- NA
      img_levels <- round(tapply(ys_img_levels, x_intervals, FUN = mean))
      if(1 %in% ys_peaks_idx) {
        img_levels <- append(img_levels, ys_img_levels[1], after = 0)
      }
      if(length(x) %in% ys_peaks_idx) {
        img_levels <- append(img_levels, ys_img_levels[length(x)], after = length(img_levels))
      }
    } else{
      img_levels <- ys_img_levels[ys_peaks_idx]
    }
  }

  # User supplied x-axis fit
  #---------------------------------
  if(is.vector(img_positions) & is.numeric(img_positions)) {
    x_range <- x_pos(img_positions)
    n_img <- length(img_positions)

    if(is.null(img_range)) {
      img_range <- 5
    }
    # are the start and end x values included? if so, just set them aside and
    # use the exast ys level for them (i.e. not the mean y value)
    x_range_adjusted <- x_range
    if(1 %in% img_positions) {
      x_range_adjusted <- x_range[2:nrow(x_range), ]
    }
    if(length(x) %in% img_positions) {
      x_range_adjusted <- x_range_adjusted[1:(nrow(x_range_adjusted) - 1), ]
    }
    x_range_adjusted <- x_pos_adjusted(x_range_adjusted, img_range)
    x_intervals <- cut(x, breaks = c(x_range_adjusted[, 1], x_range_adjusted[, 2]))
    levels(x_intervals)[seq(from = 2, to = length(levels(x_intervals)), by = 2)] <- NA
    img_levels <- round(tapply(ys_img_levels, x_intervals, FUN = mean))
    if(1 %in% img_positions) {
      img_levels <- append(img_levels, ys_img_levels[1], after = 0)
    }
    if(length(x) %in% img_positions) {
      img_levels <- append(img_levels, ys_img_levels[length(x)], after = length(img_levels))
    }
  }

  #-------------------------------------------------------------------------------
  # Create image plot and grob
  #-------------------------------------------------------------------------------
  # create image plot
  img_plot <- create_img_plot(main_plot, img_grobs, img_levels, x_range)

  # should I overwrite the main_plot panel?

  # Add columns to panel only grob
  img_plot_grob <- gtable::gtable_filter(ggplotGrob(img_plot), "panel", trim = TRUE)
  img_plot_grob <- gtable::gtable_add_cols(x = img_plot_grob, widths = unit(0, "cm"), pos = 0)
  img_plot_grob <- gtable::gtable_add_cols(x = img_plot_grob, widths = unit(0, "cm"), pos = 0)
  img_plot_grob <- gtable::gtable_add_cols(x = img_plot_grob, widths = unit(0, "cm"), pos = 0)
  img_plot_grob <- gtable::gtable_add_cols(x = img_plot_grob, widths = unit(0, "cm"))
  img_plot_grob <- gtable::gtable_add_cols(x = img_plot_grob, widths = unit(0, "cm"))
  img_plot_grob <- gtable::gtable_add_cols(x = img_plot_grob, widths = unit(0, "cm"))
  # get max widths of base plot vs manogram plot
  main_plot_grob <- ggplotGrob(main_plot)
  widths <- grid::unit.pmax(main_plot_grob$widths, img_plot_grob$widths)
  img_plot_grob$widths <- as.list(widths)

  #-------------------------------------------------------------------------------
  # Return combined image plot
  #-------------------------------------------------------------------------------
  if(is.null(img_height) & img_type == "strength") {
    img_height <- 1
  }
  if(is.null(img_height) & img_type == "hydration") {
    img_height <- 2
  }

  if(panel == TRUE) {
    gridExtra::grid.arrange(
      main_plot_grob,
      img_plot_grob,
      ncol = 1,
      heights = c(unit(x = 7, units = "null"), unit(x = img_height, units = "null"))
    )
  } else {
    img_grob <- main_plot_grob %>%
      gtable::gtable_add_rows(unit(x = img_height/7, units = "null")) %>%
      gtable::gtable_add_grob(
        img_plot_grob,
        t = nrow(main_plot_grob) + 1,
        l = 1,
        r = ncol(main_plot_grob)
      )
    # use img_plot for x axis without ticks
    gridExtra::grid.arrange(
      gtable::gtable_filter(img_grob, "axis-b", trim = TRUE),
      gtable::gtable_filter(img_plot_grob, "panel", trim = TRUE),
      ncol = 1
    )
  }
}
