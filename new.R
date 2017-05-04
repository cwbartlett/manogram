library(ggplot2)
#-------------------------------------------------------------------------------
# prepare test data
#-------------------------------------------------------------------------------
set.seed(1)
n <- 200
x1 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE)
x2 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE)
y <- sin(2 * pi * seq_along(x1) / n) + runif(length(x1), -1, 1)
data <- data.frame(
  a = x1,
  c = x2,
  b = y,
  stringsAsFactors = FALSE
)
main_plot <- ggplot(data, aes(1:length(x1), b)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.7) +
  scale_x_continuous(
    name = "Pairwise Sequence Alignment",
    breaks = 1:length(data$a),
    labels = paste0(data$a, "\n", data$c)
  )
main_plot2 <- ggplot(data, aes(1:length(x1), b)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.7)
main_plot2 <- ggplot(data, aes(1:length(x1), b)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.7)
main_plot3 <- ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

main_plot = main_plot
y_intervals = NULL
img_type = "strength"
n_img = 10
img_positions = "uniform"
peaks_adjust = 0
img_range = NULL
img_height = NULL



################################################################################
#-----------------------------------------------------------------------------
# Check arguments
#-----------------------------------------------------------------------------
if(!("ggplot" %in% class(main_plot))) {
  stop("You did not pass a ggplot object to the main_plot argument")
}
if(!(img_type %in% c("strength", "hydration"))) {
  stop(paste0("Check the img_type argument. The '", img_type, "' image is not supported by this package."))
}
if(!is.null(y_intervals)) {
  if(!is.factor(y_intervals)) {
    stop("Check the y_intervals argument. It needs to be a factor and is generally created with the cut() function.")
  }
  if(!all(levels(y_intervals) %in% 0:10)) {
    stop("Check the y_intervals argument. The factor levels need to be characters between 0 and 10")
  }
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
# Extract and create data
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
  n_img <- ceiling(length(x) / 20)
}

#-----------------------------------------------------------------------------
# Assign an image level to each value of ys
#-----------------------------------------------------------------------------
if(is.null(y_intervals)) { # uniform y_intervals
  y_img_cut <- img_cut(y, 11, labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
} else { # user defined y_intervals
  y_img_cut <- y_intervals
}
intrvl_df <- unique(y_img_cut)[order(unique(y_img_cut)$intervals), ]
ys_img_levels <- cut(
  ys,
  breaks = unique(
    c(
      as.numeric(sub("\\((.+),.*", "\\1", intrvl_df$intervals)),
      as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", intrvl_df$intervals))
    )
  ),
  right = FALSE,
  include.lowest = TRUE,
  labels = intrvl_df$levels
)
ys_img_levels <- as.integer(as.character(ys_img_levels))

#-----------------------------------------------------------------------------
# Determine where to place images on x-axis
#-----------------------------------------------------------------------------
# utility function to calculate the the lower and upper values for each img
# position on the x-axis
x_pos <- function(img_positions) {
  if(is.factor(img_positions)) { # if feeding in values from cut() function
    x_index_lower <- as.numeric(sub("\\((.+),.*", "\\1", levels(img_positions)))
    x_index_upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", levels(img_positions)))
  } else { # if feeding in numeric position values
    x_index_lower <- img_positions - min(diff(img_positions))
    x_index_upper <- img_positions + min(diff(img_positions))
  }
  cbind(
    lower = round(x_index_lower),
    upper = round(x_index_upper)
  )
}

x_pos_adjusted <- function(x_range, img_range) {
  x_index_lower <- round(apply(x_range, 1, function(x) {median(x)}) - img_range) - 0.1
  x_index_upper <- round(apply(x_range, 1, function(x) {median(x)}) + img_range)
  cbind(
    lower = x_index_lower,
    upper = round(x_index_upper)
  )
}

# Fit uniform based y smoothed values on the x-axis indices
#----------------------------------------------------------
if(all(img_positions == "uniform")) {
  x_intervals <- cut(x, n_img)
  x_range <- x_pos(x_intervals)
  # recalculate intervals based on img_range
  if(!is.null(img_range)) {
    x_range <- x_pos_adjusted(x_range, img_range)
    x_intervals <- cut(x, breaks = c(x_range[, 1], x_range[, 2]))
    levels(x_intervals)[seq(from = 2, to = length(levels(x_intervals)), by = 2)] <- NA
  }
  img_levels <- round(tapply(ys_img_levels, x_intervals, FUN = mean))
}

# Fit peak y smoothed values on the x-axis indices
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

# User supplied x-axis indices fit
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
# helper function to create image plot
create_img_plot <- function(main_plot, img_levels, x_range) {
  data <- data.frame(
    x = with(main_plot$data, eval(main_plot$mapping[["x"]])),
    y = with(main_plot$data, eval(main_plot$mapping[["y"]]))
  )
  img_plot <- ggplot(data = data, aes(x, y)) +
    geom_blank() +
    theme_minimal() +
    theme(line=element_blank())
  for (i in seq_along(img_levels)) {
    img_plot <- img_plot + annotation_custom(img_grobs[[img_levels[[i]]]], xmin = x_range[i, 1], xmax = x_range[i, 2])
  }
  img_plot
}

# create image plot
img_plot <- create_img_plot(main_plot, img_levels, x_range)

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
# Return combined image grob
#-------------------------------------------------------------------------------
if(is.null(img_height) & img_type == "strength") {
  img_height <- 1
}
if(is.null(img_height) & img_type == "hydration") {
  img_height <- 2
}
gridExtra::grid.arrange(
  main_plot_grob,
  img_plot_grob,
  ncol = 1,
  heights = c(unit(x = 7, units = "null"), unit(x = img_height, units = "null"))
)
################################################################################









img_plot(

  )
