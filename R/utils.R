# This function will create a list with 2 elements: breaks and labels. The
# function is dependent on factors preserving ordering to get the results we need
# from the cut() function.
# x - A numeric vector which is to be converted to a factor by cut().
# breaks - Either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.
# labels - Labels for the levels of the images. Labels need to be integers 0 - 10, generally specifying lowest to highest values (strength, hydration, etc).
y_cut <- function(x, breaks, labels) {
  if(!all(labels %in% 0:10)) {
    stop("Check the labels argument. The labels need to be integers between 0 and 10.")
  }
  intervals_no_label <- cut(x = x, breaks = breaks, labels = NULL)
  intervals_label <- cut(x = x, breaks = breaks, labels = labels)
  lower <- as.numeric(sub("\\((.+),.*", "\\1", levels(intervals_no_label)))
  upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", levels(intervals_no_label)))
  list(
    labels = levels(intervals_label),
    breaks = sort(unique(c(lower,upper)))
  )
}

# utility function to calculate the the lower and upper values for each img
# position on the x-axis
x_pos <- function(img_positions) {
  if(is.factor(img_positions)) { # if feeding in values from cut() function
    x_index_lower <- as.numeric(sub("\\((.+),.*", "\\1", levels(img_positions))) + 0.1
    x_index_upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", levels(img_positions))) - 0.1
  } else { # if feeding in numeric position values
    x_index_lower <- img_positions - min(diff(img_positions))
    x_index_upper <- img_positions + min(diff(img_positions))
  }
  cbind(
    lower = x_index_lower,
    upper = x_index_upper
  )
}

# helper function to adjust the ranges of values used to calculate the average
# image level
x_pos_adjusted <- function(x_range, img_range) {
  x_index_lower <- round(apply(x_range, 1, function(x) {median(x)}) - img_range) - 0.1
  x_index_upper <- round(apply(x_range, 1, function(x) {median(x)}) + img_range)
  cbind(
    lower = x_index_lower,
    upper = round(x_index_upper)
  )
}

# helper function to create image plot
create_img_plot <- function(main_plot, img_grobs, img_levels, x_range) {
  data <- data.frame(
    x = with(main_plot$data, eval(main_plot$mapping[["x"]])),
    y = with(main_plot$data, eval(main_plot$mapping[["y"]]))
  )
  img_plot <- ggplot(data = data, aes(x, y)) +
    geom_blank() +
    theme_minimal() +
    theme(line=element_blank())
  for (i in seq_along(img_levels)) {
    img_plot <- img_plot + annotation_custom(img_grobs[[paste(img_levels[[i]])]], xmin = x_range[i, 1], xmax = x_range[i, 2])
  }
  img_plot
}
