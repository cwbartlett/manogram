library(imgplot)
#-------------------------------------------------------------------------------
# test1 - uniform
#-------------------------------------------------------------------------------
set.seed(1)
n <- 80
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
main_plot

main_plot = main_plot
breaks = 11
labels = 0:10
img_type = "strength"
n_img = 10
img_positions = "peaks"
peaks_adjust = 0
img_range = NULL
img_height = NULL
panel = TRUE

plot <- img_plot(
  main_plot = main_plot,
  breaks = breaks,
  img_type = "strength",
  n_img = 10,
  img_positions = "peaks",
  peaks_adjust = 0,
  img_range = NULL,
  img_height = NULL,
  panel = panel
)
ggsave(filename = "test.pdf", plot = plot)

#-------------------------------------------------------------------------------
# test2 - uniform
#-------------------------------------------------------------------------------
set.seed(1)
n <- 300
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
  geom_smooth(method = "loess", span = 0.7, n = length(x1)) +
  scale_x_continuous(
    name = "Pairwise Sequence Alignment",
    breaks = 1:length(data$a),
    labels = paste0(data$a, "\n", data$c)
  )

img_plot(
  main_plot = main_plot,
  y_intervals = NULL,
  img_type = "strength",
  n_img = 2,
  x_index = "uniform"
)

#-------------------------------------------------------------------------------
# test3 - uniform
#-------------------------------------------------------------------------------
set.seed(1)
n <- 300
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
  geom_smooth(method = "loess", span = 0.7, n = length(x1)) +
  scale_x_continuous(
    name = "Pairwise Sequence Alignment",
    breaks = 1:length(data$a),
    labels = paste0(data$a, "\n", data$c)
  )

img_plot(
  main_plot = main_plot,
  y_intervals = NULL,
  img_type = "strength",
  n_img = 20,
  x_index = "uniform"
)



#-------------------------------------------------------------------------------
# test4 - peaks
#-------------------------------------------------------------------------------
set.seed(1)
n <- 300
x1 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE)
x2 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE)
y <- sin(5 * pi * seq_along(x1) / n) + runif(length(x1), -1, 1)
data <- data.frame(
  a = x1,
  c = x2,
  b = y,
  stringsAsFactors = FALSE
)
main_plot <- ggplot(data, aes(1:length(x1), b)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3, n = length(x1)) +
  scale_x_continuous(
    name = "Pairwise Sequence Alignment",
    breaks = 1:length(data$a),
    labels = paste0(data$a, "\n", data$c)
  )

img_plot(
  main_plot = main_plot,
  y_intervals = NULL,
  img_type = "strength",
  n_img = 1,
  x_index = "peaks"
)

#-------------------------------------------------------------------------------
# test5 - peaks
#-------------------------------------------------------------------------------
set.seed(1)
n <- 300
x1 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE)
x2 = sample(LETTERS[c(1, 3, 7, 20)], size = n, replace = TRUE)
y <- sin(5 * pi * seq_along(x1) / n) + runif(length(x1), -1, 1)
data <- data.frame(
  a = x1,
  c = x2,
  b = y,
  stringsAsFactors = FALSE
)
main_plot <- ggplot(data, aes(1:length(x1), b)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.1, n = length(x1)) +
  scale_x_continuous(
    name = "Pairwise Sequence Alignment",
    breaks = 1:length(data$a),
    labels = paste0(data$a, "\n", data$c)
  )

img_plot(
  main_plot = main_plot,
  y_intervals = NULL,
  img_type = "strength",
  n_img = 20,
  x_index = "peaks",
  window_adjust = 10
)
