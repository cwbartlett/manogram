# imgplot

`imgplot` is an R package that produces scatterplots with culturally meaningful glyphs. It offers hydration and strength images that can aide in the interpretation of biological based scatterplots.

## Examples

```{r}
library(imgplot)

#-------------------------------------------------------------------------------
# Create example data.
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
  theme_bw() +
  geom_smooth(method = "loess", span = 0.7) +
  scale_x_continuous(
    name = "Pairwise Sequence Alignment",
    breaks = 1:length(data$a),
    labels = paste0(data$a, "\n", data$c)
  )
main_plot

#-------------------------------------------------------------------------------
# Uniform - Strength
#-------------------------------------------------------------------------------
plot <- img_plot(
  main_plot = main_plot,
  y_breaks = 11,
  y_labels = 0:10,
  img_type = "strength",
  n_img = 9,
  img_positions = "uniform"
)

#-------------------------------------------------------------------------------
# Peaks - Strength
#-------------------------------------------------------------------------------
plot <- img_plot(
  main_plot = main_plot,
  y_breaks = 11,
  y_labels = 0:10,
  img_type = "strength",
  n_img = 9,
  img_positions = "peaks"
)

#-------------------------------------------------------------------------------
# Manual positioning - Strength
#-------------------------------------------------------------------------------
plot <- img_plot(
  main_plot = main_plot,
  y_breaks = 11,
  y_labels = 0:10,
  img_type = "strength",
  img_positions = c(20, 60)
)

#-------------------------------------------------------------------------------
# Uniform - Hydration
#-------------------------------------------------------------------------------
plot <- img_plot(
  main_plot = main_plot,
  y_breaks = 11,
  y_labels = 0:10,
  img_type = "hydration",
  n_img = 9,
  img_positions = "uniform",
)

#-------------------------------------------------------------------------------
# Peaks - Hydration
#-------------------------------------------------------------------------------
plot <- img_plot(
  main_plot = main_plot,
  y_breaks = 11,
  y_labels = 0:10,
  img_type = "hydration",
  n_img = 9,
  img_positions = "peaks"
)

#-------------------------------------------------------------------------------
# Manual positioning - Hydration
#-------------------------------------------------------------------------------
plot <- img_plot(
  main_plot = main_plot,
  y_breaks = 11,
  y_labels = 0:10,
  img_type = "hydration",
  img_positions = c(20, 60)
)
```

## Installation

devtools::install_github("cwbartlett/manogram")
