library(ggplot2)
library(patchwork)

# 1. Define range and create a data frame grid
# expand.grid is the standard tidyverse/ggplot way to create the grid
df <- expand.grid(
  x = seq(-10, 10, length.out = 2000),
  y = seq(-10, 10, length.out = 2000)
)

# 2. Compute inequalities and add them as columns
df$Z1 <- sin(df$x * cos(df$y)) > cos(df$x * sin(df$x))
df$Z2 <- sin(df$x * cos(df$y)) > cos(df$x * sin(df$y))

# 3. Create Plot 1
p1 <- ggplot(df, aes(x = x, y = y, fill = Z1)) +
  geom_raster() +
  scale_fill_manual(
    name = "Condition", # Legend title
    values = c("TRUE" = "green", "FALSE" = "blue")
  ) +
  labs(
    title = "sin(x*cos(y)) > cos(x*sin(x))",
    x = "x-axis",
    y = "y-axis"
  ) +
  theme_minimal() +
  coord_fixed() # Ensures the aspect ratio is 1:1

# 4. Create Plot 2
p2 <- ggplot(df, aes(x = x, y = y, fill = Z2)) +
  geom_raster() +
  scale_fill_manual(
    name = "Condition", # Legend title
    values = c("TRUE" = "green", "FALSE" = "blue")
  ) +
  labs(
    title = "sin(x*cos(y)) > cos(x*sin(y))",
    x = "x-axis",
    y = "y-axis"
  ) +
  theme_minimal() +
  coord_fixed() # Ensures the aspect ratio is 1:1

# 5. Arrange plots side-by-side using patchwork
p1 + p2