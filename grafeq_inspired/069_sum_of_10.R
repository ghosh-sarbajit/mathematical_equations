# Load the necessary library
library(ggplot2)

# 1. SET UP THE GRID
# Define the range and resolution for x and y coordinates
x_vals <- seq(-150, 150, by = 0.1)
y_vals <- seq(-150, 150, by = 0.1)
grid <- expand.grid(x = x_vals, y = y_vals)

# 2. DEFINE THE INEQUALITY FUNCTION
# This function calculates A+B+C+D+E for any given x and y
calculate_sum <- function(x, y) {
  # R's trig functions use radians, so convert degrees to radians
  deg2rad <- function(deg) {
    return(deg * pi / 180)
  }

  A <- cos(x)
  B <- cos(x * cos(deg2rad(72)) + y * sin(deg2rad(72)))
  C <- cos(x * cos(deg2rad(144)) + y * sin(deg2rad(144)))
  D <- cos(x * cos(deg2rad(216)) + y * sin(deg2rad(216)))
  E <- cos(x * cos(deg2rad(288)) + y * sin(deg2rad(288)))

  return(A + B + C + D + E)
}

# 3. CALCULATE THE VALUE FOR EACH POINT ON THE GRID
grid$value <- calculate_sum(grid$x, grid$y)

# 4. CREATE THE PLOT
ggplot(grid, aes(x = x, y = y, fill = value > 0)) +
  geom_raster() +
  coord_fixed(ratio = 1) + # Ensures the plot is not stretched
  scale_fill_manual(
    values = c("TRUE" = "gold", "FALSE" = "navyblue"),
    name = "A+B+C+D+E > 0"
  ) +
  labs(
    title = "Plot of the Quasicrystal Inequality",
    subtitle = "Region where Σcos(...) > 0",
    x = "x-axis",
    y = "y-axis"
  ) +
  theme_minimal()
