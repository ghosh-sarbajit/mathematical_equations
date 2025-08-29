# Load the plotting library
library(ggplot2)

# 1. SET UP THE GRID
# Define the range and resolution for the plot
x_vals <- seq(-200, 200, by = 0.1)
y_vals <- seq(-200, 200, by = 0.1)
grid <- expand.grid(x = x_vals, y = y_vals)

# 2. DEFINE THE INEQUALITY FUNCTION
# This function checks if max > 2 * second_max for a single (x,y) point
check_inequality <- function(x, y) {
  # Helper to convert degrees to radians
  deg2rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  # Calculate the five values
  A <- cos(x)
  B <- cos(x * cos(deg2rad(72)) + y * sin(deg2rad(72)))
  C <- cos(x * cos(deg2rad(144)) + y * sin(deg2rad(144)))
  D <- cos(x * cos(deg2rad(216)) + y * sin(deg2rad(216)))
  E <- cos(x * cos(deg2rad(288)) + y * sin(deg2rad(288)))
  
  # Combine them into a vector
  values <- c(A, B, C, D, E)
  
  # Sort the values in descending order to find the max and 2nd max
  sorted_values <- sort(values, decreasing = TRUE)
  
  # The inequality check: is the 1st element > 2 * 2nd element?
  return(sorted_values[1] > 2 * sorted_values[2])
}

# 3. APPLY THE FUNCTION TO THE GRID
# Use mapply to apply the function to each pair of x and y coordinates
grid$result <- mapply(check_inequality, grid$x, grid$y)

# 4. CREATE THE PLOT
ggplot(grid, aes(x = x, y = y, fill = result)) +
  geom_raster(show.legend = FALSE) + # Hides the TRUE/FALSE legend
  coord_fixed(ratio = 1) + # Ensures correct aspect ratio
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "navyblue")) +
  labs(
    title = "Plot of max(V) > 2 * second_max(V)",
    x = "x-axis",
    y = "y-axis"
  ) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "navyblue"))