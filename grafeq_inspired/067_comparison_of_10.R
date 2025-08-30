# Load the plotting library
library(ggplot2)
library(svglite)

# 1. SET UP THE GRID
# Define the range and resolution for the plot
x_vals <- seq(-20, 20, by = 0.1)
y_vals <- seq(-20, 20, by = 0.1)
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
myplot <- ggplot(grid, aes(x = x, y = y, fill = result)) +
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

print(myplot)
# --- Lines Added to Save the Graph ---

# Determine the script name for the output file
# This finds the name of the script being run.
# It works when you run the script from the command line (e.g., Rscript my_script.R)
# or by clicking "Source" in RStudio.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("--file=", args, value = TRUE)

# Set a default name in case the script name can't be found (e.g., running line-by-line)
if (length(file_arg) == 0) {
  script_name <- "output_plot"
  cat("Could not determine script name. Saving plot as 'output_plot.svg'\n")
} else {
  # Extract the base name of the script, without the ".R" extension
  script_path <- sub("--file=", "", file_arg)
  script_name <- tools::file_path_sans_ext(basename(script_path))
}

# 7. Save the combined plot to an SVG file
svg_filename <- paste0(script_name, ".svg")

ggsave(
  filename = svg_filename,
  plot = myplot,
  device = 'svg',
  width = 10, # You can adjust the width
  height = 5  # You can adjust the height
)

cat("Graph successfully saved to:", svg_filename, "\n")