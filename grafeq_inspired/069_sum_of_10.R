# Load the necessary library
library(ggplot2)
library(svglite)

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
myplot <- ggplot(grid, aes(x = x, y = y, fill = value > 0)) +
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
