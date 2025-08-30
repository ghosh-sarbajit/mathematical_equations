library(ggplot2)
library(patchwork)
library(svglite)

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

# 5. Arrange plots and assign to a variable
combined_plot <- p1 + p2

# Display the plot in the plot pane
print(combined_plot)


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
  plot = combined_plot,
  device = 'svg',
  width = 10, # You can adjust the width
  height = 5  # You can adjust the height
)

cat("Graph successfully saved to:", svg_filename, "\n")