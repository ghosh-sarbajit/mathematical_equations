# ==============================================================================
# RIEMANN HYPOTHESIS VISUALIZATION DASHBOARD IN R
# ==============================================================================
# Author: Antigravity AI
# Description: Computes and visualizes key analytical properties of the 
#              Riemann Zeta Function zeta(s) in relation to the Riemann Hypothesis:
#              1. Zeta & Hardy's Z-function on the Critical Line Re(s) = 1/2
#              2. Parametric Trajectory of zeta(1/2 + it) in the Complex Plane
#              3. Magnitude Landscape |zeta(sigma + it)| across the Critical Strip
#              4. Connection between Zeta Zeros and the Prime Counting Function pi(x)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(gridExtra)
  library(viridis)
  library(dplyr)
  library(tidyr)
})

# ------------------------------------------------------------------------------
# 1. CORE NUMERICAL FUNCTIONS FOR THE RIEMANN ZETA FUNCTION
# ------------------------------------------------------------------------------

# Dirichlet Eta function acceleration (Borwein 1995 Algorithm)
# s: complex number or vector of complex numbers
zeta_borwein <- function(s, N = 45) {
  k_vec <- 1:N
  two_N <- 2^N
  e <- sapply(k_vec, function(k) sum(choose(N, k:N))) / two_N
  
  sapply(s, function(s_val) {
    if (abs(s_val - 1.0) < 1e-9) return(Inf)
    term <- (-1)^(k_vec - 1) * e / (k_vec^s_val)
    eta <- sum(term)
    eta / (1 - 2^(1 - s_val))
  })
}

# Asymptotic expansion of the Riemann-Siegel Theta function theta(t)
theta_rs <- function(t) {
  sapply(t, function(t_val) {
    if (t_val <= 0.1) return(0)
    0.5 * t_val * log(t_val / (2 * pi)) - 0.5 * t_val - pi / 8 + 
      1 / (48 * t_val) + 7 / (5760 * t_val^3)
  })
}

# Hardy's Z-function: Z(t) = exp(i * theta(t)) * zeta(1/2 + i*t)
hardy_z <- function(t, N = 45) {
  s_vals <- 0.5 + 1i * t
  z_vals <- zeta_borwein(s_vals, N = N)
  th_vals <- theta_rs(t)
  Re(exp(1i * th_vals) * z_vals)
}

# Known non-trivial zeros (imaginary parts t_k on critical line Re(s) = 0.5)
known_zeros <- c(
  14.13472514, 21.02203964, 25.01085758, 30.42487613, 32.93506159,
  37.58617815, 40.91871901, 43.32707328, 48.00515088, 49.77383248
)

# Custom dark theme for publication-quality output
theme_riemann <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#0f172a", color = NA),
      panel.background = element_rect(fill = "#1e293b", color = NA),
      panel.grid.major = element_line(color = "#334155", linewidth = 0.4),
      panel.grid.minor = element_line(color = "#1e293b", linewidth = 0.2),
      text = element_text(color = "#f8fafc"),
      axis.text = element_text(color = "#cbd5e1"),
      axis.title = element_text(color = "#f1f5f9", face = "bold"),
      plot.title = element_text(color = "#38bdf8", face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(color = "#94a3b8", size = 10, hjust = 0.5),
      legend.background = element_rect(fill = "#1e293b", color = NA),
      legend.text = element_text(color = "#f8fafc")
    )
}

# ------------------------------------------------------------------------------
# 2. GENERATE PLOT 1: HARDY'S Z-FUNCTION & MODULUS ON CRITICAL LINE
# ------------------------------------------------------------------------------

cat("Computing Plot 1: Zeta on the Critical Line Re(s) = 1/2...\n")
t_grid1 <- seq(0, 50, length.out = 800)
zeta_crit <- zeta_borwein(0.5 + 1i * t_grid1, N = 45)
z_func <- hardy_z(t_grid1, N = 45)
mod_zeta <- Mod(zeta_crit)

df_crit <- data.frame(
  t = t_grid1,
  Z = z_func,
  ModZeta = mod_zeta
)

df_zeros <- data.frame(
  t = known_zeros[known_zeros <= 50],
  y = 0
)

p1 <- ggplot(df_crit, aes(x = t)) +
  geom_hline(yintercept = 0, color = "#64748b", linetype = "dashed", linewidth = 0.6) +
  geom_line(aes(y = Z, color = "Hardy Z(t)"), linewidth = 0.8) +
  geom_line(aes(y = ModZeta, color = "|ζ(1/2 + it)|"), linewidth = 0.8, linetype = "dotdash") +
  geom_point(data = df_zeros, aes(x = t, y = y, fill = "Non-trivial Zeros"), 
             shape = 23, size = 3, color = "#f59e0b", stroke = 1) +
  scale_color_manual(name = "Functions", values = c("Hardy Z(t)" = "#38bdf8", "|ζ(1/2 + it)|" = "#f43f5e")) +
  scale_fill_manual(name = "Markers", values = c("Non-trivial Zeros" = "#fbbf24")) +
  labs(
    title = "1. Behavior on the Critical Line Re(s) = 1/2",
    subtitle = "Z(t) vanishes at roots; |ζ(1/2 + it)| touches zero at all non-trivial roots",
    x = "Imaginary Height (t)",
    y = "Magnitude / Z(t)"
  ) +
  theme_riemann() +
  theme(legend.position = "top")

# ------------------------------------------------------------------------------
# 3. GENERATE PLOT 2: PARAMETRIC TRAJECTORY IN THE COMPLEX PLANE
# ------------------------------------------------------------------------------

cat("Computing Plot 2: Parametric Spiral Trajectory in Complex Plane...\n")
t_grid2 <- seq(0, 35, length.out = 1000)
zeta_traj <- zeta_borwein(0.5 + 1i * t_grid2, N = 45)

df_traj <- data.frame(
  Real = Re(zeta_traj),
  Imag = Im(zeta_traj),
  t = t_grid2
)

zeros_traj <- data.frame(
  Real = 0,
  Imag = 0,
  label = "Origin (0,0)\nZeros of ζ(s)"
)

p2 <- ggplot(df_traj, aes(x = Real, y = Imag)) +
  geom_path(aes(color = t), linewidth = 0.8) +
  geom_point(data = zeros_traj, aes(x = Real, y = Imag), color = "#f59e0b", size = 4, shape = 4, stroke = 2) +
  geom_vline(xintercept = 0, color = "#475569", linetype = "dotted") +
  geom_hline(yintercept = 0, color = "#475569", linetype = "dotted") +
  scale_color_viridis_c(option = "turbo", name = "t parameter") +
  labs(
    title = "2. Parametric Trajectory of ζ(1/2 + it)",
    subtitle = "As t grows, curve loops continuously through the origin (0,0) at every zero",
    x = "Re(ζ(1/2 + it))",
    y = "Im(ζ(1/2 + it))"
  ) +
  coord_fixed(ratio = 1) +
  theme_riemann() +
  theme(legend.position = "right")

# ------------------------------------------------------------------------------
# 4. GENERATE PLOT 3: MAGNITUDE LANDSCAPE IN THE CRITICAL STRIP
# ------------------------------------------------------------------------------

cat("Computing Plot 3: 2D Landscape Across Critical Strip (0 < σ < 1)...\n")
sigma_grid <- seq(0.05, 0.95, length.out = 60)
t_grid3 <- seq(10, 35, length.out = 120)

grid_3d <- expand.grid(sigma = sigma_grid, t = t_grid3)
s_grid <- grid_3d$sigma + 1i * grid_3d$t
grid_3d$mod_zeta <- Mod(zeta_borwein(s_grid, N = 35))

p3 <- ggplot(grid_3d, aes(x = sigma, y = t, fill = mod_zeta)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "|ζ(s)|", trans = "sqrt") +
  geom_vline(xintercept = 0.5, color = "#38bdf8", linetype = "dashed", linewidth = 1) +
  geom_point(data = data.frame(sigma = 0.5, t = known_zeros[known_zeros >= 10 & known_zeros <= 35]),
             aes(x = sigma, y = t), color = "#34d399", size = 2.5, inherit.aes = FALSE) +
  annotate("text", x = 0.52, y = 12, label = "Critical Line (σ = 1/2)", color = "#38bdf8", angle = 90, hjust = 0, fontface = "bold") +
  labs(
    title = "3. Magnitude |ζ(σ + it)| in Critical Strip",
    subtitle = "All zeros (green dots) lie strictly on the central line σ = 1/2 (Riemann Hypothesis)",
    x = "Real Part (σ)",
    y = "Imaginary Height (t)"
  ) +
  theme_riemann() +
  theme(legend.position = "right")

# ------------------------------------------------------------------------------
# 5. GENERATE PLOT 4: PRIME COUNTING FUNCTION & RIEMANN CONNECTION
# ------------------------------------------------------------------------------

cat("Computing Plot 4: Prime Distribution & Logarithmic Integral...\n")
is_prime <- function(n) {
  if (n <= 1) return(FALSE)
  if (n <= 3) return(TRUE)
  if (n %% 2 == 0 || n %% 3 == 0) return(FALSE)
  i <- 5
  while (i * i <= n) {
    if (n %% i == 0 || n %% (i + 2) == 0) return(FALSE)
    i <- i + 6
  }
  return(TRUE)
}

x_vals <- 2:100
primes <- sapply(x_vals, is_prime)
pi_x <- cumsum(primes)

# Logarithmic integral approximation Li(x) = int_2^x dt/ln(t)
li_x <- sapply(x_vals, function(x) {
  integrate(function(t) 1 / log(t), lower = 2, upper = x)$value + 1.04516 # Li(2) offset
})

df_primes <- data.frame(
  x = x_vals,
  pi_x = pi_x,
  li_x = li_x,
  diff = li_x - pi_x
)

p4 <- ggplot(df_primes, aes(x = x)) +
  geom_step(aes(y = pi_x, color = "Prime Count π(x)"), linewidth = 1) +
  geom_line(aes(y = li_x, color = "Logarithmic Integral Li(x)"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(name = "Functions", values = c("Prime Count π(x)" = "#a855f7", "Logarithmic Integral Li(x)" = "#34d399")) +
  labs(
    title = "4. Prime Number Theorem & Zeta Zeros",
    subtitle = "Zeros of ζ(s) govern the precise error fluctuations between π(x) and Li(x)",
    x = "Number (x)",
    y = "Count"
  ) +
  theme_riemann() +
  theme(legend.position = "top")

# ------------------------------------------------------------------------------
# 6. COMBINE ALL PLOTS INTO A COMPOSITE DASHBOARD & SAVE
# ------------------------------------------------------------------------------

cat("Saving individual plots and dashboard...\n")
ggsave("plot1_critical_line.png", p1, width = 8, height = 6, dpi = 300)
ggsave("plot2_complex_trajectory.png", p2, width = 8, height = 6, dpi = 300)
ggsave("plot3_critical_strip_landscape.png", p3, width = 8, height = 6, dpi = 300)
ggsave("plot4_prime_connection.png", p4, width = 8, height = 6, dpi = 300)

title_grob <- grid::textGrob(
  "THE RIEMANN HYPOTHESIS: MATHEMATICAL EXPLORATION DASHBOARD",
  gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#38bdf8")
)
dashboard <- gridExtra::marrangeGrob(
  list(p1, p2, p3, p4),
  nrow = 2, ncol = 2,
  top = title_grob
)

output_file <- "riemann_hypothesis_dashboard.png"
ggsave(output_file, dashboard, width = 16, height = 12, dpi = 300)
cat(sprintf("\nSuccessfully saved dashboard image to '%s' and individual plot PNGs!\n", output_file))
