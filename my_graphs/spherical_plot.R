# --- spherical_surface_with_planes.R -----------------------------------------
library(rgl)
library(viridisLite)

# 1) Angular grid
n_theta <- 200
n_phi   <- 400
theta <- seq(0, pi, length.out = n_theta)
phi   <- seq(0, 2*pi, length.out = n_phi)

# 2) Parameter matrices
Theta <- outer(theta, rep(1, n_phi))
Phi   <- outer(rep(1, n_theta), phi)

# 3) Radius field
# R <- exp(-(Theta^2 + Phi^2))
# R <- exp(-(Theta + Phi))
R <- Theta * Phi

# 4) Cartesian conversion
X <- R * sin(Theta) * cos(Phi)
Y <- R * sin(Theta) * sin(Phi)
Z <- R * cos(Theta)

# 5) Colors
r_min <- min(R); r_max <- max(R)
R_norm <- (R - r_min) / (r_max - r_min + 1e-12)
cols <- viridis(256)[pmax(1, round(R_norm * 255) + 1)]

# 6) Render surface
open3d(windowRect = c(100, 100, 900, 900))
bg3d("white")
material3d(specular = "gray70", shininess = 40)
surface3d(X, Y, Z, color = cols, back = "lines", lit = TRUE)

# 7) Semi-transparent coordinate planes
alpha_plane <- 0.2   # transparency

# XY-plane at Z=0
quads3d(x = c(-1,1,1,-1)*10.0, 
        y = c(-1,-1,1,1)*10.0, 
        z = rep(0,4), color="red", alpha=alpha_plane)

# YZ-plane at X=0
quads3d(x = rep(0,4), 
        y = c(-1,-1,1,1)*10.0, 
        z = c(-1,1,1,-1)*10.0, color="green", alpha=alpha_plane)

# XZ-plane at Y=0
quads3d(x = c(-1,-1,1,1)*10.0, 
        y = rep(0,4), 
        z = c(-1,1,1,-1)*10.0, color="blue", alpha=alpha_plane)

# 8) Axes and labels
axes3d(edges=c("x--","y--","z--"))
title3d(xlab="X", ylab="Y", zlab="Z")
aspect3d("iso")
view3d(theta = -40, phi = 20, zoom = 1.2)
# ------------------------------------------------------------------------------
