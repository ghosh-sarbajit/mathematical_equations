library(caTools)

jet.colors <-
    colorRampPalette(
        c("green", "pink", "#007FFF", "cyan", "#7FFF7F",
          "white", "#FF7F00", "red", "#7F0000"))

dx <- 1500 # define width
dy <- 1400 # define height

C  <- complex(
  real = rep(seq(-2.2, 1.0, length.out = dx), each = dy),
  imag = rep(seq(-1.2, 1.2, length.out = dy), times = dx)
)

# reshape as matrix of complex numbers
C <- matrix(C, dy, dx)

# initialize output 3D array
X <- array(0, c(dy, dx, 20))

Z <- 0

# loop with 20 iterations
for (k in 1:20) {

  # the central difference equation
  Z <- Z^2 + C

  # capture the results
  X[, , k] <- exp(-abs(Z))
}

write.gif(
    X,
    "Mandelbrot.gif",
    col = jet.colors,
    delay = 100
)


