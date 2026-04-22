# ======================================
# Ridge vs Lasso paths as constraint shrinks
# ======================================

# OLS estimate (2D for visualization)
beta_hat <- c(1, 2)

# Function: ridge solution for given t
ridge_solution <- function(t, beta_hat) {
  b <- beta_hat
  if (sum(b^2) <= t) return(b)  # constraint not active
  c_t <- sqrt(t / sum(b^2))
  c_t * b
}

# Function: lasso solution for given t
# We'll find lambda s.t. sum(|beta|)=t via binary search on lambda
lasso_solution <- function(t, beta_hat, tol = 1e-5) {
  soft_thresh <- function(b, lam) sign(b) * pmax(abs(b) - lam, 0)
  f <- function(lam) sum(abs(soft_thresh(beta_hat, lam))) - t
  # Binary search over lambda in [0, max|beta_hat|]
  lam_low <- 0; lam_high <- max(abs(beta_hat))
  for (i in 1:100) {
    lam_mid <- (lam_low + lam_high) / 2
    if (f(lam_mid) > 0) lam_low <- lam_mid else lam_high <- lam_mid
  }
  soft_thresh(beta_hat, lam_high)
}

# Grid for least squares contours
grid_lim <- 2.5
beta1 <- seq(-grid_lim, grid_lim, length = 200)
beta2 <- seq(-grid_lim, grid_lim, length = 200)
Z <- outer(beta1, beta2, function(b1, b2)
  (b1 - beta_hat[1])^2 + (b2 - beta_hat[2])^2)

# Plot LS contours
contour(beta1, beta2, Z, nlevels = 15, drawlabels = TRUE,
        xlab = expression(beta[1]), ylab = expression(beta[2]),
        main = "Ridge vs Lasso solutions as constraint shrinks")

# Draw constraint sets for reference (t=1)
theta <- seq(0, 2*pi, length = 200)
t0 <- 1
polygon(sqrt(t0)*cos(theta), sqrt(t0)*sin(theta),
        col = rgb(0.2,0.4,1,0.15), border = NA)
diamond_x <- c(-t0, 0, t0, 0)
diamond_y <- c(0, t0, 0, -t0)
polygon(diamond_x, diamond_y, col = rgb(1,0.3,0.3,0.15), border = NA)

# Mark OLS
points(beta_hat[1], beta_hat[2], pch=19, col="black")
text(beta_hat[1]+0.2, beta_hat[2], "OLS", col="black")

# Compute solutions over a sequence of t values
t_seq <- seq(2, .1, length = 12)

ridge_pts <- t(sapply(t_seq, ridge_solution, beta_hat = beta_hat))
lasso_pts <- t(sapply(sqrt(t_seq), lasso_solution, beta_hat = beta_hat))
for(i in 1:length(t_seq))
{
  Sys.sleep(1)
  # Plot LS contours
  contour(beta1, beta2, Z, nlevels = 15, drawlabels = TRUE,
          xlab = expression(beta[1]), ylab = expression(beta[2]),
          main = "Ridge vs Lasso solutions as constraint shrinks")
  
  abline(v = 0, h = 0, lty = 2, col = "orange")
  # Draw constraint sets for reference (t=1)
  theta <- seq(0, 2*pi, length = 200)
  t0 <- t_seq[i]
  polygon(sqrt(t0)*cos(theta), sqrt(t0)*sin(theta),
          col = rgb(0.2,0.4,1,0.15), border = NA)
  t0 <- sqrt(t0)
  diamond_x <- c(-t0, 0, t0, 0)
  diamond_y <- c(0, t0, 0, -t0)
  polygon(diamond_x, diamond_y, col = rgb(1,0.3,0.3,0.15), border = NA)
  
  # Mark OLS
  points(beta_hat[1], beta_hat[2], pch=19, col="black")
  text(beta_hat[1]+0.2, beta_hat[2], "OLS", col="black")
  
  # # Plot ridge and lasso solution paths
  # lines(ridge_pts[,1], ridge_pts[,2], col = "blue", lwd = 2)
  # lines(lasso_pts[,1], lasso_pts[,2], col = "red", lwd = 2)
  
  # Mark each t value point
  points(ridge_pts[1:i,1], ridge_pts[1:i,2], pch = 21, bg = "blue", col = "white")
  points(lasso_pts[1:i,1], lasso_pts[1:i,2], pch = 21, bg = "red", col = "white")
  
  # Legend
  legend("topright",
         legend = c("Ridge path", "Lasso path"),
         col = c("blue", "red"), lwd = 2, bty = "n")
}
