# hsl_convert(img): compute H, S, L and plot histograms with fixed bins.
# Binning:
#   Hue:        breaks = seq(0, 360, 10)   (36 bins)
#   Saturation: breaks = seq(0, 1, 0.05)   (20 bins)
#   Lightness:  breaks = seq(0, 1, 0.05)   (20 bins)
hsl_convert <- function(img) {
  
  R <- as.vector(img[,,,1])
  G <- as.vector(img[,,,2])
  B <- as.vector(img[,,,3])
  
  Cmax  <- pmax(R, G, B)
  Cmin  <- pmin(R, G, B)
  Delta <- Cmax - Cmin
  
  # Lightness
  L <- (Cmax + Cmin) / 2
  
  # Saturation
  S <- ifelse(Delta == 0, 0, Delta / (1 - abs(2 * L - 1)))
  
  # Hue (degrees in [0, 360))
  H <- numeric(length(R))
  mask <- Delta > 0
  idxR <- mask & (Cmax == R)
  idxG <- mask & (Cmax == G)
  idxB <- mask & (Cmax == B)
  H[idxR] <- 60 * (((G[idxR] - B[idxR]) / Delta[idxR]) %% 6)
  H[idxG] <- 60 * (((B[idxG] - R[idxG]) / Delta[idxG]) + 2)
  H[idxB] <- 60 * (((R[idxB] - G[idxB]) / Delta[idxB]) + 4)
  H <- (H %% 360 + 360) %% 360
  
  par(mfrow = c(1, 3))
  
  hist(H,
       breaks = seq(0, 360, by = 10),
       xlim   = c(0, 360),
       include.lowest = TRUE,
       main = "Hue (deg)", xlab = "H", ylab = "Count")
  
  hist(S,
       breaks = seq(0, 1, by = 0.05),
       xlim   = c(0, 1),
       include.lowest = TRUE,
       main = "Saturation", xlab = "S", ylab = "Count")
  
  hist(L,
       breaks = seq(0, 1, by = 0.05),
       xlim   = c(0, 1),
       include.lowest = TRUE,
       main = "Lightness", xlab = "L", ylab = "Count")
  
  invisible(NULL)
}

library(imager)

# Point to your image
img <- imager::load.image("data/sample_image.png")

# Call your function (it will plot 3 histograms)
hsl_convert(img)


