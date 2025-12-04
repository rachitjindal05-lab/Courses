## MTH208: Quiz 2 — SOLUTION

## ---------
## Problem 1
## ---------

library(imager)

add_border <- function(img, border, save_path = NULL) {
  w  <- width(img)    # or dim(img)[1]
  h  <- height(img)   # or dim(img)[2]
  z  <- 1
  cc <- spectrum(img) # or dim(img)[4]
  
  W <- w + 2*border
  H <- h + 2*border
  
  border_col <- if (cc == 1) 0 else c(0, 0, 128/255)
  
  canvas_arr <- array(0, dim = c(W, H, z, cc))
  if (cc == 1) {
    canvas_arr[,,1,1] <- border_col
  } else {
    canvas_arr[,,1,1] <- border_col[1]
    canvas_arr[,,1,2] <- border_col[2]
    canvas_arr[,,1,3] <- border_col[3]
  }
  canvas <- as.cimg(canvas_arr)
  
  canvas[(border+1):(border+w), (border+1):(border+h), 1, 1:cc] <- img
  out <- canvas
  
  if (!is.null(save_path)) {
    save.image(out, save_path)
  }
  out
}

img <- load.image("djb.jpeg")
out <- add_border(img, 30, save_path = "djb_with_border.jpeg")
plot(out, axes = FALSE, main = "Image with Colored Border")

## ---------
## Problem 2
## ---------

library(rbenchmark)

set.seed(1)
make_X <- function(n, p) matrix(rnorm(n*p), nrow = n)

# A) Nested for loop
dist_loop <- function(X) {
  n <- nrow(X)
  D <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in i:n) {
      dij <- sqrt(sum((X[i,] - X[j,])^2))
      D[i,j] <- dij
      D[j,i] <- dij
    }
  }
  return(D)
}

# B) Base R dist() → full matrix
dist_base <- function(X) {
  as.matrix(dist(X))
}

# C) Robust cross-product version
dist_xprod <- function(X) {
  G  <- tcrossprod(X)
  rs <- rowSums(X^2)
  D2 <- outer(rs, rs, "+") - 2*G
  D2 <- pmax(D2, 0)
  diag(D2) <- 0
  sqrt((D2 + t(D2))/2)  # the last evaluated expression
}

# Correctness
X_small <- make_X(50, 20)
A <- dist_loop(X_small)
B <- dist_base(X_small)
C <- dist_xprod(X_small)
stopifnot(max(abs(A - B)) < 1e-6)
stopifnot(max(abs(A - C)) < 1e-6)

# Benchmarks
n_seq <- c(200, 400, 800)
p <- 50
bench_all <- list()

for (n in n_seq) {
  X <- make_X(n, p); 
  res <- benchmark(
    "loop"  = dist_loop(X),
    "dist"  = dist_base(X),
    "xprod" = dist_xprod(X),
    replications = 5,
    columns = c("test","replications","elapsed","relative"),
    order   = "elapsed"
  )
  bench_all[[as.character(n)]] <- res
}
bench_res <- do.call(rbind, bench_all)
print(bench_res)

