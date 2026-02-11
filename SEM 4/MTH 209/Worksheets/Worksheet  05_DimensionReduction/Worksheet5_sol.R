
#                Worksheet 5 Solutions             
#                by Saurabh Yadav (TA)             
#===============================================================================

# Question no.1

library(ggplot2)
library(Rtsne)

#-------------------------------------------------------------------------------
# 1. CONDITIONAL PROBABILITIES p_{j|i} IN HIGH DIMENSIONS
#-------------------------------------------------------------------------------

# Given distances from point i to all other points
# and a variance sigma^2,
# compute the conditional probabilities
#
#   p_{j|i} ∝ exp( -||x_i - x_j||^2 / (2 sigma_i^2) )
#
conditional_probs <- function(dist_i, sigma2)
{
  # numerator of the Gaussian kernel
  num <- exp(-dist_i^2 / (2 * sigma2))
  
  # remove self–probability p_{i|i}
  num[which(dist_i == 0)] <- 0
  
  # normalize so probabilities sum to 1
  num <- num / sum(num)
  
  as.numeric(num)
}

#-------------------------------------------------------------------------------
# 2. LOW-DIMENSIONAL AFFINITIES Q
#-------------------------------------------------------------------------------

# Construct the joint probability matrix Q in low dimensions
#
# q_{ij} ∝ 1 / (1 + ||y_i - y_j||^2)
#
# This corresponds to a Student-t distribution
# with 1 degree of freedom (Cauchy)
make_Q <- function(ys)
{
  # pairwise distances in embedding space
  Dy <- as.matrix(dist(ys))
  
  # heavy-tailed kernel
  num_q <- 1 / (1 + Dy^2)
  
  # remove diagonal terms
  diag(num_q) <- 0
  
  # normalize to make a probability mass function
  Q <- num_q / sum(num_q)
  
  return(Q)
}

#-------------------------------------------------------------------------------
# 3. ENTROPY AND PERPLEXITY
#-------------------------------------------------------------------------------

# Shannon entropy of a probability mass function
entropy <- function(p, eps = 1e-12) {
  p <- p[p > eps]      # avoid log(0)
  -sum(p * log2(p))
}

#-------------------------------------------------------------------------------
# 4. FIND sigma^2 FOR A GIVEN PERPLEXITY
#-------------------------------------------------------------------------------

# Perplexity is defined as:
#
#   perplexity = 2^{H(P_i)}
#
# This function finds sigma_i^2 such that
# entropy(p_{j|i}) matches the desired perplexity.
#
# Done using binary search.
find_sigma2 <- function(dist_i, perplexity, tol = 1e-5, max_iter = 50) 
{
  # target entropy corresponding to desired perplexity
  target_entropy <- log2(perplexity)
  
  # bounds for sigma^2
  sigma2_min <- 1e-10
  sigma2_max <- 1e5
  sigma2 <- 1
  
  for (iter in 1:max_iter) {
    
    # compute conditional probabilities
    p <- conditional_probs(dist_i, sigma2)
    
    # entropy of current distribution
    H <- entropy(p)
    
    # stop if close enough
    if (abs(H - target_entropy) < tol) {
      break
    }
    
    if (H > target_entropy) {
      # distribution too spread out
      # → sigma too large
      sigma2_max <- sigma2
      sigma2 <- (sigma2 + sigma2_min) / 2
    } else {
      # distribution too concentrated
      # → sigma too small
      sigma2_min <- sigma2
      sigma2 <- (sigma2 + sigma2_max) / 2
    }
  }
  
  sigma2
}

#-------------------------------------------------------------------------------
# 5. KULLBACK–LEIBLER DIVERGENCE
#-------------------------------------------------------------------------------

# KL(P || Q) = sum_{ij} p_{ij} log(p_{ij} / q_{ij})
KL <- function(P, Q) 
{
  idx <- P > 0
  sum(P[idx] * log(P[idx] / Q[idx]))
}

#-------------------------------------------------------------------------------
# 6. GENERATE A SIMPLE TWO-CLUSTER DATASET
#-------------------------------------------------------------------------------

set.seed(1)

n <- 50

# two Gaussian clusters in 2D
X <- cbind(
  rnorm(n, rep(c(0,3), n/2)),
  rnorm(n, rep(c(0,3), n/2))
)

X <- round(X, 2)
colnames(X) <- c("x1", "x2")
rownames(X) <- paste0("x", 1:n)

# visualize original data
plot(
  X[,1], X[,2],
  pch = 19,
  xlab = "x1",
  ylab = "x2",
  main = "Observed data"
)

#-------------------------------------------------------------------------------
# 7. PAIRWISE DISTANCES
#-------------------------------------------------------------------------------

# Euclidean distance matrix
D <- as.matrix(dist(X))

#-------------------------------------------------------------------------------
# 8. FIND LOCAL BANDWIDTHS sigma_i^2
#-------------------------------------------------------------------------------

# small perplexity → local neighborhoods
# large perplexity → more global neighborhoods
sigma2_vec2  <- as.numeric(apply(D, 1, find_sigma2, 2))
sigma2_vec10 <- as.numeric(apply(D, 1, find_sigma2, 50))

# compare bandwidths
cbind(sigma2_vec2, sigma2_vec10)

#-------------------------------------------------------------------------------
# 9. CONDITIONAL PROBABILITIES p_{j|i}
#-------------------------------------------------------------------------------

P_cond2  <- matrix(0, n, n)
P_cond10 <- matrix(0, n, n)

for (i in 1:n) {
  P_cond2[i, ]  <- conditional_probs(D[i, ], sigma2_vec2[i])
  P_cond10[i, ] <- conditional_probs(D[i, ], sigma2_vec10[i])
}

round(P_cond2, 3)

#-------------------------------------------------------------------------------
# 10. VISUALIZE CONDITIONAL DISTRIBUTIONS
#-------------------------------------------------------------------------------

i <- 5  # reference point

df <- data.frame(
  x = X[,1],
  y = X[,2],
  p = P_cond2[i, ],
  label = rownames(X)
)

ggplot(df, aes(x, y)) +
  geom_point(aes(size = p), color = "steelblue") +
  geom_text(aes(label = label), vjust = -1) +
  scale_size(range = c(2, 10)) +
  ggtitle(paste("p_{j|i}, perplexity = 2, i =", rownames(X)[i])) +
  theme_minimal()

# same visualization for larger perplexity
df <- data.frame(
  x = X[,1],
  y = X[,2],
  p = P_cond10[i, ],
  label = rownames(X)
)

ggplot(df, aes(x, y)) +
  geom_point(aes(size = p), color = "steelblue") +
  geom_text(aes(label = label), vjust = -1) +
  scale_size(range = c(2, 10)) +
  ggtitle(paste("p_{j|i}, perplexity = 10, i =", rownames(X)[i])) +
  theme_minimal()

#-------------------------------------------------------------------------------
# 11. SYMMETRIZED JOINT PROBABILITIES P_{ij}
#-------------------------------------------------------------------------------

# t-SNE uses:
#
#   P_{ij} = (p_{j|i} + p_{i|j}) / (2n)
#
P2  <- (P_cond2  + t(P_cond2))  / (2 * n)
P10 <- (P_cond10 + t(P_cond10)) / (2 * n)

sum(P2)   # should be 1
sum(P10)

#-------------------------------------------------------------------------------
# 12. BAD EMBEDDING EXAMPLE
#-------------------------------------------------------------------------------

# random 2D embedding
y_bad <- matrix(rnorm(n * 2), nrow = n)
rownames(y_bad) <- rownames(X)

Q_bad <- make_Q(y_bad)

# KL divergence should be large
kl_bad2  <- KL(P2,  Q_bad)
kl_bad10 <- KL(P10, Q_bad)

c(kl_bad2, kl_bad10)

#-------------------------------------------------------------------------------
# 13. RUN ACTUAL t-SNE
#-------------------------------------------------------------------------------

tsne_fit2 <- Rtsne(
  X,
  dims = 2,
  perplexity = 2,
  check_duplicates = FALSE,
  verbose = TRUE
)

tsne_fit10 <- Rtsne(
  X,
  dims = 2,
  perplexity = 10,
  check_duplicates = FALSE,
  verbose = TRUE
)

y2  <- tsne_fit2$Y
y10 <- tsne_fit10$Y

Q2  <- make_Q(y2)
Q10 <- make_Q(y10)

# KL divergence should now be much smaller
c(KL(P2, Q2), KL(P10, Q10))

# visualize embeddings
par(mfrow = c(1,3))
plot(X, col = "black", pch = 16, main = "Original Data")
plot(y2, col = "purple", main = "t-SNE (perplexity = 2)", pch = 16)
plot(y10, col = "purple", main = "t-SNE (perplexity = 10)", pch = 16)

# Part(a)

# One can see if we put  the perplexity =50, then all points have the same mass.

# Part(b)
# we can run the same code for n = 1000 and for different perplexity.

#===============================================================================

# Question no.2 

# Load the iris data
data("iris")
X <- iris[,1:4]  # for just only numeric data

#-------------------------------------------------------------------------------
# t-SNE for iris data
#-------------------------------------------------------------------------------
tsne_fit <- Rtsne(
  X,
  dims = 2,
  perplexity = 15,
  check_duplicates = FALSE,
  verbose = TRUE
)
y <- tsne_fit$Y

# Plot t-sne dimensions reduction for iris data
plot(X, 
     col = "black",
     pch = 16, 
     main = "Original Data")

plot(y, 
     col = "purple", 
     xlab = "y1",
     ylab = "y2",
     main = "t-SNE (perplexity = 15)", pch = 16)

# When we increase perpelexity, one can see the sepration in data.

#-------------------------------------------------------------------------------
# PCA for iris data
#-------------------------------------------------------------------------------

pca_iris <- prcomp(X ,center = TRUE, scale.= TRUE)

# Plot first two PCA scores
pca_scores <- pca_iris$x
plot(pca_scores , col = iris$Species, pch = 16, 
     xlab = "PC1" , ylab  = "PC2", main = "PCA for scaled iris data")

# One can see the visualizations for  dimension of reduction 
# for iris data  is better using t-SNE.

#===============================================================================

# Question no.3

install.packages("uwot")
library(uwot)

#-------------------------------------------------------------------------------
# UMAP for iris data
#-------------------------------------------------------------------------------

umap_iris <- umap(iris[, 1:4] , n_components = 2,
                    metric = "euclidean")

# Plot for  UMAP dimensions reductions for iris data

plot(umap_iris[,1], umap_iris[,2], col = iris$Species, pch = 16, 
     xlab = "y1" , ylab  = "y2", main = "umap for  iris data")

#===============================================================================

# Question no. 4

# Load the wine data set
wine <- read.table("wine_data.txt", header = FALSE, sep = ",")

# Remove the first column
wine_data <- wine[,-1]

#-------------------------------------------------------------------------------
# PCA for the wine data
#-------------------------------------------------------------------------------

pca_wine <- prcomp(wine_data, center = TRUE , scale. = TRUE)
pca_wine_scores <- pca_wine$x

# Plot first two scores
plot(pca_wine_scores , 
     col ="red" , 
     pch = 16, 
     xlab = "PC1" , 
     ylab  = "PC2" ,
     main = "PCA for wine data")

#-------------------------------------------------------------------------------
# UMAP for wine data
#-------------------------------------------------------------------------------

umap_wine <- umap(wine_data, n_components = 2,
                  metric = "euclidean")

# Plot for UMAP dimensions reductions for wine data.
plot(umap_wine, col = "blue", pch = 16, 
     xlab = "y1" , ylab  = "y2", main = "umap for  wine data")

#-------------------------------------------------------------------------------
# t-SNE for wine data
#-------------------------------------------------------------------------------

tsne_wine <- Rtsne(wine_data,
                  dims = 2, 
                  perplexity = 15 , 
                  check_duplicates = FALSE,
                  verbose = TRUE )

# Plot for t-SNE dimensions reductions for wine data.
plot(tsne_wine$Y, col = "orange", main = "t-SNE (perplexity = 15)", pch = 16)

# One can observe from above three method t-SNE best  for separate the data.

#===============================================================================

# Question no. 5

# Install required packages and library

install.packages("IMIFA")
library(IMIFA)
install.packages("imager")
library(imager)
library(RColorBrewer)

# Load the data
data(USPSdigits)

X <- as.matrix(USPSdigits$train[, -1])
y <- USPSdigits$train[, 1]

# Here y is  vector of length 7291, and the entry of y is 
# coming from {0,1,2,3,4,5,6,7,8,9}. and X is the matrix the 
# contains  the data to create images of digits. 

#-------------------------------------------------------------------------------
# Part(a) Creating the images from data. 
#-------------------------------------------------------------------------------

foo <- matrix(X[9, ], nrow = 16, ncol = 16, byrow = TRUE)
plot(as.cimg(t(foo)))

# we can see that here, ith row is create image for the digit y[i].

#-------------------------------------------------------------------------------
# Part(b) Subset of 2000 images data
#-------------------------------------------------------------------------------
# Each row generate single image and we want to generate the data for 2000
# images, so we will draw 2000 row from X using following code.

set.seed(1) 
idx <- sample(seq_len(nrow(X)), 2000) 

cols <- brewer.pal(10, "Set3")  # for different color

#-------------------------------------------------------------------------------
# Part(c) PCA for subset data
#-------------------------------------------------------------------------------

Xsub <- X[idx, ]  # subset for 2000 image
ysub <- y[idx]

pca <- prcomp(Xsub, center = TRUE, scale. = TRUE)

scores_pca <- pca$x[, 1:2]

# Plot of the two pca  
plot(
  scores_pca,
  col = cols[as.numeric(ysub) + 1],
  pch = 19,
  xlab = "PC1",
  ylab = "PC2",
  main = "PCA (prcomp)"
)

legend(
  "topright",
  legend = 0:9,
  col = cols,
  pch = 19,
  cex = 0.8
)

#-------------------------------------------------------------------------------
# Part(d) t-SNE for subset data
#-------------------------------------------------------------------------------

 tsne <- Rtsne(
  X[idx, ],
  perplexity = 30,
  pca = TRUE,
  check_duplicates = FALSE
)
# Plot of the data after applying t-sne.
plot(
  tsne$Y,
  col = cols[as.numeric(y[idx]) + 1],
  pch = 19,
  main = "t-SNE on USPS handwritten digits"
)

legend(
  "topright",
  legend = 0:9,
  col = cols,
  pch = 19,
  cex = 0.8
)

#-------------------------------------------------------------------------------
# Part(e) UMAP for subset  data
#-------------------------------------------------------------------------------

umap_sub <- umap(Xsub, n_components = 2,
                  metric = "euclidean")

# Plot for umap dimensions reductions for subset data.
plot(umap_sub,
  col = cols[as.numeric(ysub) + 1],
  pch = 19,
  xlab = "umap1",
  ylab = "umap2",
  main = "UMAP"
)

legend(
  "topright",
  legend = 0:9,
  col = cols,
  pch = 19,
  cex = 0.8
)


#===============================================================================


