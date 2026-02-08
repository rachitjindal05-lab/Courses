library(ggplot2)
library(Rtsne)

############################################################
# 1. CONDITIONAL PROBABILITIES p_{j|i} IN HIGH DIMENSIONS
############################################################

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

############################################################
# 2. LOW-DIMENSIONAL AFFINITIES Q
############################################################

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

############################################################
# 3. ENTROPY AND PERPLEXITY
############################################################

# Shannon entropy of a probability mass function
entropy <- function(p, eps = 1e-12) {
  p <- p[p > eps]      # avoid log(0)
  -sum(p * log2(p))
}

############################################################
# 4. FIND sigma^2 FOR A GIVEN PERPLEXITY
############################################################

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

############################################################
# 5. KULLBACK–LEIBLER DIVERGENCE
############################################################

# KL(P || Q) = sum_{ij} p_{ij} log(p_{ij} / q_{ij})
KL <- function(P, Q) 
{
  idx <- P > 0
  sum(P[idx] * log(P[idx] / Q[idx]))
}

############################################################
# 6. GENERATE A SIMPLE TWO-CLUSTER DATASET
############################################################

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

############################################################
# 7. PAIRWISE DISTANCES
############################################################

# Euclidean distance matrix
D <- as.matrix(dist(X))

############################################################
# 8. FIND LOCAL BANDWIDTHS sigma_i^2
############################################################

# small perplexity → local neighborhoods
# large perplexity → more global neighborhoods
sigma2_vec2  <- as.numeric(apply(D, 1, find_sigma2, 2))
sigma2_vec10 <- as.numeric(apply(D, 1, find_sigma2, 100))

# compare bandwidths
cbind(sigma2_vec2, sigma2_vec10)

############################################################
# 9. CONDITIONAL PROBABILITIES p_{j|i}
############################################################

P_cond2  <- matrix(0, n, n)
P_cond10 <- matrix(0, n, n)

for (i in 1:n) {
  P_cond2[i, ]  <- conditional_probs(D[i, ], sigma2_vec2[i])
  P_cond10[i, ] <- conditional_probs(D[i, ], sigma2_vec10[i])
}

round(P_cond2, 3)

############################################################
# 10. VISUALIZE CONDITIONAL DISTRIBUTIONS
############################################################

i <- 11  # reference point

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

############################################################
# 11. SYMMETRIZED JOINT PROBABILITIES P_{ij}
############################################################

# t-SNE uses:
#
#   P_{ij} = (p_{j|i} + p_{i|j}) / (2n)
#
P2  <- (P_cond2  + t(P_cond2))  / (2 * n)
P10 <- (P_cond10 + t(P_cond10)) / (2 * n)

sum(P2)   # should be 1
sum(P10)

############################################################
# 12. BAD EMBEDDING EXAMPLE
############################################################

# random 2D embedding
y_bad <- matrix(rnorm(n * 2), nrow = n)
rownames(y_bad) <- rownames(X)

Q_bad <- make_Q(y_bad)

# KL divergence should be large
kl_bad2  <- KL(P2,  Q_bad)
kl_bad10 <- KL(P10, Q_bad)

c(kl_bad2, kl_bad10)

############################################################
# 13. RUN ACTUAL t-SNE
############################################################

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
