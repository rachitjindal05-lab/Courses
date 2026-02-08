##         Worksheet 3 Solutions        ##
##         by Saurabh Yadav  (TA)       ##
#=======================================================

# Question no.1

# Generates Marks for MTH208 student.

set.seed(123)  

n <- 100

age <- sample(19:20, n, replace = TRUE)
mth_208_marks <- round(runif(n, min = 0, max = 100), 1)

# plot age vs MTH208 marks
plot( mth_208_marks , age ,
  xlab = "MTH 208 Marks",
  ylab = "Age",
  ylim = c(10 , 100),
  main = "Age vs MTH 208 Marks",
  pch = 16,
)


# Generate Marks for MTH209 which depend to Marks of MTH208
mth_209_marks <- 0.85 * mth_208_marks + rnorm(n, mean = 5, sd = 5)

# Keep Marks within 0–100
mth_209_marks <- round(pmin(pmax(marks2, 0), 100) , 1)

# plot for MTH209 vs MTH208 marks.

plot( mth_208_marks , mth_209_marks ,
  xlab = "Marks MTH209",
  ylab = "Marks MTH208",
  pch = 16,
  main = "Marks MTH208 vs Marks MTH209"
)

# Combine into data frame
student_data <- data.frame(
  Marks1 = mth_208_marks,
  Marks2 = mth_209_marks
)

# Center the data 

X <- scale(student_data, center = TRUE, scale = FALSE)

# PCA via covariance eigen-decomposition

sigma_hat <- cov(X)
eig <- eigen(sigma_hat)

# First principal component direction
v1 <- eig$vectors[, 1]

# Projection 
X_proj <- X %*% v1 %*% t(v1)

# Plot centered data, PC1 line, projections

plot(
  X[, 1], X[, 2],
  pch = 16,
  xlab = "Centered MTH209 Marks",
  ylab = "Centered MTH208 Marks",
  main = ""
)

# First PC line (red)
t_vals <- seq(min(X %*% v1), max(X %*% v1), length.out = 100)
pc_line <- t_vals %*% t(v1)
lines(pc_line[, 1], pc_line[, 2], col = "red", lwd = 2)

# Orthogonal projections 
segments(
  X[, 1], X[, 2],
  X_proj[, 1], X_proj[, 2],
  col = "gray70"
)

# Projected points 
points(X_proj[, 1], X_proj[, 2], col = "red", pch = 16)


#========================================================

#Questio no.2

data(iris)
plot(iris)
plot(iris, col = iris$Species)

# PCA on the iris dataset (manual implementation)

X <- iris[, 1:4]   # Extract only the first four columns

# (a) Compute the sample covariance matrix sigma hat

sigma_hat <- cov(X)
sigma_hat

# (b) Obtain eigenvalues and eigenvectors of sigma hat

eig <- eigen(sigma_hat)
eig$values        # Eigenvalues 
eig$vectors       # Eigenvectors


# (c) Choose k = 2 eigenvectors and  principal components.
#Select the first two eigenvectors (corresponding to largest eigenvalues)
W <- eig$vectors[, 1:2]

# Center the data (for PCA)
X_centered <- scale(X, center = TRUE, scale = FALSE)

# Compute PCA scores (project data onto principal components)
scores_manual <- X_centered %*% W 

head(scores_manual)   # View first few scores


# (d) Verify results using prcomp function.

# PCA using prcomp function
pca_prcomp <- prcomp(X, center = TRUE, scale. = FALSE)

# Compare eigenvalues
pca_prcomp$sdev^2    # eigen values

# Compare eigenvectors (loadings)
pca_prcomp$rotation[, 1:2]  #eigenvector 

# PCA using prcomp function. 
head(pca_prcomp$x[, 1:2])

# (e) Reconstruct data using all 4 PCs and compute eigenvalues
# of reconstructed covariance matrix

# Use all eigenvectors
W_full <- eig$vectors

# Reconstruct centered data using all components
X_reconstructed <- X_centered %*% W_full %*% t(W_full)

# Original reconstruted matrix.
X_reconstructed <- X_reconstructed + matrix(colMeans(X),
                    nrow = nrow(X),ncol = ncol(X),byrow = TRUE)

# Compute covariance of reconstructed data
sigma_reconstructed <- cov(X_reconstructed)

# Eigenvalues of reconstructed covariance matrix
eigen(sigma_reconstructed)$values

# The eigenvalues of the reconstructed covariance matrix
# are identical to the original covariance matrix. Because
# PCA is an orthogonal rotation of the coordinate system.

#===========================================================

# Question no.3
source("pca_plots.R")  # To call function plot_loading
plot_loadings(eig$vectors[ ,1:2] , colnames(X))

#===========================================================

# Question no. 4

# From the loadings plot, the first pca represents overall flower size,
# dominated by petal length and petal width.

#=========================================================

# Question no. 5

screeplot(pca_prcomp) 

# From plot one can observe that only one eigen value is large other are 
# very closed to zero. So k=1.

#=======================================================

# Question no.6 Wine Dataset PCA

# Load wine data 

wine <- read.table("wine_data.txt", header = FALSE, sep = ",")

# Remove the first colomn

wine_data  <- wine[, -1]

#  6a PCA without scaling

pca_wine <- prcomp(wine_data, center = TRUE, scale. = FALSE)
pca_wine


# 6b Scale the data 
wine_scaled <- scale(wine_data)

# 6c PCA with scaled data
pca_wine_scaled <- prcomp(wine_scaled, center = TRUE, scale. = FALSE)
pca_wine_scaled

# 6d Plot first two PC scores colored by wine class

scores_wine <- pca_wine_scaled$x
plot(scores_wine[,1], scores_wine[,2],
     col = wine_class,
     pch = 19,
     xlab = "PC1",
     ylab = "PC2",
     main = "Wine Data PCA (Scaled)")

legend("topright",
       legend = unique(wine_class),
       col = unique(wine_class),
       pch = 19)

# 6e choice of k.

screeplot(pca_wine_scaled)
# Based on the screeplot the choice for k is 3.


#================================================

# Question no.7 Grocery Basket Data PCA

# Load grocery basket data 
X <- read.csv("my_basket.csv")
source("pca_plots.R")
# variance covariance matrix
sigma_hat <- cov(X) 

# eigenvalue and eigen vectors of sigma_hat
eigen(sigma_hat)

# PCA for grocery data

pca_grocery <- prcomp(grocery, center = TRUE, scale. = TRUE)

# 7a Plot loadings for first PCA

plot_loadings(eig$vectors[,1] , colnames(X))

# First PC represents overall beverage consumption, dominated by pepsi.

# 7b Plot first two PC scores

plot_twoPC(eig$vectors[, 1:2], colnames(X))


# 7c Choose number of PCs 
# number of Pca is 1.







