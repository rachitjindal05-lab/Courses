
#                   Worksheet9 Solution
#                   By Saurabh Yadav

#===============================================================================

# Question no.1

k_means_cluster <- function(X , k , centroids = NULL ,  maxit = 100, tol = 1e-6) {

# Convert to matrix
X <- as.matrix(X)
n <- nrow(X)
d <- ncol(X)

# Random initialization of centroids

if (is.null(centroids)) {
  set.seed(1)
  idx <- sample(1:n, k)
  centroids <- X[idx, ]
}

cluster <- rep(0, n)

  for (iter in 1:maxit) {

    for (i in 1:n) {

      dist <- rep(0, k)    # null vector for distance

      for (j in 1:k) {
        dist[j] <- sum((X[i, ] - centroids[j, ])^2)
      }

      cluster[i] <- which.min(dist)
    }

# Calculate new centroid

new_centroids <- matrix(0, k, d)

    for (j in 1:k) {
      X_sub <- as.matrix(X[cluster == j, ])

      if (nrow(X_sub) > 0) {
        new_centroids[j, ] <- colMeans(X_sub)
                           }
      else {
        new_centroids[j, ] <- centroids[j, ]
           }
    }

# Breaking Rule

if (sum((new_centroids - centroids)^2) < tol) {
      break
    }
centroids <- new_centroids
   }
return(list(
    cluster = cluster,
    Centroid = centroids,
    iterations = iter
  ))
}

# Example

X <- matrix(rnorm(1000, 0, 1), nrow = 100 , ncol = 10)

k_means_cluster(X,k =2)

#===============================================================================

# Question no. 2

# Load the dataset

data(faithful)

# This data contains duration eruptions and waiting time for next eruptions

X <- faithful
head(X)
dim(X)
# Part (a) K-means(k=2) clustering using my function

result <- k_means_cluster(X, k = 2)
result

# Part (b) Using kmeans function

kmeans_result <- kmeans(X , centers = 2)

kmeans_result$centers
kmeans_result$iter

# Extract all point for cluster1
X_sub <- as.matrix(X[A==1,])


# Plot the point for both two cluster
plot(X,
     col = kmeans_result$cluster,
     pch = 19,
     xlab = "Eruption Duration",
     ylab = "Waiting Time",
     main = "Clusters1 and 2")

points(kmeans_result$centers,
       col = "green",
       pch = 19,
       cex = 2)

#===============================================================================

# Question no. 3

data <- read.csv("battingbowling.csv")
head(data)

# select numeric features

X <- as.matrix(data[, c("Bowling","Batting")])


# K-mean (say k=2) using functions
kmeans_result2 <- kmeans(X, centers = 2)
kmeans_result2$centers
kmeans_result2$size

#one can see k=2 identifies batters and Bowlers.

# k=3-means using function
kmeans_result3 <- kmeans(X, centers = 3)

kmeans_result3$centers
kmeans_result3$size

# similarly one can see  k=3 identifies batter, bowler and Allrounder.

#===============================================================================

# Question no. 4

data("USArrests")

X <- USArrests
dim(X)

# k-means clustering using function
kmeans_result <- kmeans(X[,-1], centers = 3)

# Extract center for each cluster
kmeans_result$centers

# Size of cluster
kmeans_result$size

#===============================================================================

# Question no.5

library(imager)

# Load the image

img <- load.image("best-IKEA-living-room.jpg")
plot(img)
dim(img)
# Extract red color for each pixels

R <- as.vector(img[,,1,1])
G <- as.vector(img[,,1,2])
B <- as.vector(img[,,1,3])

# We have created a matrix D which has 3 column
D <- matrix(c(R,G,B), nrow= length(R), ncol =3)
colnames(D) <- c("R", "G", "B")

# Run k-means clustering
set.seed(1)
k <- 9
kmeans_result <- kmeans(D, centers = k)

# Extract palette colors for each cluster
palette_colors <- kmeans_result$centers
palette_colors 
Size <- kmeans_result$size
Size
# Display palette colors
plot(1:k, rep(1,k),
     col = rgb(palette_colors),
     pch = 15,
     cex = 7,
     xlab = "Number of cluster" , ylab = "")

#===============================================================================

# Question no. 6

# Install required package or library

library(Rtsne)
library(class) 

# Load  the dataset

load("digits_truth.RData")

# Check the dimensions for training and test.

dim(digits_train)
dim(digits_test_truth)

# Extract training feature

train_x <- digits_train[, -1]
dim(train_x)

# Extract response corresponding  training feature

train_y <- digits_train[, 1]
length(train_y)

# Extract test feature

test_x  <- digits_test_truth[, -1]
dim(test_x)

# Extract the truth

truth_y  <- digits_test_truth[, 1]
length(truth_y)

# Prediction Using knn algorithms

k <- 5

pred_knn1 <- knn(train_x, test_x, train_y, k = k)

# Error

error_rate1 <- mean(pred_knn1 != truth_y)
error_rate1

# Next we will do dimensions reduction then prediction using KNN.

set.seed(123)

library(Rtsne)

train_test <- rbind(train_x , test_x)


tsne <- Rtsne(train_test, dims = 3, perplexity = 30, verbose = TRUE)

# After dimension reduction

red_train <- as.matrix(tsne$Y[1:2000 ,])
dim(red_train)

red_test <- as.matrix(tsne$Y[2001:3000 , ])
dim(red_test)

# K-NN algorithm for red_train and red_test

pred_knn2 <- knn(red_train , red_test , train_y , k=k)


error_rate2 <- mean(pred_knn2 != truth_y)
error_rate2


# dimensions reduction using pca

# Standardize data (VERY IMPORTANT for PCA)

X_scaled <- scale(train_test)

# Run PCA

pca_model <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# Summary (variance explained)

summary(pca_model)

# Principal components (transformed data)

X_pca <- pca_model$x

# Example: take first 6 components

X_pca_6d <- X_pca[, 1:6]
dim(X_pca_6d)

pca_train <- as.matrix(X_pca_6d[1:2000,])

pca_test <- as.matrix(X_pca_6d[2001:3000,])

pred_knn3 <- knn(pca_train , pca_test , train_y , k=k)


error_rate3 <- mean(pred_knn3 != truth_y)
error_rate3

#===============================================================================












