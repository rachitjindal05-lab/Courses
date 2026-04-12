
#Q1
library(MASS) # for generating multivariate normal
set.seed(209)
Sigma <- matrix(c(2,1,1,4), nrow = 2, ncol = 2)
mu1 <- c(2, 3)
mu2 <- c(-1, 4)
n1 <- 50
n2 <- 75


# generate xs
x1 <- mvrnorm(n1, mu = mu1, Sigma = Sigma)
x2 <- mvrnorm(n2, mu = mu2, Sigma = Sigma)

# fix ys
y1 <- rep(1, n1)
y2 <- rep(2, n2)
y <- c(y1, y2)
X <- rbind(x1, x2)
# Plot
plot(X, col = y, pch = 16)

#(a)
K <- 2
n <- n1 + n2
pi1_hat <- n1/n 
pi2_hat <- n2/n

mu1_hat <- colSums(X[y==1,])/n1
mu2_hat <- colSums(X[y==2,])/n2

x1c <- x1 - matrix(mu1_hat, nrow = n1, ncol=2, byrow = T)
x2c <- x2 - matrix(mu2_hat, nrow = n2, ncol=2, byrow = T)

sigma_hat <- ((t(x1c) %*% (x1c)) +(t(x2c) %*% (x2c))) / (n - 2)

###for any K

Sigma_hat <- matrix(0, ncol = 2, nrow = 2)

for (k in 1:K) {
  Xk <- X[y == k, ]
  muk <- colMeans(Xk)
  
  nk <- nrow(Xk)
  Xkc <- Xk-matrix(muk, nrow = nk, ncol=2, byrow = T)
  
    Sigma_hat <- Sigma_hat + t(Xkc) %*% Xkc
    
  }
Sigma_hat <- Sigma_hat / (n - K)


##(b)
decision_boundary <- function(x) {
  
  x <- matrix(x, ncol = 1)   # ensure column vector
  
  sigma_inv <- solve(sigma_hat)
  
  term1 <- t(mu1_hat - mu2_hat) %*% sigma_inv %*% x
  
  term2 <- (t(mu1_hat) %*% sigma_inv %*% mu1_hat - t(mu2_hat) %*% sigma_inv %*% mu2_hat)/2
  
  term3 <- log(pi1_hat / pi2_hat)
  
  diff_delta <- term1 - term2 + term3
  if (diff_delta > 0) {
    return("delta1-delta2 > 0")
  } else {
    return("delta1-delta2 < 0 ")
  } 
  }

y_hat <- apply(X, 1, function(x) {
  res <- decision_boundary(x)
  
  if (res == "delta1-delta2 > 0") {
    return(1)
  } else {
    return(2)
  }
})
y_hat
#mis-classification rate
mean(y_hat!=y)

##Q2
data(iris)
cov_setosa     <- cov(iris[iris$Species == "setosa", 1:4])
cov_versicolor <- cov(iris[iris$Species == "versicolor", 1:4])
cov_virginica  <- cov(iris[iris$Species == "virginica", 1:4])
cov_setosa
cov_versicolor
cov_virginica


library(MASS)
library(klaR)
lda_fit_iris <- lda(Species ~ ., data = iris)
# Plot pairwise decision boundaries
partimat(Species ~ ., data = iris, method = "lda")
pred_iris <- predict(lda_fit_iris)
lda_mis_rate_iris <- mean(pred_iris$class != iris$Species)
lda_mis_rate_iris

####Q3
data(B3)


lda_fit_B3 <- lda(PHASEN ~ ., data = B3)
lda_pred_B3 <- predict(lda_fit_B3)$class
lda_mis_rate_B3 <- mean(lda_pred_B3 != B3$PHASEN)
lda_mis_rate_B3


library(class)

# Remove label column
X <- B3[, -which(names(B3) == "PHASEN")]
y <- B3$PHASEN

# kNN prediction (k = 5, can try others)
knn_pred <- knn(train = X, test = X, cl = y, k = 5)

knn_mis_rate <- mean(knn_pred != y)
knn_mis_rate


###Q4
qda_fit_iris <- qda(Species ~ ., data = iris)
# Plot pairwise decision boundaries
partimat(Species ~ ., data = iris, method = "qda")
qda_pred_iris <- predict(qda_fit_iris)
qda_mis_rate_iris <- mean(qda_pred_iris$class != iris$Species)
qda_mis_rate_iris

###B3
qda_fit_B3 <- qda(PHASEN ~ ., data = B3)
qda_pred_B3 <- predict(qda_fit_B3)$class
qda_mis_rate_B3 <- mean(qda_pred_B3 != B3$PHASEN)
qda_mis_rate_B3

