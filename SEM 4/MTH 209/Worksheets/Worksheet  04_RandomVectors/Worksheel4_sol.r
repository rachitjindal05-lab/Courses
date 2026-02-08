#####Q2###############################
n <- 5000
mu <- c(0, 0)
Sigma <- matrix(c(1, .8, .8, 1), nrow = 2)

#independent standard normals
Z <- matrix(c(rnorm(n), rnorm(n)), ncol = 2)
#Cholesky decomposition
A <- chol(Sigma)

# Step 3: linear transformation
X <- Z %*% A + matrix(mu, n, 2, byrow = TRUE)
###Scatter Plot
plot(X, asp = 1, main = "Data cloud", pch = 16, col="black")

####Sample Covariance matrix
# by difinition
mean_vec <- colMeans(X)
Xc <- X - matrix(mean_vec, nrow=n, ncol = 2, byrow = T)
S <- (1/(n-1))*t(Xc)%*%Xc
#by cov
S_c <- cov(X)
#verify
S_c-S

####Sample Correlation matrix
#by definition
sd_vec <- sqrt(diag(S))
R <- S / (sd_vec %*% t(sd_vec))
###by cor
R_c <- cor(X)
##verify
R_c-R
#####
Sigma2 <- matrix(c(1, 0, 0, 1), nrow = 2)
Z <- matrix(c(rnorm(n), rnorm(n)), ncol = 2)
A2 <- chol(Sigma2)
X2 <- Z %*% A2 + matrix(mu, n, 2, byrow = TRUE)
#####
cov(X2)
cor(X2)

###Scatter Plot
points(X2, pch = 16, col="orange")

######Q3################################
n <- 5000
theta <- runif(n,0,2*pi)
X1 <- cos(theta) 
X2 <- sin(theta)
X <- matrix(c(X1,X2), ncol = 2)
plot(X, asp = 1, main = "Data cloud", pch = 16, col="red")
cov(X)
cor(X)

###Q4######################################
data("EuStockMarkets")
X <- as.matrix(EuStockMarkets)
cov_Eu <- cov(X)
cor_Eu <- cor(X)
log_X <- log(X)
n <- nrow(X)
p <- ncol(X)
log_returns <- matrix(0, n-1, p)
for (t in 2:n) {
  log_returns[t-1, ] <- log_X[t, ] - log_X[t-1, ]
}
###another way to compute the difference is
log_returns <- log_X[2:n,]-log_X[1:n-1,]

cor_log <- cor(log_returns)
##heatmap
install.packages("corrplot")
library(corrplot)
corrplot(
  cor_Eu,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)
corrplot(
  cor_log,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

#############Q5###########################
data_wine <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
#setwd(" ")
wine <- read.table("wine.data", sep = ",", header = FALSE)

X <- as.matrix(wine)
dim(X)
cov_mat <- cov(X)
cor_mat <- cor(X)
# PCA 
pca <- prcomp(X, scale. = FALSE)
summary(pca)
##
pca_cor <- prcomp(cor_mat)
scores <- pca$x
class <- as.numeric(wine[,1])
cols <- c("red","blue","green")

plot(scores[,1], scores[,2],
     col = cols[class],
     pch = 19,
     xlab="PC1", ylab="PC2")

legend("topright",
       legend=c("Class 1","Class 2","Class 3"),
       col=cols, pch=19, cex = 0.25)

