
#             Solution  Worksheet 13
#              by Saurabh Yadav (TA)
#===============================================================================

# Question no. 1

# Generate fake data according to linear regression model y=Xbeta +e.

set.seed(1)

n <- 50
p <- 5
beta.star <- c(-.5, .3, 1, 5,-2)

# Making design matrix
X <- matrix(rnorm(n*p), nrow = n, ncol = p)


# Generating response
y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)

#-------------------------------------------------------------------------------
# Part(a)  OLS estimator for beta
#-------------------------------------------------------------------------------

beta.ols <- solve(t(X)%*%X)%*%t(X)%*%y
beta.ols

#-------------------------------------------------------------------------------
#  Part(b)  For lambda = 1 ridge estimator for beta
#-------------------------------------------------------------------------------

beta.ridge1 <- solve(t(X)%*%X + diag(1,p))%*%t(X)%*%y
beta.ridge1

#-------------------------------------------------------------------------------
# Part(c)  For lambda = 10 ridge estimator for beta
#-------------------------------------------------------------------------------

beta.ridge2 <- solve(t(X)%*%X + diag(100,p))%*%t(X)%*%y
beta.ridge2

cbind(beta.ols, beta.ridge1, beta.ridge2)


#-------------------------------------------------------------------------------
# Part(d)
#-------------------------------------------------------------------------------

# One can see that as λ increases, the coefficients decrease.


#===============================================================================

# Question no. 2

n <- 50
p <- 1
beta.star <- c(1) # btrue beta is 1


# Making design matrix
X <- matrix(rnorm(n*p), nrow = n, ncol = p)

# Generating response
y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)


#-------------------------------------------------------------------------------
# Part(a) verification for OLS is unbiased estimator.
#-------------------------------------------------------------------------------

reps <- 100

beta.ols <- numeric(reps)

for(i in 1: reps){
  
  # Generating response
  y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)
  beta.ols[i] <- solve(t(X)%*%X)%*%t(X)%*%y
}

mean(beta.ols)
# one can see the  expectation of beta_hat is almost equal to 1. Hence, 
# beta_hat is unbiased estimator of beta.

#-------------------------------------------------------------------------------
# Part(b) verification for lambda = 10  ridge is biased estimator.
#-------------------------------------------------------------------------------

reps <- 100

beta.ridge <- numeric(reps)

for(i in 1: reps){
  
  # Generating response
  y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)
  
  beta.ridge[i] <- solve(t(X)%*%X + diag(10, p))%*%t(X)%*%y
}

mean(beta.ridge)

# One can see the expectation of beta_ridge is almost equal to 0.843, 
# which is not equal to 1. Hence, beta_ridge is biased estimator of beta.

#===============================================================================

# Question no. 3   Variance of OlS and Ridge estimator

#-------------------------------------------------------------------------------
# Variance of ols estimator
#-------------------------------------------------------------------------------

rep <- 100   # Replication
mean.beta.ols <- numeric(rep)

# Replication to calculate variance of OLS 

for(i in 1 : rep){
  
# replication to calculate mean of OLS
  
reps <- 100
beta.ols <- numeric(reps)
  
for(i in 1: reps){
    
# Generating response
y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)

beta.ols[i] <- solve(t(X)%*%X)%*%t(X)%*%y
                 }
  
mean.beta.ols[j] <- mean(beta_hat)
           }


#-------------------------------------------------------------------------------
# Variance of ridge estimator
#-------------------------------------------------------------------------------

rep <- 100 # Replication 
mean.beta.ridge <- numeric(rep)

for( j in 1 :rep){

# replication to calculate mean of ridge
  
reps <- 100
beta.ridge <- numeric(reps)

for(i in 1: reps){
  
# Generating response
y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)
  
beta.ridge[i] <- solve(t(X)%*%X + diag(10, p))%*%t(X)%*%y
               }

mean.beta.ridge[j] <- mean(beta.ridge)
               }


# Combine both variance to see the difference

cbind(var(mean.beta.ols),var(mean.beta.ridge))
# One can see ridge estimator has smaller varaince than ols estimator.

#===============================================================================

# Question no. 4 Simulation study for comparing OLS and Ridge MSE

set.seed(123)

# Given data setup

n <- 50
p <- 1
beta.star <- c(1)   # true beta

# Design matrix
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# Grid of lambda values

lambda.grid <- seq(0, 20, length = 10)

# Number of  repetitions
B <- 1000

# Store MSE values
mse.ols <- numeric(B)
mse.ridge <- matrix(0 , nrow = B , ncol = length(lambda.grid))

# ------------------------------------------------------------------------------
# MSE of OLS estimator 
# ------------------------------------------------------------------------------
for (b in 1:B) {
  
# Generate new response each time
y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)
  
# OLS estimator
beta.ols <- solve(t(X) %*% X) %*% t(X) %*% y
  
# Squared error
mse.ols[b] <- sum((beta.ols - beta.star)^2)  
              
}

# Average OLS MSE

mean.mse.ols <- mean(mse.ols)

# ------------------------------------------------------------------------------
# Mse of Ridge estimator
# ------------------------------------------------------------------------------

for (j in 1:length(lambda.grid)) {
  
lambda <- lambda.grid[j]
  
for (b in 1:B) {
    
# Generate new response
y <- X %*% beta.star + rnorm(n, mean = 0, sd = 1)
    
# Ridge estimator
beta.ridge <- solve(t(X) %*% X + lambda * diag(p)) %*% t(X) %*% y
    
# Squared error
mse.ridge[b,j] <- sum((beta.ridge - beta.star)^2)
}
}

y <- colMeans(mse.ridge)

# ------------------------------------------------------------------------------
# Plot  mse of ols vs mse of ridge for different values of lambda
# ------------------------------------------------------------------------------

plot(lambda.grid, y,
     type = "l",
     lwd = 2,
     col = "blue",
     ylim = range(mean.mse.ols,max(y)),
     xlab = expression(lambda),
     ylab = "MSE",
     main = "MSE of Ridge vs OLS")

abline(h = mean.mse.ols, lty = 1, lwd = 2, col = "red")

legend("topleft",
       legend = c("Ridge MSE", "OLS MSE"),
       col = c("blue", "red"),
       cex = 0.8,
       lwd = 2)








