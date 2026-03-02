
##                        Worksheet7 Solutions           
##                        by Saurabh Yadav (TA)          
#===============================================================================

# Question no1. We have two estimator T1(sample mean ) and T2(sample median) for
# central parameter of t3, we want to compare their MSE.

#-------------------------------------------------------------------------------
# Part(a) 
#-------------------------------------------------------------------------------

set.seed(123)

n <- 10
reps <- 100  # number of replications

store_mean <- numeric(reps)    # memory allocation
store_median <- numeric(reps)

for( r in 1: reps){
  sample <- rt(n, df = 3)
  
  store_mean[r] <-mean(sample) 
  store_median[r] <- median(sample)
}

truth <- 0    # True value for central parameter

# MSE for sample mean
mean_mse <- mean((store_mean-truth)^2)
mean_mse

# MSE for sample median 
median_mse <- mean((store_median-truth)^2)
median_mse 

# From above implimentation one can see, T2 (sample median) is better.

#-------------------------------------------------------------------------------
# Part(b)
#-------------------------------------------------------------------------------

# One can see, if we increase the value of sample size still our 
# T2 (sample median) is better.

#-------------------------------------------------------------------------------
#  Part(c)  Reapet part(a) for central parameter of N(0,1)
#-------------------------------------------------------------------------------

n <- 100
reps <- 100  # number of replications

store_mean <- numeric(reps)    # memory allocation
store_median <- numeric(reps)

for( r in 1: reps){
  sample <- rnorm(n, 0 , 1)
  
  store_mean[r] <-mean(sample) 
  store_median[r] <- median(sample)
}

truth <- 0    # True value for central parameter

# MSE for sample mean
mean_mse <- mean((store_mean-truth)^2)
mean_mse

# MSE for sample median 
median_mse <- mean((store_median-truth)^2)
median_mse 

# One can see, in case of N(0,1) T1(sample mean ) is better.


#-------------------------------------------------------------------------------
# Part(d)
#-------------------------------------------------------------------------------

mean_vs_median <- function(n.vec) {
  
truth <- 0
k  <- length(n.vec)
  
mse.mat <- matrix(0, nrow = k, ncol = 2)
colnames(mse.mat) <- c("mse_mean", "mse_median")
  
  for (i in 1:k) {
    
    n <- n.vec[i]
  
    x <- rnorm(n, mean = 0, sd = 1)
    
 mse.mat[i, 1] <- (mean(x) - truth)^2
 mse.mat[i, 2] <- (median(x) - truth)^2
  }
  
  return(mse.mat)
}


#-------------------------------------------------------------------------------
# Part(e)
#-------------------------------------------------------------------------------

reps <- 1e3
n.vec <- c(10, 50 , 100, 200, 500, 1000 , 2000, 5000)
k <- length(n.vec)

mse <- matrix(0, nrow = k, ncol = 2)

for (r in 1: reps) {
  
  mse.sum <- mse + mean_vs_median(n.vec)
}
avg.mse <- mse.sum/reps
avg.mse


#===============================================================================

# Question no. 2 (Question no.1 part(e) using embarrassingly parallel)

library(foreach)
library(doParallel)

ncores <- parallel::detectCores() - 3
cl <- makeCluster(ncores)
registerDoParallel(cl)


# Parallel replications
mse.sum.par <- foreach(r = 1:reps,
                       .combine = "+",
                       .packages = "stats") %dopar% {
                         
                         mean_vs_median(n.vec)
                       }

avg.mse.par <- mse.sum.par / reps

print(avg.mse.par)

stopCluster(cl)

#===============================================================================

#Question no.3

set.seed(123)
x <- rnorm(100 , 0 , 1)

# Maximum/ minimum / median for x 
max(x)
min(x)
median(x)

# Divide the data into four part

x1 <- x[1:25]
x2 <- x[26:50]
x3 <- x[51:75]
x4 <- x[76:100]

# first we take maximum/ minimum / Median  for all four part then combine.
combined_max <- max(max(x1),  max(x2), max(x3), max(x4))
combined_max
combined_min <- min(min(x1),  min(x2), min(x3), min(x4))
combined_min
combined_median <- median(c(median(x1) , median(x2) , median(x3) , median(x4)))
combined_median

# One can see that Combined_max= max, combined_min = min, but combined_median
# is not equal to median. So median is not  embarassingly parallel. 

#===============================================================================

# Question no.4

#-------------------------------------------------------------------------------
# Part (a): Generate data from the true model
#-------------------------------------------------------------------------------

set.seed(1)

n <- 1000
p <- 100

# TRUE PARAMETERS

beta_true <- rnorm(p, mean = 0, sd = 1)   # true beta
sigma_square <- 1                         # true variance

#  DESIGN MATRIX 

X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# Generate response from model 

epsilon <- rnorm(n, mean = 0, sd = 1)

y <- X %*% beta_true + epsilon


#-------------------------------------------------------------------------------
# Part (b) OLS estimator
#-------------------------------------------------------------------------------

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y


#-------------------------------------------------------------------------------
# Part (c)  estimate of average MSE
#-------------------------------------------------------------------------------

reps <- 1e3
mse_values <- numeric(reps)

for (i in 1:reps) {
  
  epsilon <- rnorm(n, mean = 0, sd = 1)
  
  y <- X %*% beta_true + epsilon
  
  
mse_values[i] <- sum((beta_hat - beta_true)^2)
     }

avg_mse <- mean(mse_values)

cat("Estimated Average MSE =", avg_mse, "\n")

#===============================================================================

# Question no.5  Average mse using foreach

library(foreach)
library(doParallel)

ncores <- parallel::detectCores() - 3
cl <- makeCluster(ncores)
registerDoParallel(cl)

reps <- 1000

# Parallel computation
mse_values <- foreach(r = 1:reps,
                      .combine = c,
                      .export = c("X", "beta_true", "beta_hat", "n")) %dopar% {
                        
                        epsilon <- rnorm(n, mean = 0, sd = 1)
                        
                        y <- X %*% beta_true + epsilon
                        
                        sum((beta_hat - beta_true)^2)
                      }

avg_mse <- mean(mse_values)

cat("Estimated Average MSE =", avg_mse, "\n")

stopCluster(cl)

#===============================================================================

# Question no. 6 verify that ols estimator is not embarrassingly parallel

set.seed(1)

#--------------------------------------------------------------
#  Generate data from linear regression model
#--------------------------------------------------------------

n <- 100
p <- 20

beta_true <- rnorm(p)

X <- matrix(rnorm(n*p), nrow = n, ncol = p)
epsilon <- rnorm(n)

y <- X %*% beta_true + epsilon


#--------------------------------------------------------------
#  global ols estimator beta_hat = (X'X)^(-1) X'y
#--------------------------------------------------------------

beta_global <- solve(t(X) %*% X) %*% t(X) %*% y

#--------------------------------------------------------------
#  Split data into 4 parts
#--------------------------------------------------------------

K <- 4
index_list <- split(1:n, rep(1:K, length.out = n))


#--------------------------------------------------------------
# Compute local OLS estimator on each part
#--------------------------------------------------------------

beta_local <- vector("list", K)   # create empty list

for (k in 1:K) {
  
  ind <- index_list[[k]]
  
  Xk <- X[ind, ]
  yk <- y[ind]
  
  beta_local[[k]] <- solve(t(Xk) %*% Xk) %*% t(Xk) %*% yk
}

#--------------------------------------------------------------
#  average of local estimators
#--------------------------------------------------------------

beta_average <- matrix(0, nrow = p, ncol = 1)

for (k in 1:K) {
  beta_average <- beta_average + beta_local[[k]]
}

beta_average <- beta_average / K

#--------------------------------------------------------------
#  Compare global vs combined estimator
#--------------------------------------------------------------

difference <- norm(beta_global - beta_average, type = "2")

cat("Difference between estimators =", difference, "\n")

# One can see global beta is not same as average of local beta's. Hence OLS 
# estimator is not embarrassingly parallel.






