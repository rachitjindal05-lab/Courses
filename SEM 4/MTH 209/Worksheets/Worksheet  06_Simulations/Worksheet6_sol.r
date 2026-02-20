#Q1...Law of Large Number

set.seed(123)

running_mean <- function(n, df) {
  x <- rt(n, df = df)   # generate one long sample
  cumsum(x) / seq_along(x)
}

n_values <- c(100, 500, 1000, 5000, 10000)
Nmax <- max(n_values)

#a. t3 distribution
rm_t3 <- running_mean(Nmax, 3)

plot(rm_t3, type="l",
     xlab="n",
     ylab="Running Mean",
     main="Running Mean: t3 Distribution")

abline(h = 0, col="red", lwd=2)


#b. t2 distribution
rm_t2 <- running_mean(Nmax, 2)

plot(rm_t2, type="l",
     xlab="n",
     ylab="Running Mean",
     main="Running Mean: t2 Distribution")

abline(h = 0, col="orange", lwd=2)


# c. t1 distribution
rm_t1 <- running_mean(Nmax, 1)

plot(rm_t1, type="l",
     xlab="n",
     ylab="Running Mean",
     main="Running Mean: t1 Distribution")

abline(h = 0, col="blue", lwd=2)


###Q2 
# sample size
n <- 50        
# number of replications
R <- 10000     

sample_means <- numeric(R)

for (r in 1:R) {
  x <- rnorm(n, mean = 0, sd = 1)
  sample_means[r] <- mean(x)
}

# Estimated mean and variance
mean_est <- mean(sample_means) #should be close to 0
var_est  <- var(sample_means)  # should be close to sigma^2/n

##Q3
set.seed(123)
R <- 5000
n_values <- c(10, 100, 500)
## N(0,1)

par(mfrow=c(1,3))

for (n in n_values) {
  
  Z <- numeric(R)
  
  for (r in 1:R) {
    x <- rnorm(n)
    Z[r] <- sqrt(n) * mean(x)
  }
  
  hist(Z, prob=TRUE,
       main=paste("Normal, n =", n),
       xlab="")
  
  curve(dnorm(x, 0, 1), col="red", lwd=2, add=TRUE)
}
 # t3

par(mfrow=c(1,3))

for (n in n_values) {
  
  Z <- numeric(R)
  
  for (r in 1:R) {
    x <- rt(n,3)
    Z[r] <- sqrt(n) * mean(x)
  }
  
  hist(Z, prob=TRUE, 
       main=paste("t3, n =", n),
       xlab="")
  
  curve(dnorm(x, 0, sqrt(3)), col="red", lwd=2, add=TRUE)
}
## Gamma
a <- 100
par(mfrow=c(1,3))

for (n in n_values) {
  
  Z <- numeric(R)
  
  for (r in 1:R) {
    x <- rgamma(n, shape=a, rate=1)
    Z[r] <- sqrt(n) * (mean(x) - a)
  }
  
  hist(Z, prob=TRUE, 
       main=paste("Gamma, n =", n),
       xlab="")
  
  curve(dnorm(x, 0, sqrt(a)),
        col="red", lwd=2, add=TRUE)
}

a <- 10
par(mfrow=c(1,3))

for (n in n_values) {
  
  Z <- numeric(R)
  
  for (r in 1:R) {
    x <- rgamma(n, shape=a, rate=1)
    Z[r] <- sqrt(n) * (mean(x) - a)
  }
  
  hist(Z, prob=TRUE, 
       main=paste("Gamma, n =", n),
       xlab="")
  
  curve(dnorm(x, 0, sqrt(a)),
        col="red", lwd=2, add=TRUE)
}

###t2--infinite variance
par(mfrow=c(1,3))

for (n in n_values) {
  
  Z <- numeric(R)
  
  for (r in 1:R) {
    x <- rt(n, df=2)
    Z[r] <- sqrt(n) * mean(x)
  }
  
  hist(Z, prob=TRUE, breaks=40,
       main=paste("t2, n =", n),
       xlab="")
  
}


###Q4
set.seed(123)

R <- 1000
n <- 10
mu <- 0

mse_mean <- numeric(R)
mse_median <- numeric(R)

for (r in 1:R) {
  x <- rt(n, df = 3)
  
  T1 <- mean(x)
  T2 <- median(x)
  
  mse_mean[r] <- (T1 - mu)^2
  mse_median[r] <- (T2 - mu)^2
}

cat("MSE of sample mean:", mean(mse_mean), "\n")
cat("MSE of sample median:", mean(mse_median), "\n")

##b
n_values <- c(10, 30, 50, 100)

for (n in n_values) {
  
  mse_mean <- numeric(R)
  mse_median <- numeric(R)
  
  for (r in 1:R) {
    x <- rt(n, df = 3)
    
    mse_mean[r] <- (mean(x))^2
    mse_median[r] <- (median(x))^2
  }
  
  cat("\nn =", n, "\n")
  cat("Mean MSE   :", mean(mse_mean), "\n")
  cat("Median MSE :", mean(mse_median), "\n")
}

##c.
for (n in n_values) {
  
  mse_mean <- numeric(R)
  mse_median <- numeric(R)
  
  for (r in 1:R) {
    x <- rnorm(n)
    
    mse_mean[r] <- (mean(x))^2
    mse_median[r] <- (median(x))^2
  }
  
  cat("\nn =", n, "(Normal)\n")
  cat("Mean MSE   :", mean(mse_mean), "\n")
  cat("Median MSE :", mean(mse_median), "\n")
}

