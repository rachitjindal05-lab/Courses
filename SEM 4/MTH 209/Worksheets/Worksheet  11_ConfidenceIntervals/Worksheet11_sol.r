
#               Worksheet11 Solution
#               By Saurabh Yadav (TA)

#===============================================================================

# Question no.1 {Confidence interval for mu from N(mu, 1)}

mu <- 0
sigma <- 1
alpha <- 0.05
reps <- 1000
n <- 100

# Function for create confidence interval

confid_int <- function(n, mu=0, alpha=0.05) {
  
z <- qnorm(1 - alpha/2)       # Critical value
  
x <- rnorm(n, mean=mu, sd=1)  # Generate sample
  
mu_hat <- mean(x)             # Sample mean
  
# Confidence interval
  
lower <- mu_hat - z / sqrt(n)
upper <- mu_hat + z / sqrt(n)
  
return(c(lower = lower, upper = upper))
}

# Replications  

store_value <- matrix(0, nrow = reps, ncol = 2)

for (r in 1:reps) {
  store_value[r, ] <- confid_int(n, mu, alpha)
}

# Compute Coverage Probability

coverage <- mean(store_value[,1] <= mu & store_value[,2] >= mu)

# Print the Coverage Probability and Confidence Interval

cat("Coverage Probability:", coverage, "\n")
cat("Confidence Interval:", mean(store_value[,1]), mean(store_value[,2]),"\n")

#===============================================================================

# Question no. 2

#-------------------------------------------------------------------------------
# Part(i) Unknown variance(Exact t CI)
#-------------------------------------------------------------------------------

n <- 100
confid_int <- function(n, mu=0, sigma=1, alpha=0.05) {
  
tcrit_value <- qt(1 - alpha/2, df=n-1) # Critical value

x <- rnorm(n, mean=0, sd=1)             #sample

mu_hat <- mean(x)      # sample mean

s <- sd(x)

lower <- mu_hat - tcrit_value * s / sqrt(n)
upper <- mu_hat + tcrit_value * s / sqrt(n)
    
return(c(lower = lower, upper = upper))
}

# Replications  

store_value <- matrix(0, nrow = reps, ncol = 2)

for (r in 1:reps) {
  store_value[r, ] <- confid_int(n)
}

# Compute Coverage Probability

coverage <- mean(store_value[,1] <= 0 & store_value[,2] >= 0) # mean of TRUE or FALSE

# Print the Results

cat("Coverage Probability:", coverage, "\n")
cat("Confidence Interval:", mean(store_value[,1]), mean(store_value[,2]), "\n")

#-------------------------------------------------------------------------------
# Part(ii) Asymptotic normal approximation
#-------------------------------------------------------------------------------

n <- 1000
Appro_confid_int <- function(n, mu=0, sigma=1, alpha=0.05) {
  
z <- qnorm(1 - alpha/2)
  
x <- rnorm(n, mean=mu, sd=sigma)
mu_hat <- mean(x)

s <- sd(x)
    
lower <- mu_hat - z * s / sqrt(n)

upper <- mu_hat + z * s / sqrt(n)
    
return(c(lower = lower , upper = upper))
}

# Replications  

store_value <- matrix(0, nrow = reps, ncol = 2)

for (r in 1:reps) {
  store_value[r, ] <- confid_int(n)
}

# Compute Coverage Probability

coverage <- mean(store_value[,1] <= 0 & store_value[,2] >= 0) # mean of TRUE or FALSE

# Print the Results

cat("Coverage Probability:", coverage, "\n")
cat("Average Lower Bound:", mean(store_value[,1]), mean(store_value[,2]),"\n")


#-------------------------------------------------------------------------------
# Part(iii) Repeat part(ii) for n=10 and n=1000
#-------------------------------------------------------------------------------


#===============================================================================

# Question no. 3  (F = Bern(p))

# Part(i)

p <- 1/2
alpha <- 0.05
n <- 100
reps <- 1000

Confid_int <- function(n, p = 1/2, alpha = 0.05){
  x <- rbinom(n,1, 1/2)
  p_hat  <- mean(x)
  
  sd <- sqrt(p*(1-p))
  z <- qnorm(1-alpha/2)
  
  
  lower <- p_hat - z * sd / sqrt(n)
  upper <- p_hat + z * sd / sqrt(n)
  return(c(lower = lower , upper = upper))
}

# Replications  

store_value <- matrix(0, nrow = reps, ncol = 2)

for (r in 1:reps) {
  store_value[r, ] <- Confid_int(n)
}

# Compute Coverage Probability

coverage <- mean(store_value[,1] <= p & store_value[,2] >= p) # mean of TRUE or FALSE

# Print the Results

cat("Coverage Probability:", coverage, "\n")
cat("Confidence Interval:", mean(store_value[,1]), mean(store_value[,2]),"\n")


#Part(ii)


p <- 1/2
alpha <- 0.05
n <- 100
reps <- 1000

Confid_int <- function(n, p = 1/2, alpha = 0.05){
  x <- rbinom(n,1, 1/2)
  p_hat  <- mean(x)
  
  sd <- sqrt(p_hat*(1-p_hat))
  z <- qnorm(1-alpha/2)
  
  
  lower <- p_hat - z * sd / sqrt(n)
  upper <- p_hat + z * sd / sqrt(n)
  return(c(lower = lower , upper = upper))
}

# Replications  

store_value <- matrix(0, nrow = reps, ncol = 2)

for (r in 1:reps) {
  store_value[r, ] <- Confid_int(n)
}

# Compute Coverage Probability

coverage <- mean(store_value[,1] <= p & store_value[,2] >= p) 

# Results

cat("Coverage Probability:", coverage, "\n")
cat("Confidence Interval:", mean(store_value[,1]), mean(store_value[,2]),"\n")



#===============================================================================

#Questio no.4 (F = gamma(alpha, 1)) where alpha= 0.01

n <- 50
sig_alpha <- 0.05
alpha <- 0.01

Confid_int <- function(n, alpha= 0.01, sig_alpha = 0.05){
  
  z <- qnorm(1- sig_alpha/2)
x <- rgamma(n, shape = alpha, rate = 1)
alpha_hat <- mean(x)

sd <- sqrt(alpha/1)

lower <- alpha_hat - z * sd / sqrt(n)
upper <- alpha_hat + z * sd / sqrt(n)
return(c(lower = lower , upper = upper))

}
# Replications  

store_value <- matrix(0, nrow = reps, ncol = 2)

for (r in 1:reps) {
  store_value[r, ] <- Confid_int(n)
}

# Compute Coverage Probability

coverage <- mean(store_value[,1] <= alpha & store_value[,2] >= alpha) 

# Print the Results

cat("Coverage Probability:", coverage, "\n")
cat("Confidence Interval:", mean(store_value[,1]), mean(store_value[,2]),"\n")

#===============================================================================

# Question no.5


# Parameters

n <- 100
mu <- c(0, 0)
sig_alpha <- 0.05
reps <- 10000

# function for calculating confidence interval

Confid_int <- function(n, mu = c(0,0), sig_alpha = 0.05){
  
  z <- qnorm(1 - sig_alpha/2)
  
  # Generate data
  x1 <- rnorm(n, mean = mu[1], sd = 1)
  x2 <- rnorm(n, mean = mu[2], sd = 1)
  
  # Sample means
  
  mu1_hat <- mean(x1)
  mu2_hat <- mean(x2)
  
  # Known sd = 1
  sd <- 1
  
  # Confidence intervals
  lower1 <- mu1_hat - z * sd / sqrt(n)
  upper1 <- mu1_hat + z * sd / sqrt(n)
  
  lower2 <- mu2_hat - z * sd / sqrt(n)
  upper2 <- mu2_hat + z * sd / sqrt(n)
  
  return(c(lower1, upper1, lower2, upper2))
}


# Replications

store_value <- matrix(0, nrow = reps, ncol = 4)

for (r in 1:reps) {
  store_value[r, ] <- Confid_int(n)
}

# Coverage probability

# mu1 coverage
coverage_mu1 <- mean(store_value[,1] <= mu[1] & store_value[,2] >= mu[1])

# mu2 coverage
coverage_mu2 <- mean(store_value[,3] <= mu[2] & store_value[,4] >= mu[2])

# joint coverage probability 

coverage_joint <- mean(
  (store_value[,1] <= mu[1] & store_value[,2] >= mu[1]) &
    (store_value[,3] <= mu[2] & store_value[,4] >= mu[2])
)

# Print the  Results

cat("Coverage for mu1:", coverage_mu1, "\n")
cat("Coverage for mu2:", coverage_mu2, "\n")
cat("Joint Coverage:", coverage_joint, "\n")

cat("Confidnce interval for mu1:", mean(store_value[,1]),
    mean(store_value[,2]), "\n")
cat("Confidence interval for mu2:", mean(store_value[,3]), 
    mean(store_value[,4]), "\n")


# One can see the coverage probability for mu is less than mu1 and mu2.

#===============================================================================










