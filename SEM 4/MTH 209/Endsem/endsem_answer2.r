##################################
# My solution for the second question
##################################
MY.sim_normal <- function(n, mu, sigma1, sigma2, rho)
{
  # covariance matrix
  foo <-  rho * sigma1 * sigma2
  Sigma <- matrix(c(sigma1^2, foo, foo, sigma2^2), nrow = 2, ncol = 2, byrow = TRUE)
  
  # Cholesky decomposition
  A <- chol(Sigma)
  
  # generate independent standard normals
  Z <- matrix(rnorm(2 * n), nrow = n, ncol = 2)
  
  # transform to correlated normals
  samples <- Z %*% A
  
  # add mean vector
  samples[, 1] <- samples[, 1] + mu[1]
  samples[, 2] <- samples[, 2] + mu[2]
  
  # returned samples must be an nx2 matrix
  # each row is a draw from normal
  return(samples)
}



# X: an nx2 dimensional matrix from Normal distribution

MY.test_stat <- function(X)
{
  # sample correlation
  rho_hat <- cor(X)[1, 2]
  
  # test statistic using plug-in variance estimate
  statistic <- sqrt(nrow(X)) * rho_hat / (1 - rho_hat^2)
  
  return(statistic) # a single number
}


MY.type1_error <- function(reps, n, mu, sigma1, sigma2, alpha)
{
  # store rejection indicators
  track <- numeric(length = reps)
  
  # critical value from standard normal
  crit <- qnorm(1 - alpha / 2)
  
  for(i in 1:reps)
  {
    # simulate under H0: rho = 0
    X <- MY.sim_normal(n = n,
                    mu = mu,
                    sigma1 = sigma1,
                    sigma2 = sigma2,
                    rho = 0)
    
    # compute test statistic
    stat <- MY.test_stat(X)
    
    # reject if statistic exceeds critical value
    track[i] <- as.numeric(abs(stat) > crit)
  }
  
  # estimated Type I error
  error <- mean(track)
  
  return(error) # one number
}



