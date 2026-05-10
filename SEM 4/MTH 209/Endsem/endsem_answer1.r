##################################
# My solution for the first question
##################################

MY.ridge <- function(y, X, lambda)
{
  p <- ncol(X)
  n <- nrow(X)
  est <- solve( t(X)%*%X + lambda * diag(p)) %*% t(X) %*%y
  
  return(est)
}

MY.lasso <- function(y, X, lambda)
{
  p <- ncol(X)
  n <- nrow(X)
  
  # Objective Function
  beta_lasso <- function(beta, lambda, y, X)
  {
    sum((y - X %*% beta)^2) + lambda * sum(abs(beta))
  }
  
  n.tries <- 10
  mle <- solve(t(X) %*% X) %*% t(X) %*% y
  ridge <- solve( t(X)%*%X + lambda * diag(p)) %*% t(X) %*%y
  
  # various starting values between ridge and mle
  # to ensure convergence happens
  fn.value <- numeric(length = n.tries)
  lasso.est <- matrix(NA, ncol = p, nrow = n.tries)
  for(k in 1:n.tries)
  {
    start <- ridge + runif(p, min = -1, max = 1) * abs(mle - ridge)
    optim <- optim(par = start, fn = beta_lasso, method = "Nelder-Mead",
                   lambda = lambda, y = y, X = X, control = list(maxit = 1e5, reltol = 1e-16))
    lasso.est[k, ] <- optim$par
    fn.value[k] <- optim$value
  }
  
  est <- lasso.est[which.min(fn.value), ]
  return(est)
}


MY.mse_regression <- function(X, beta.star, sigma2, reps, lambda)
{
  p <- ncol(X)
  n <- nrow(X)
  mse.ols <- numeric(reps)
  mse.ridge <- numeric(reps)
  mse.lasso <- numeric(reps)
  
  
  for(r in 1:reps)
  {
    y <- X %*% beta.star + rnorm(n, mean = 0, sd = sqrt(sigma2))
    
    # OLS estimator
    beta.ols <- solve(t(X) %*% X) %*% t(X) %*% y
    
    # ridge estimator
    beta.ridge <- MY.ridge(y, X, lambda)
    
    # lasso estimator
    beta.lasso <- MY.lasso(y, X, lambda)
    
    # Squared error
    mse.ols[r] <- sum((beta.ols - beta.star)^2)
    mse.ridge[r] <- sum((beta.ridge - beta.star)^2)
    mse.lasso[r] <- sum((beta.lasso - beta.star)^2)
  }
  # final MSE must be stored in a vector of length 3 called mse_beta
  mse_beta <- c(mean(mse.ols), mean(mse.ridge), mean(mse.lasso))
  names(mse_beta) <- c("OLS", "Ridge", "Lasso")
  return(mse_beta)
}
