##################################
# My solution for the third question
##################################
MY.predict_qda <- function(y, X, x_new)
{
  n <- length(y)
  K <- max(y)
  
  deltak <- numeric(length = K)
  # obtain mu_hat_k and Sigma_hat_k
  for(k in 1:K)
  {
    ind <- (y == k)
    yk <- y[ind]
    Xk <- X[ind, ]
    pik <- sum(ind)/n
    
    muk <- colMeans(Xk)
    Sigmak <- cov(Xk)
    
    deltak[k] <- -0.5*log(det(Sigmak)) - t(x_new - muk) %*% solve(Sigmak) %*% (x_new - muk)/2 + log(pik)
  }
  
  return(which.max(deltak))
}