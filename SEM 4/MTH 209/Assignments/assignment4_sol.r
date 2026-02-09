MY_PC_scores <- function(covariance, x, k)
{
  pr <- eigen(covariance)$vectors
  pr_rot <- pr[,1:k]
  scores <- numeric(length = k)
  scores <- as.numeric(apply(pr_rot, 2, function(y) t(y) %*% x))
  return(scores) # output must be a vector of length k
}

