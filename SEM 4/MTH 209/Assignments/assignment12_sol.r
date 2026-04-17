MY.predict_lda <- function(x, mus, pis, Sigma)
{
  K <- length(mus)
  
  delta <- length(K)
  for(k in 1:K)
  {
    delta[k] <- log(pis[k]) + t(mus[[k]]) %*% solve(Sigma) %*% x - t(mus[[k]]) %*% solve(Sigma) %*% mus[[k]]/2
  }

  label <- which.max(delta)# either 1, 2, ..., K (numeric)
  return(label)
}