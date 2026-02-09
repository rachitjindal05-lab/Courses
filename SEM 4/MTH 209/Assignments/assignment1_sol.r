MY.diagTelescope <- function(A)
{
  n <- sqrt(length(A))  # or dim(A)[1]
  t <- numeric(length = n)
  alldiags <- diag(A)
  
  for(k in 1:n)
  {
    t[k] <- sum(alldiags[1:k])
  }
  return(t) 
}