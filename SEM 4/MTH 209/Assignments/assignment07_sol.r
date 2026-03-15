MY.CLT_simulation <- function(alpha)
{
  reps <- 100
  n <- 50
  
  store <- numeric(length = reps)
  for(r in 1:reps)
  {
    sample <- rgamma(n, shape = alpha)
    store[r] <- sqrt(n)*(mean(sample) - alpha)
  }
  return(store)
}