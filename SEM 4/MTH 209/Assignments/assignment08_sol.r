MY.comp_alpha <- function(alpha, n.vec = c(10, 100, 500, 1000), reps)
{
  l <- length(n.vec)
  # to store mse estimated
  mse1 <- numeric(length = l)
  mse2 <- numeric(length = l)
  # loop for each n in n.vec
  for(r in 1:reps)
  {
    for(i in 1:l)
    {
      n <- n.vec[i]
      x <- rgamma(n, alpha, 1)
      m1 <- mean(x)
      m2 <- (sum((x - m1)^2))/(n-1)
      mse1[i] <- mse1[i] + mean((m1 - alpha)^2)
      mse2[i] <- mse2[i] + mean((m2 - alpha)^2)
    }
  }
  return(cbind(mse1, mse2)/reps)
}
