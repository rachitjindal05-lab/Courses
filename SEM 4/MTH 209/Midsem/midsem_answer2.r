##################################
# My solution for the svd problem
##################################
MY.comp_alpha <- function(alpha, n.vec = c(10, 100, 500, 1000), reps)
{
  MSE.T1 <- matrix(NA, nrow=reps, ncol=length(n.vec))
  MSE.T2 <- matrix(NA, nrow=reps, ncol=length(n.vec))
  
  # Calculating MSE. Reps on the outside
  for(r in 1:reps)
  {
    for(i in 1:length(n.vec))
    {
      n <- n.vec[i]
      samp <- rgamma(n, shape=alpha, rate=1)
      T1 <- mean(samp)
      T2 <- var(samp)
      MSE.T1[r, i] <- (T1 - alpha)^2
      MSE.T2[r, i] <- (T2 - alpha)^2
    }
  }

  # Making plot
  plot(log(n.vec), colMeans(MSE.T1), type="b", xlab="log(n)", 
       ylim = range(colMeans(cbind(MSE.T1, MSE.T2))),ylab=" Estimated MSE", col="blue", pch=16)
  
  lines(log(n.vec), colMeans(MSE.T2), type="b", col="red", pch=16)
  legend("topright", legend=c("T1", "T2"), col=c("blue", "red"), pch=16, lty=1)
}
