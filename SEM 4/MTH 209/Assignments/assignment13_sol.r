# x: a vector of the X observations
# y: a vector of the Y observations
# alpha: confidence level of the Confidence Interval
MY.make_CI <- function(x, y, alpha)
{
  n1 <- length(x)
  n2 <- length(y)
  s1sq <- 5/n1
  s2sq <- 10/n2
  se <- sqrt(s1sq + s2sq)
  est <- mean(y) - mean(x)
  Lower <- est - se*qnorm((1-alpha/2)) # calculate lower end of CI
  Upper <- est + se*qnorm((1-alpha/2)) # calculate upper end of CI
  return(c(Lower, Upper))
}