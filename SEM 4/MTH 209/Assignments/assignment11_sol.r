MY.find_minima <- function(fun, start)
{
  n <- length(start)
  
  local.minimas <- numeric(length = n)
  fn.at.minimas <- numeric(length = n)
  
  for(i in 1:n)
  {
    fit <- optim(par = start[i], fn = fun)
    
    local.minimas[i] <- fit$par
    fn.at.minimas[i] <- fit$value
  }
  
  ind <- which.min(fn.at.minimas)
  
  return(local.minimas[ind])
}

