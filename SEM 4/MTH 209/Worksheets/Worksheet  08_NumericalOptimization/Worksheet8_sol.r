#Q1: Nelder-Mead function in one dimension

nelder_mead <- function(fn, x0, alpha = 1, gamma = 2,
                        rho = 0.5, sigma = 0.5,
                        tol = 1e-8, maxit = 1000)
{
  thetas <- c(x0, x0 + 1)
  iter <- 1
  continue <- TRUE
  
  while(continue)
  {
    # Step 1: order vertices
    fvals <- sapply(thetas, fn)
    ord <- order(fvals)
    
    theta1 <- thetas[ord[1]]
    theta2 <- thetas[ord[2]]
    
    f1 <- fvals[ord[1]]
    f2 <- fvals[ord[2]]
    
    # Step 2: centroid
    c <- theta1
    
    # Step 3: reflection
    theta_r <- c + alpha*(c - theta2)
    # Step 3(b)
    
    if(fn(theta_r) < f1){
      # expansion
      theta_e <- c + gamma*(theta_r - c)
      
      if(fn(theta_e) < fn(theta_r))
        theta2 <- theta_e
      else
        theta2 <- theta_r
    }
    else{
      # contraction
      theta_c <- c + rho*(theta_r - c)
      if(fn(theta_c) < f2)
        theta2 <- theta_c
      else
        # shrink
        theta2 <- theta1 + sigma*(theta2 - theta1)
    }
    
    #Update simplex
    thetas <- c(theta1, theta2)
    
    # stopping rule
    if(abs(thetas[2] - thetas[1]) < tol) continue <- FALSE
    if(iter > maxit) continue <- FALSE
    
    iter <- iter + 1
  }
  
  return(list(theta_hat = thetas[1],
              f_min = fn(thetas[1]),
              iterations = iter))
}

#Q2: Nelder-Mead for d dimension
nelder_mead_d <- function(fn, x0, alpha = 1, gamma = 2,
                        rho = 0.5, sigma = 0.5,
                        tol = 1e-8, maxit = 1000)
{
  
  d <- length(x0)
  
  # initial simplex (d+1 vertices)
  simplex <- rbind(x0, matrix(x0, nrow=d, ncol=d, byrow=TRUE) + diag(d))
  
  iter <- 1
  continue <- TRUE
  
  while(continue){
    fn <- function(z){
      x <- z[1]
      y <- z[2]
      (1-x)^2 + (y-1)^2
    }
    
    fvals <- apply(simplex,1,fn)
    ord <- order(fvals)
    simplex <- simplex[ord, ]
    fvals <- fvals[ord]
    
    theta_best <- simplex[1, ]
    theta_worst <- simplex[d+1, ]
    
    
    # centroid of best d points
    c <- colMeans(simplex[1:d, ])
    
    # reflection
    theta_r <- c + alpha*(c - theta_worst)
    
    #3
    if(fn(theta_r) >= fvals[1] && fn(theta_r) < fvals[d]) {
      simplex[d+1, ] <- theta_r
    } else if(fn(theta_r) < fvals[1]) {
      
      theta_e <- c + gamma*(theta_r - c) #expansion
      
      if(fn(theta_e) < fn(theta_r))
        simplex[d+1, ] <- theta_e
      else
        simplex[d+1, ] <- theta_r
      
    } else {
      
      theta_c <- c + rho*(theta_r - c) #contraction
      
      if(fn(theta_c) < fvals[d+1])
        simplex[d+1, ] <- theta_c
      else
        for(i in 2:(d+1))
          simplex[i, ] <- simplex[1, ] +
            sigma*(simplex[i, ] - simplex[1, ]) #shrink
    }
    # stopping rule
    if(max(abs(simplex[1,]-simplex[d+1,])) < tol) continue <- FALSE
    if(iter > maxit) continue <- FALSE
    iter <- iter + 1
  }
  
  list(par = simplex[1, ],
       value = fn(simplex[1, ]),
       iterations = iter)
}


#Q3: Test function for (x-3)^2
 f <- function(x){
     (x - 3)^2
   }
 x0 <- -3
 nelder_mead(f,x0)
 
 #Q4: Find the MLE of Cauchy distribution
 set.seed(3)
 n <- 10
 theta <- 5
 x <- rcauchy(n = n, location = theta)
 # let's plot the likelihood function
 thetas <- seq(0, 8, length = 1e3)
 log.like <- sapply(thetas, function(t)
   sum(dcauchy(x, location = t, log = TRUE)) )
 plot(thetas, log.like, type = 'l', ylab = "Log Likelihood")
 neg_log.like <- function(t){
   -sum(dcauchy(x, location = t, log = TRUE))
 }
 
 theta_mle <- nelder_mead(neg_log.like, x0 = median(x))$theta_hat
 abline(v = theta_mle, col = "red", lwd = 2)

#Q5: OPTIM Function

optim(-2, f, method = "Nelder-Mead", control = list(maxit = 100000, reltol = 1e-36))

optim(2,neg_log.like, control = list(maxit = 100000, reltol = 1e-36))


#Q6: Rosenbrock

rosenbrock <- function(x, y){
  (1 - x)^2 + 100*(y - x^2)^2
}
x <- seq(-2, 2, length = 200)
y <- seq(-1, 3, length = 200)
# evaluate rosenbrock on these grid of values
z <- outer(x, y, rosenbrock)
# contours of the function
contour(x, y, z,
        nlevels = 40,
        xlab = "x",
        ylab = "y",
        main = "Rosenbrock Function")
## Nelder-Mead compatible function
rosenbrock <- function(z)
{
  x <- z[1]
  y <- z[2]
  (1 - x)^2 + 100*(y - x^2)^2
}

# Q7: Minimum for Rosenbrock
result <- nelder_mead_d(rosenbrock, x0 = c(-1,2))
result
