score <- function(mu, x) {
  sum(-2/(x - mu) + 6*(x - mu)^2)
}

score2 <- function(mu, x) {
  sum(-2/(x - mu)^2 - 12*(x - mu))
}


# QUESTION 1: NEWTON-RAPHSON

newton_method <- function(x, mu0 = 1.1, tol = 0.001) {
  
  mu <- mu0
  
  # First 
  mu1 <- mu - score(mu, x)/score2(mu, x)
  
  # Continue
  repeat {
    mu_new <- mu - score(mu, x)/score2(mu, x)
    
    if (abs(mu - mu_new) < tol) break
    mu <- mu_new
  }
  
  return(list(A = mu1, B = mu_new))
}

# Quiz 2A
x1 <- c(1.25, 2.25, 3.25)
res_2A_NR <- newton_method(x1)

# Quiz 2B
x2 <- c(1.20, 2.20, 3.20)
res_2B_NR <- newton_method(x2)

cat("Newton Method Results:\n")
cat("Quiz 2A -> A =", res_2A_NR$A, " B =", res_2A_NR$B, "\n")
cat("Quiz 2B -> A =", res_2B_NR$A, " B =", res_2B_NR$B, "\n")


# QUESTION 2: BISECTION METHOD


bisection_method <- function(x, a0, b0, tol = 0.001) {
  
  a <- a0
  b <- b0
  
  a_vals <- c()
  b_vals <- c()
  
  i <- 0
  
  repeat {
    c_mid <- (a + b)/2
    
    if (score(a, x) * score(c_mid, x) < 0) {
      b <- c_mid
    } else {
      a <- c_mid
    }
    
    i <- i + 1
    a_vals[i] <- a
    b_vals[i] <- b
    
    if (abs(b - a) < tol) break
  }
  
  A <- sqrt(a_vals[1] * b_vals[1])
  B <- (a_vals[3] + b_vals[3]) / 2
  
  return(list(A = A, B = B))
}

# Quiz 2A
res_2A_BIS <- bisection_method(x1, 1.00, 1.2499)

# Quiz 2B
res_2B_BIS <- bisection_method(x2, 1.00, 1.199)

cat("\nBisection Method Results:\n")
cat("Quiz 2A -> A =", res_2A_BIS$A, " B =", res_2A_BIS$B, "\n")
cat("Quiz 2B -> A =", res_2B_BIS$A, " B =", res_2B_BIS$B, "\n")

