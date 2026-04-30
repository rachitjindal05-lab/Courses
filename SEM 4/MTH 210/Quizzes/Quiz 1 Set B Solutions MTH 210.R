#### quiz1B #########
#######q1############
x16 <- numeric(length = 1000)
for(i in 1:1000){
  x <- rexp(31, 1)
  xord <- sort(x)
  x16[i] <- xord[16]
}
x16
mean(x16)
var(x16)

####quiz1B
##########q2##########
## analytic bounds
a <- (1/ sqrt(6))*exp(-0.5)
b <- (1/ 3)*exp(-1)

## h(x)
h <- function(x) {
  x^2 * exp(-6*x^2)
}

n <- 1000
X <- numeric(n)

i <- 0
while(i < n) {
  
  u <- runif(1, 0, a)
  v <- runif(1, 0, b)
  
  x <- v/u
  
  if(u <= sqrt(h(x))) {
    i <- i + 1
    X[i] <- x
  }
}


## Order statistics
X_sorted <- sort(X)

A <- X_sorted[900]
A
B <- X_sorted[950]
B
