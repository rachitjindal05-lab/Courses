#### quiz1A #########
#######q1############
x11 <- numeric(length = 1000)
for(i in 1:1000){
x <- rexp(21, 1)
xord <- sort(x)
x11[i] <- xord[11]
}
x11
mean(x11)
var(x11)

####quiz1A
##########q2##########
## analytic bounds
a <- 0.5 * exp(-0.5)
b <- 0.5 * exp(-1)

## h(x)
h <- function(x) {
  x^2 * exp(-4*x^2)
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
