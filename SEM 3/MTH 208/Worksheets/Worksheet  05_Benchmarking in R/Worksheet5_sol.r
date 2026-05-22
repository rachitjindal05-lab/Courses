attempts <- function(age) {
  count <- 0
  remain <- age # age no. of candles remain in the beginning
  while(remain > 0) {
    count <- count + 1 # randomly choose any number between 1 and remain
    blown_out <- sample(1:remain, size = 1)
    remain <- remain - blown_out
  }
  return(count)
}

att_vec <- numeric(length = 1e3)
for(i in 1:1e3) {
  att_vec[i] <- attempts(25)
}

###########

# ---------
# Problem 1
# ---------

att_vec <- replicate(1e3, attempts(25))

###########

# ---------
# Problem 2
# ---------

library(rbenchmark)
benchmark(
  "for loop" = {
    att_vec <- numeric(length = 1e3)
    for(i in 1:1e3) {
      att_vec[i] <- attempts(25)
    }
  },
  "replicate" = replicate(1e3, attempts(25)), 
  replications = 100
)

###########

# ---------
# Problem 3
# ---------

benchmark(
  {
    att_vec <- numeric(length = 1e4)
    for(i in 1:1e4) {
      att_vec[i] <- attempts(25)
    }
  },
  replicate(1e4, attempts(25)), 
  replications = 20
)

###########

# ---------
# Problem 4
# ---------

benchmark(
  "dynamic" = {
    att_vec <- NULL
    for (i in 1:1e4) {
      att_vec<-c(att_vec,attempts(25))
    }
  },
  "preallocate" = {
    att_vec <- numeric(length = 1e4)
    for(i in 1:1e4) {
      att_vec[i] <- attempts(25)
    }
  },
  "replicate" = {
    att_vec <- numeric(length = 1e4)
    att_vec <- replicate(1e4, attempts(25))
  }, 
  replications = 100
)

###########

# ---------
# Problem 5
# ---------

library(Rcpp)
sourceCpp("attempts.cpp")

benchmark(
  "For Loop" = {
    att_vec <- numeric(length = 1e3)
    for(i in 1:1e3) {
      att_vec[i] <- attempts(25)
    }
  },
  "Replicate" = {
    att_vec <- replicate(1e3, attempts(25))
  },
  "C++ via Rcpp" = {
    att_vec <- attempts_cpp(25, 1e3)
  },
  replications = 100,
  columns = c("test", "replications", "elapsed", "relative"),
  order = "elapsed"
)

###########

# ---------
# Problem 6
# ---------

n <- 1e4
benchmark(
  {
    x_loop <- numeric(n)
    for (i in 1:n) {
      x_loop[i] <- runif(1, 0, 1)
    }
  },
  x_vec <- runif(n, 0, 1),
  replications = 100,
  columns = c("test", "replications", "elapsed", "relative")
)

###########

# ---------
# Problem 7
# ---------

set.seed(123)
n <- 5000   # number of rows
m <- 1000   # number of columns
mat <- matrix(runif(n*m, 0, 1), nrow = n, ncol = m)

benchmark(
  means_colMeans <- colMeans(mat),
  means_apply <- apply(mat, 2, mean),
  replications = 10
)
# Check if results are identical
all.equal(means_colMeans, means_apply)

# ---------
# Problem 8
# ---------

num1 <- numeric(length = 1e3) # 8000
object.size(num1)
num2 <- numeric(length = 1e6) # 8000000
object.size(num2)

mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000) #8x10^5
object.size(mat1)
mat2 <- matrix(0, nrow = 100, ncol = 1000) #8x10^5
object.size(mat2)

arr <- array(0, dim = c(100,100,100)) #8x10^6
object.size(arr)
