num1 <- numeric(length = 1e3)
num2 <- numeric(length = 1e6)
mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000)
mat2 <- matrix(0, nrow = 100, ncol = 1000)
arr <- array(0, dim = c(100,100,100))
object.size(num1)/1e3
object.size(num2)/1e6
object.size(mat1)/(100*1000)
object.size(mat2)/(100*1000)
object.size(arr)/(1000000)
#Problem 2
library(rbenchmark)
benchmark({
  relLoop <- numeric(length = 1e4)
  for (i in 1:1e4)
  {
    relLoop[i] <- rnorm(n = 1, mean = 0, sd = 1)
  }
  },
  relOnce <- rnorm(n = 1e4,mean = 0, sd = 1),
  replications = 50
)
# at once method is faster

#Problem 3
benchmark(rnorm(n = 1e4),runif(n = 1e4),replications = 50)
#runif is faster

#Problem 4
rho_mat_1 <- function(n,rho)
{
  mat <- matrix(rho,nrow = n, ncol = n)
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      mat[i,j]=rho^abs(i-j)
    }
  }
  return(mat)
}
rho_mat_2 <- function(n, rho)
{
  mat <- matrix(rho, nrow = n, ncol = n)
  mat <- mat^abs(col(mat) - row(mat))
  return(mat)
}

benchmark(rho_mat_1(10,2),rho_mat_2(10,2),replications = 50)
benchmark(rho_mat_1(100,2),rho_mat_2(100,2),replications = 50)
benchmark(rho_mat_1(1000,2),rho_mat_2(1000,2),replications = 50)

#without loops faster in all three cases

#Problem 5
x <- c(10,1e2,1e3,1e3,1e4,1e5,1e6)
for (a in x)
{
  print(stirling(a))
}
stirling <- function(n)
{
  val <- 1
  for(i in 1:n)
  {
    val <- val * i * exp(1)/n
  }
  val/sqrt(2*pi*n))}
y <- stirling(x)
