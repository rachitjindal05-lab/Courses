         
##       Solution of Worksheet 2      ##
##       by Habiba Khatun (TA)        ##

#===========================================================

#Q1.  Consider the function f(x) =  exp(-2*x^2) defined on all ℝ. Consider the Taylor series expansion of f(x)
#around x0 = 1, 2, 3. For each x0 draw the approximation f_K(x) of f for K = 1, 2, 3, 4 in the same plot.
f <- function(x) exp(-2*x^2)

# derivatives at a point x0 (up to 4th)
d0 <- function(x0)  exp(-2*x0^2)
d1 <- function(x0)  (-4*x0) * exp(-2*x0^2)
d2 <- function(x0)  (-4 + 16*x0^2) * exp(-2*x0^2)
d3 <- function(x0)  (48*x0 - 64*x0^3) * exp(-2*x0^2)
d4 <- function(x0)  (48 - 384*x0^2 + 256*x0^4) * exp(-2*x0^2)

# Taylor polynomial of degree K around x0
taylorK <- function(x, x0, K){
  a0 <- d0(x0)
  a1 <- d1(x0)
  a2 <- d2(x0)
  a3 <- d3(x0)
  a4 <- d4(x0)
  
  if(K==1) return(a0 + a1*(x-x0))
  if(K==2) return(a0 + a1*(x-x0) + a2*(x-x0)^2/2)
  if(K==3) return(a0 + a1*(x-x0) + a2*(x-x0)^2/2 + a3*(x-x0)^3/6)
  if(K==4) return(a0 + a1*(x-x0) + a2*(x-x0)^2/2 + a3*(x-x0)^3/6 + a4*(x-x0)^4/24)
}

par(mfrow=c(1,1))

for(x0 in c(1,2,3)){
  x <- seq(x0-5, x0+5, length=1000)
  plot(x, f(x), type="l", lwd=1,
       main=paste("Taylor at x0 =", x0),ylim = c(-.05,2),
       ylab="y", xlab="x")
  #abline(v=x0, lty=3)
  
  lines(x, taylorK(x,x0,1), lty=2)
  lines(x, taylorK(x,x0,2), lty=3)
  lines(x, taylorK(x,x0,3), lty=4)
  lines(x, taylorK(x,x0,4), lty=5)
  
  legend("topright",
         legend=c("f(x)", "K=1", "K=2", "K=3", "K=4"),
         lty=c(1,2,3,4,5), lwd=c(1,1,1,1,1), bty="o", cex=0.5)
}

###Q2.


A <-  matrix(1:12, nrow = 4, ncol = 3)
A
# SVD
s <- svd(A)
U <- s$u
D <- diag(s$d)    
V <- s$v

A_hat <- U %*% D %*% t(V)
A - A_hat

#Verify: AA^TU = U d^2

AAt <- A %*% t(A)
eig_left_vals <-  AAt %*% U   
rhs_d2U <- U %*% diag(s$d^2)
eig_left_vals-rhs_d2U

# Verify: A^TAV= d^2 V
AtA <- t(A) %*% A
eig_right_vals <-  AtA %*% V
rhs_d2V <-  diag(s$d^2) %*% V
eig_right_vals - rhs_d2V


###Q3.

A <- matrix(runif(500*100), 500, 100)

s <- svd(A)
U <- s$u
V <- s$v
d <- s$d

r <- length(d)   

err <- numeric(r)
for(k in 1:r){
  sk <- svd(A, nu=k, nv=k)
  Uk <- sk$u #as.matrix(U[, 1:k])
  Vk <- sk$v #as.matrix(V[, 1:k])
  dk <- diag(sk$d, k,k)
  Ak <-  Uk %*% dk %*% t(Vk) #Uk %*% diag(d[1:k], k, k) %*% t(Vk)
  err[k] <- norm(A - Ak, type = "F")
}

plot(1:r, err, type="l", xlab="k", ylab="||A - A_k||_F")

###Q4
#load the package
library(imager)
# # convert image to grayscale
graydog <- load.image("graydog.jpg")
plot(graydog)
dim(graydog)

# store only the matrix of numbers
mat <- graydog[,,1,1]

## SVD and recombination
decomp <- svd(mat)
recomb <- decomp$u %*%diag(decomp$d) %*% t(decomp$v)
plot(as.cimg(recomb))

# relative sum of singular values
ratio <- cumsum(decomp$d)/sum(decomp$d)
k <- which(ratio >= 0.95)[1]
k
ratio[233]



####Q5
## Choosing only 2 top eigenvalues
ind <- 2
#computing matrix with just 2 dimensions.
lowermat <- decomp$u[,1:ind] %*%diag(decomp$d[1:ind]) %*% t(decomp$v[,1:ind])
#creating image with this lower dimensional matrix.
lower <- as.cimg(lowermat)
#plot image
plot(lower)
#save image
save.image(as.cimg(lower), file = "lower_dog.jpg")

####Q6
library(imager)
dog <- load.image("dog.jpeg")
plot(dog)
dim(dog)

# store each color as matrices
R <- dog[,,1,1]
G <- dog[,,1,2]
B <- dog[,,1,3]

## SVD and recombination
decomp_R <- svd(R)
decomp_G <- svd(G)
decomp_B <- svd(B)

ind <- 200
recomb_R <- decomp_R$u[,1:ind] %*%diag(decomp_R$d[1:ind]) %*% t(decomp_R$v[,1:ind])
recomb_G <- decomp_G$u[,1:ind] %*%diag(decomp_G$d[1:ind]) %*% t(decomp_G$v[,1:ind])
recomb_B <- decomp_B$u[,1:ind] %*%diag(decomp_G$d[1:ind]) %*% t(decomp_G$v[,1:ind])


dog_lower <- dog
dog_lower[,,1,1] <- recomb_R
dog_lower[,,1,2] <- recomb_G
dog_lower[,,1,3] <- recomb_B
plot(as.cimg(dog_lower))





