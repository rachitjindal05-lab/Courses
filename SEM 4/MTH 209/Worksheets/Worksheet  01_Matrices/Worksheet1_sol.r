         
##       Solution of Worksheet 1      ##
##       by Saurabh Yadav (TA)        ##

#===========================================================
#Question no.1

A <- matrix(1:9, ncol = 3, nrow = 3)
B <- matrix(11:19, ncol=3, nrow=3)

# Product of A and B using standard matrix multiplication.

Truth <- A%*%B

#Product of A and B using summation A[,i]B[i,].

sum <- 0
for(i in 1:3)
  {
sum <- sum + as.matrix(A[,i])%*%B[i,]
  }
sum

# verification
Truth == sum


#============================================================
#Question no.2

n <- 100
A <- matrix(runif(n^2), ncol=n, nrow=n)
time <- system.time(A%*%A)[3]
time

#The above code gives the running time to calculate square of
# a matrix A which is 100*100.


#===========================================================
#Question no.3

n_vector <- c(1e2, 5e2, 1e3, 2e3, 3e3, 4e3, 5e3)
n <- length(n_vector)
time <- rep(0,length(n_vector))
for(i in 1:n){
     A <- matrix(runif((n_vector[i])^2),ncol=n_vector[i],
               nrow=n_vector[i])
     time[i] <- system.time(A%*%A)[3]
             }
plot(n_vector,time,"b",main="Time vs Size",
     xlab="size of matrices",ylab="Time")

# As the size of the matrix increases, the time required to compute
# the square of the matrix also increases.


#===========================================================
#Question no.4

n <- 500
B <- matrix(runif(n^2),ncol=n,nrow=n)
A <- t(B)%*%B


#we have a matrix, we will calculate its inverse using different techniques.

#Run time for inverse of A using standard solve function.
time.A.inv <- system.time
({
  A.inv <- solve(A)
})[3]
time.A.inv


#Run time for inverse of A using qr.solve function.
time.A.qr.inv <- system.time
({
  A.qr.inv <- qr.solve(A)
})[3]
time.A.qr.inv


# Run time for inverse of A  using cholesky decomposition.
time.A.chol.inv <- system.time
({
  R <- chol(A)            # A = R^T R
  A.chol.inv <- chol2inv(R)
})[3]
time.A.chol.inv


#Run time for inverse of A using singular value decomposition.
time.A.svd.inv <- system.time
({
  svd_A <- svd(A)
  U <- svd_A$u
  D <- svd_A$d
  V <- svd_A$v
  A.svd.inv <- V %*% diag(1 / D) %*% t(U)
})[3]
time.A.svd.inv


# Compare run times
time <- c(
        solve = time.A.inv,
       qrsolve = time.A.qr.inv,
       cholesky = time.A.chol.inv,
       svd = time.A.svd.inv
       )
time

# Cholesky method is fastest one.


#==========================================================
#Question no.5

n <- 8
A <- matrix(0, n, n)
for(i in 1:n){
    for(j in 1:n){
 A[i,j] <- 1/(i+j-1)
                 }
}
t(A) == A   # To check A is symmetric matrix

# Inverse using standard solve function
A.inv <- solve(A)
A.inv


# Inverse using qr solve function
#A.qr.inv<-qr.solve(A)
#A.qr.inv


# Inverse using cholesky decomposition.
R <- chol(A)
A.chol.inv <- chol2inv(R)
A.chol.inv


# Inverse using singular value decomposition.

svd_A <- svd(A)
U <- svd_A$u
V <- svd_A$v
D <- svd_A$d
A.svd.inv <- V%*% diag( 1 / D)%*% t(U)

#Error comparision
I <- diag(n)
err.solve <- norm(A %*% A.inv - I, type = "F")
#err.qr    <- norm(A %*% A.qr.inv - I, type = "F")
err.chol  <- norm(A %*% A.chol.inv - I, type = "F")
err.svd   <- norm(A %*% A.svd.inv - I, type = "F")

c(solve = err.solve,
  chol  = err.chol,
  svd   = err.svd)
min <- min(err.solve, err.chol, err.svd)

#Since inverse is not existing for qr.solve function, here we 
#find inverse using solve function and cholesky decomposition 
#and svd, comparing error. one can say inverse of A is accurate
#if we are using solve function.
#==========================================================



