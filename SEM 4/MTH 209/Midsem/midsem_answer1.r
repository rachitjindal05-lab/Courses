##################################
# My solution for the svd problem
##################################

MY_svd_compress <- function(img, p)
{
  # extract the image removing time dimension
  img_array <- img[,,1,]
  n <- dim(img_array)[1]
  m <- dim(img_array)[2]
  # stacking the image
  stacked <- rbind(img_array[,,1],
                   img_array[,,2],
                   img_array[,,3])
  # do svd, determine k and compress
  
  
  # foo is the stacked but compressed image
  foo <- svd(stacked)
  
  # find k
  k <- which(cumsum(foo$d)/sum(foo$d) >= p)[1]
  
  # extract first k components
  foo$u <- as.matrix(foo$u[, 1:k], ncol = k, nrow = 3*n)
  foo$d <- foo$d[1:k]
  foo$v <- as.matrix(foo$v[, 1:k], ncol = k, nrow = m)
  
  # combine
  foo <- foo$u %*% diag(foo$d, k) %*% t(foo$v)
  
  # unstack the compressed image
  compressed_image <- array(NA, dim = c(n, m, 3))
  compressed_image[,,1] <- foo[1:n, ]
  compressed_image[,,2] <- foo[(n+1):(2*n), ]
  compressed_image[,,3] <- foo[(2*n+1):(3*n), ]
  # this return object should be an array of size n x m x 3
  return(compressed_image)
}

# library(imager)
# img <- load.image("boat.jpeg")
# compressed <- MY_svd_compress(img, 0.1)
# plot(as.cimg(compressed))
