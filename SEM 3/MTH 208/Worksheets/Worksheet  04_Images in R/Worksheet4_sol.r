##### Worksheet 4 solutions #####

library(imager)

##############################
# Problem 1
dog <- load.image("dog.jpeg")
col.mat<-as.array(dog[, ,1, ])

dims <- dim(col.mat)

# Calculate distance to purest color
# for each pixel
dist <- matrix(0, nrow = dims[1], ncol = dims[2])
for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # replace c(0,1,0) with c(1,0,0) or c(0,0,1) for other problems
    dist[i,j] <- norm(col.mat[i,j, ] - c(0,1,0), "2")
  }
}

# find pixels with smallest distance
ind1 <- which(dist == min(dist), arr.ind = TRUE)

# another way
# read ?apply help page to understand the command below
dist <- apply(col.mat, c(1,2), function(s) norm(s - c(0,1,0), "2"))
ind2 <- which(dist == min(dist), arr.ind = TRUE)

# checking to see that ind1 = ind2. Hence both methods are same
ind1 == ind2

plot(dog)
points(ind1, col = "red")




###################################
# Problem 3

col1 <- load.image("col1.png")
# col1 has 4 color channels for some reason. 
# Removing the last one.
col1 <- as.cimg(col1[ , , , 1:3])

col2 <- load.image("col2.png")
col3 <- load.image("col3.png")


# I will make a function that calculates the
# difference matrix from a color for each image

diff.col <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  # return the distance matrix of each pixel
  return(dist)
}

## Making another function for each color 
## for a given image. Then for whichever primary
## color the mean of the distance matrix is smallest
## That will be the guess. 
which.color <- function(img)
{
  dist.cols <- numeric(length = 3)
  
  # make a matrix of ones.
  # this is a shortcut to define all three
  # primary colors in one go.
  colors.matrix <- diag(3)  
  for(k in 1:3)
  {
    # picking kth column of colors.matrix as we are picking
    # the kth primary color to compare to
    dist.cols[k] <- mean(diff.col(img, col = colors.matrix[ ,k]))
  }
  
  # which color has the smallest distance
  ind <- which.min(dist.cols)
  guess <- c("red", "green", "blue")[ind]
  
  return(guess)
}

which.color(col1)  # guessing for col1
which.color(col2)  # guessing for col2
which.color(col3)  # guessing for col3


###################################
# Problem 4

pic1 <- load.image("land1.jpeg")
pic2 <- load.image("land2.jpeg")

# We can reuse function diff.col!
# measuring average distance to c(1,1,1) for both pics
distance.to.white1 <- mean(diff.col(pic1, c(1,1,1)))
distance.to.white2 <- mean(diff.col(pic2, c(1,1,1)))

ifelse(distance.to.white1 < distance.to.white2, "Land1", "Land2")




##############################
# Problem 5
# 180 deg means that n x m remains n x m
# and directions are flipped
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = dims)

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    rot[i, j, ] <- col.mat[dims[1] - i + 1, dims[2] - j + 1, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))



##############################
# Problem 6
# 90 deg means that n x m becomes m x n
# and then appropriate rotation
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = c(dims[2], dims[1], dims[3]))

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # i component becomes j
    rot[j, i, ] <- col.mat[i, dims[2] - j + 1, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




##############################
# Problem 7
# 90 deg means that n x m becomes m x n
# and then appropriate rotation
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])

dims <- dim(col.mat)
rot <- array(0, dim = c(dims[2], dims[1], dims[3]))

for(i in 1:dims[1])
{
  for(j in 1:dims[2])
  {
    # i component becomes j
    rot[j, i, ] <- col.mat[dims[1] - i + 1, j, ]
  }
}

# Let's plot size by side
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(rot))




##############################
# Problem 8

# cropping image first
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])
col.mat <- col.mat[1:600, 1:600, ]

reduce <- array(0, dim = c(300, 300, 3))
red.dims <- dim(reduce)

# I will take an average of the 2 by 2 pixels
# and then assign the average value to the reduce
averaging <- rep(0,3)
for(i in 1:red.dims[1])
{
  for(j in 1:red.dims[2])
  {
    ind1 <- (2*(i-1) + 1): (2*i)
    ind2 <- (2*(j-1) + 1): (2*j)
    
    # taking the average rbg in the 2 by 2 area
    averaging[1] <- mean(col.mat[ind1, ind2, 1])
    averaging[2] <- mean(col.mat[ind1, ind2, 2])
    averaging[3] <- mean(col.mat[ind1, ind2, 3])
    
    reduce[i, j, ] <- averaging
  }
}
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(reduce))
save.image(as.cimg(reduce), file = "dog_300.jpeg")


##############################
# Problem 9

# cropping image first
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[, ,1, ])
col.mat <- col.mat[1:600, 1:600, ]

reduce <- array(0, dim = c(60, 60, 3))
red.dims <- dim(reduce)

# I will take an average of the 10 by 10 pixels
# and then assign the average value to the reduce
averaging <- rep(0,3)
for(i in 1:red.dims[1])
{
  for(j in 1:red.dims[2])
  {
    ind1 <- (10*(i-1) + 1): (10*i)
    ind2 <- (10*(j-1) + 1): (10*j)
    
    # taking the average rbg in the 10 by 10 area
    averaging[1] <- mean(col.mat[ind1, ind2, 1])
    averaging[2] <- mean(col.mat[ind1, ind2, 2])
    averaging[3] <- mean(col.mat[ind1, ind2, 3])
    
    reduce[i, j, ] <- averaging
  }
}
par(mfrow = c(1,2))
plot(dog)
plot(as.cimg(reduce))
save.image(as.cimg(reduce), file = "dog_60.jpeg")
