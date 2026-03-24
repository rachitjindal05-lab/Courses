##################################
# My solution for the digits classification problem
##################################
load("digits.RData")
library(Rtsne)
library(imager)
set.seed(1)

# Do tsne on the combined train and test
X <- digits_train[,-1]
X <- rbind(X, digits_test)

cols <- rainbow(11)
labels <- c(digits_train[,1], rep(10, nrow(digits_test)))


tsne <- Rtsne(X, perplexity = 30, dims = 3, check_duplicates = FALSE)
plot(tsne$Y, col = cols[labels + 1], pch = 16, xlab = "t-SNE 1", ylab = "t-SNE 2")
legend("topright", legend = 0:10, col = cols, pch = 16)


# name must be prediction
prediction <- numeric(length = 1000)
for(i in 1:1000)
{
  # get distance of the ith observation to all the training data
  d <- apply(tsne$Y[1:2000, ], 1, function(x) sqrt( sum( (x - tsne$Y[2000 + i, ])^2 ) ) )
   
  # get the index of the closest training observation
  idx <- order(d, decreasing = FALSE)[1]
  
  labels_of_idx <- labels[idx]
  
  # assign the label of the most commonly found label
  prediction[i] <-  as.numeric(names(which.max(table(labels_of_idx))))
}

# prediction


## check prediction
# load("digits_truth.RData")
# true_labels <- digits_test_truth[,1]

# sum(prediction == true_labels)
