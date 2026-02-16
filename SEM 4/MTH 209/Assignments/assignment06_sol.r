library(Rtsne)

data <- read.csv("assignment_dataset.csv")
X <- data[, 1:5]
set.seed(1)
tsne_result <- Rtsne(X, perplexity = 50, verbose = TRUE)

tsne_df <- tsne_result$Y

MY.dist1 <- norm(tsne_df[15, ] - tsne_df[20, ], "2")

sub <- tsne_df[data$type == 2, ]

# euclidean distance between point i and all points in sub
distances <- numeric(length = dim(sub)[1])
for(i in 1:dim(sub)[1])
{
	distances[i] <- norm(tsne_df[15, ] - sub[i, ], "2")
}

# smallest distance
MY.dist2 <- min(distances)

MY.answer <- c(MY.dist1, MY.dist2)