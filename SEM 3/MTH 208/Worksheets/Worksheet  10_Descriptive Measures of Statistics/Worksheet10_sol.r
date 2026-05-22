####################################
## Partial Solutions for Worksheet 10
####################################

## Problem 1

# set your working directory correctly
movies <- read.csv("movie_unweighted.csv")
head(movies)  # data is called dat

## Problem 2a
hist(movies$ratings, main = "Histrogram of Ratings",
     xlab = "Ratings")

## 2b
hist(movies$ratings, main = "Histrogram of Ratings",
     xlab = "Ratings", col = "white")


## 2c
par(mfrow = c(1,2))
hist(movies$ratings, main = "Ratings",
     xlab = "Ratings", xlim = c(7.5, 10))
hist(movies$unweighted, main = "Unweighted Ratings",
     xlab = "Unweighted Ratings", xlim = c(7.5, 10))

# Both are positively skewed


## 2c (vi)
par(mfrow = c(1,2))
hist(movies$ratings, main = "Ratings",
     xlab = "Ratings", xlim = c(7.5, 10))
abline(v = mean(movies$ratings), col = "blue", lty = 2,
       lwd = 2)
abline(v = median(movies$ratings), col = "red", lty = 3,
       lwd = 2)
legend("topright", legend = c("sample mean", "sample median"),
       col = c("blue", "red"), lty = c(2,3), lwd = 2)


hist(movies$unweighted, main = "Unweighted Ratings",
     xlab = "Unweighted Ratings", xlim = c(7.5, 10))
abline(v = mean(movies$unweighted), col = "blue", lty = 2,
       lwd = 2)
abline(v = median(movies$unweighted), col = "red", lty = 3,
       lwd = 2)
legend("topright", legend = c("sample mean", "sample median"),
       col = c("blue", "red"), lty = c(2,3), lwd = 2)

## modal class for Ratings is 8-8.2
## modal class for Unweighted is also 8-8.2


# Question 2e
var(movies$ratings)
var(movies$unweighted)



par(mfrow = c(1,1))
boxplot(movies$ratings, movies$unweighted,
        names = c("Ratings", "Unweighted"))

quantile(movies$ratings, c(.25, .75))




# Question 3

# 3a
boxplot(movies$rating, main = "Boxplot of Ratings")

# 3b
boxplot(movies$rating, main = "Boxplot of Ratings", col = "pink")

# 3c
range(movies$rating)
range(movies$unweighted)

# 3d
diff(quantile(movies$rating, c(.25, .75)))
diff(quantile(movies$unweighted, c(.25, .75)))


# Question 4
boxplot(movies[, 2:3], main = "Boxplot of different ratings")

# Question 5
par(mfrow = c(1,1))
hist(movies$ratings, col = adjustcolor("blue", alpha.f = .2), 
  xlim = range(c(movies$ratings, movies$unweighted)),
  main = "Histogram of Ratings")
hist(movies$unweighted, add = TRUE,
     col =  adjustcolor("red", alpha.f = .2))
legend("topright", fill = c(adjustcolor("blue", alpha.f = .2),
  adjustcolor("red", alpha.f = .2)), legend = c("Original Rating", "Unweighted"))



# Question 6

# Right skewed

