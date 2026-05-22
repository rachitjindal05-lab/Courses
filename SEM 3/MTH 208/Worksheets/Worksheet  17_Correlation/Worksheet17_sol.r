#Problem 1
dat <- read.csv('auto-mpg.csv')
cor(dat$mpg,dat$acceleration)

#me >> moderately positively correlated
#
#Problem 2
plot(dat$acceleration,dat$mpg)
abline(lm(dat$mpg ~ dat$acceleration))
# >> if the car has more acceleration then it will consume more gallons per mile but their are many other factors that effect this dependence
#
#Problem 3
#a
library(ggplot2)
g <- ggplot(dat,aes(acceleration,mpg))
g <- g + geom_point(aes(color =factor(cylinders)))
g <- g + stat_smooth(mapping = aes(color =factor(cylinders)),method = "lm",formula = y ~ x, geom = "smooth")
g
#b
#acceleration doesn't really effect mpg as changing the number of cyllinder drastically cahnges the correlation
#Problem 4 
#winning more rallies >> winner but actually good player? federrer 28 times he did not had maximum number of win rallies but still he won 24 times
#Problem 5
dat2 <- iris
g <- ggplot(dat2,aes(Sepal.Length,Sepal.Width))
g <- g + geom_point(aes(color =factor(Species)))
g <- g + stat_smooth(mapping = aes(color =factor(Species)),method = "lm",formula = y ~ x, geom = "smooth")
g
