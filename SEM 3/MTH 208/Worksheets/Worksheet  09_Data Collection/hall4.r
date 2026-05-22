set.seed(1)  # so your answers are same as mine

## Problem 1
# a
# Population: Orders in Hall 4 canteen in 2022
# Sample: sampled ordered from Hall 4 canteen in 2022

# b
# Population: All voters in the election
# sample: interviewed voters after voting

# c
# Population: Data Scientists in India
# Sample: Data Scientists in India who have declared
#         their income on Glassdoor.com

# d
# Population: Duels designed in that particular way of
#              tossing a fair coin and having players shoot
#             one at a time with equal probability of hitting in each shoot
# Sample: Random simulations of the duel experiment

# e 
## decide as per your project


## Problem 2
# a. mu
# b. sigma^2/n

reps <- 1000
# population
N <- 100
xi <- 1:N
mu <- mean(xi)
sig2 <- var(xi)

# allocating memory
means <- numeric(length = reps)
vars <- numeric(length = reps)

n <- 10
# obtaining repeated samples
for(r in 1:reps)
{
 # one sample of size 10
 samp <- sample(xi, size = n, replace = TRUE)
 
 # sample mean and sample variance
 means[r] <- mean(samp)  # sample mean -- should be close to mu
 vars[r] <- var(samp)  # sample variance -- should be close to sigma^2
}

mean(means)  # should be close to your answer for a.
var(means)  # should be close to your answer for b.

# compare with
c(mu, sig2/n)



## Problem 3
## There was a small mistake in the sample code
## shared in the worksheet

reps <- 1000
# population
N <- 100
xi <- 1:N
mu <- mean(xi)
sig2 <- var(xi)

# allocating memory
means_wor <- numeric(length = reps)
vars_wor <- numeric(length = reps)

n <- 10
# obtaining repeated samples
for(r in 1:reps)
{
 # one sample of size 10
 samp <- sample(xi, size = n, replace = FALSE)
 
 # sample mean and sample variance
 means_wor[r] <- mean(samp)  # sample mean -- should be close to mu
 vars_wor[r] <- var(samp)  # sample variance -- should be close to sigma^2
}

mean(means_wor)  
var(means_wor)  # smaller than with replacement!

# Problem 4

hall <- read.csv("hall4.csv")

# sample size
n <- 100

# number of stratas
n.stratas <- length(unique(hall$dayOfWeek))

# create props
props <- table(hall$dayOfWeek)
print(props)

# decide the sample size for each day

# equal probability
samp.strats <- sample(1:n.stratas, size = n, prob = props, replace = TRUE)

samp <- numeric(length = n)
for(i in 1:n)
{
  # randomly choose one sample from the chosen day of week
  samp[i] <- sample( which(hall$dayOfWeek == samp.strats[i]), size = 1)
}

samp.order <- hall[samp, ]
# samp is the stratified sample

# Problem 5

# since stratified sample has sample size in proportion
# to the population, we can take simple mean
mean(samp.order$order)


