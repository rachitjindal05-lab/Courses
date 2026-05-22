### Solutions to Worksheet 1 ###

## Problem 3
# We can also add conditions
# for only positive integers
fact <- function(n)
{
  track <- 1
  for(i in 1:n)
  {
    track <- track * i
  }
  return(track)
}
fact(6)

## Problem 4
euler <- function(n)
{
  nlim <- (1 + 1/n)^(n)
  return(nlim)
}
euler(3)
# checking comparisons with e
euler(10000) - exp(1)

## Problem 5

# make sure seating.csv is downloaded 
# to your working directory
# use getwd() to find your working directory
seat <- read.csv("seating.csv")

# suppose my roll no. is 240823
seat[seat$Roll == 240823, ]

# Print the roll number of the person assigned C25
print(seat$Roll[seat$Seat == "C25"])

