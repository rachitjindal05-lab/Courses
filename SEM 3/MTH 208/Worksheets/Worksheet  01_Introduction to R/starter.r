## Basic arithmatic
1 + 2 
3 + 5 - 2

## assigning value
a <- 4  # I prefer arrow over =
b = 5
my.number <- 8
my.vector <- 5:100

## operations with objects
a + b
my.number %% 5   #modulo
my.vector %% 5
b/a + sqrt(my.number) - sin(b) + log(a)




## Indexing 
## Logical checks
a == 10  # is a = 10?
my.number == a
my.vector == c(5:100)
my.vector == my.number

index <- which(my.vector == my.number)
my.vector[index]



# R is great for vectors
sum(my.vector)
mean(my.vector)
median(my.vector)
max(my.vector)
min(my.vector)

## Making functions
greetings <- function(name)
{
  text <- paste("Hello", name)
  return(text)
}
##greetings(Akash)
greetings("Akash")

add <- function(num1, num2 = 5)
{
  y <- num1 + num2
  return(y)
}
add(num1 = 6, num2 = 10)


# function for evaluating f(x) = 5x^2 + 3x - 3
fx <- function(x)
{
  val <- 5*x^2 + 3*x - 3
  return(val)
}
fx(4)
fx(10)
fx(4:10)
fx(a)
fx( c(4, 9, 15) )
x <- c(4, 9, 15)
fx(x)

## working directory
getwd()
list.files()
##setwd("path/to/your/folder")

# Loading a csv file (csv is much easier than xls)
seat <- read.csv("s.csv")
head(seat)  # to see the top of the data
str(seat)  # structure of the data
dim(seat)  #dimensions

seat[1,1]
seat[1, ]
seat[3:5, ]
seat[c(4, 7, 9), ]

# When in doubt
?head
?dim
?read.csv

# Loops in R
track <- 0
for(i in 1:length(my.vector))
{
  track <- track + my.vector[i]
}
track
