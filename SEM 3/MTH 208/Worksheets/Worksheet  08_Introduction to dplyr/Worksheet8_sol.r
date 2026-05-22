library(rvest)
library(dplyr)


# Problem 1
html <- read_html("https://www.relianceiccrankings.com/ranking/womenodi/batting/")

dat <- html %>% html_table()
# this is equivalent
dat <- html_table(html)
head(dat) #my

# dat is a list of tables. There is only one table
# so we extract the first one
batting <- dat[[1]]
print(batting) #my
colnames(batting)[4] <- "Country"
colnames(batting)[1] <- "Ranking"

country <- html %>% html_elements("td img") %>% html_attr("alt")

batting$Country<- country
head(batting) #my

## problem 2
batting <- as_tibble(batting) # Convert to tibble explicitly (so it prints nicely)
print(batting) #my

## Problem 3
# go through the link on your own

## Problem 4
# Let's load the dataset
data(mtcars)
head(mtcars)  # look at the data
?mtcars  # search for information on data


## Problem 5
# making a grouped tibble by cylinder
by_cyl <- mtcars %>% group_by(cyl)

# summarizing with table of average disp and horsepower
by_cyl %>% summarise(
  avg_disp = mean(disp),
  avg_hp = mean(hp)
)


## Problem 6
# (a) ranking of all players
india <- batting %>% filter(Country == "IND")

india %>% select(Ranking, Name, Country)
india$Ranking


india %>% summarise(n = n())



# (b) number in each team
by_country <- batting %>% group_by(Country)
by_country %>% summarize(number = n())

# (c) average rank
mean_Rank <- by_country %>% summarise(average.rank = mean(Ranking))
mean_Rank %>% arrange(average.rank)

# (d) in order from lowest ranking to highest
by_country %>% 
  summarise(average.rank = mean(Ranking)) %>%
  arrange(desc(average.rank))


## Problem 7
# Making a function that allocates
# "Asia" to Asian teams
# and "Not Asia" to other teams
asia <- function(team)
{
  k <- length(team)
  cont <- numeric(length = k)
  for(i in 1:k)
  {
    if( sum(team[i]  == c("SL", "IND", "PAK", "THA", "BAN") ) > 0)
    {
      cont[i] <- "Asia"
    } else{
      cont[i] <- "Not Asia"
    }
  }
  return(cont)
}

# using mutate to add a new column
# the new column will be called "continent"
# and will be made with function asia() applied on Team
asia_batting <- batting %>% mutate(continent = asia(Country))
by_continent <- asia_batting %>% group_by(continent) 
print(by_continent, width = Inf) #my

# (a) how many Asia/Non-Asia players
by_continent %>% summarise(n = n())

# (b) Average Rating
by_continent %>% summarise(Rating = mean(Rating)) %>% arrange(desc(Rating))


