####################################
## Solutions for Worksheet 11
####################################
library(ggplot2)
library(tibble)

load("IMDB_movies.Rdata")

ggplot(dat, aes(x = rating)) +
  geom_histogram()

# also run this
ggplot(dat, aes(x = rating)) +
  geom_boxplot()

ggplot(dat, aes(x = rating)) +
  geom_bar()

ggplot(dat, aes(x = year, y = over.votes)) +
  geom_point()

# zooming in to some part
ggplot(dat, aes(x = year, y = over.votes)) +
  geom_point() +
  coord_cartesian(xlim = c(1996, 2025))

Year <- dat$year < 2000
Year <- as.factor(Year)
levels(Year) <- c("After 2000", "Before 2000")
#levels(Year) <- c("Before 2000", "After 2000")

ggplot(dat, aes(x = over.votes, y = rating)) +
  geom_point(aes(shape = Year, col = Year)) +
  labs(title = "Votes vs Rating", y = "Rating", x = "Number of Votes")

#####################################

## Load the dataset
load("covid.Rdata")

ggplot(dat, aes(x = rating))

names(india_covid) <- c("state", "confirmed", "active", "cured", "death")
covid <- as_tibble(india_covid)


## Create ordered barplot where the color is 
## the rate of death in the state

covid <- covid[order(covid$confirmed), ]
covid$state <- factor(covid$state, levels = covid$state)
covid$rate <- round(covid$death/covid$confirmed, 3)

g <- ggplot(covid, aes(x = state, y = log10(confirmed)))
g + geom_bar(stat = 'identity', aes(fill = rate)) + 
labs(y = "Log of Confirmed Cases", x = "State/UT", 
     title = "Covid India Data", subtitle = "Confirmed Cases shaded by rate of death") + 
coord_flip()

# Ideally, we would think that as confirmed cases increase, it affects
# the rate of death will also increase, as the hospitals become more burdened.
# However, Nagaland and Punjab have high rates of death, but reltively low
# confirmed cases. We may hypothesise that treatment may have not been done 
# sufficiently in these states


# Same plot as above, except with text as the rate
covid <- covid[order(covid$confirmed), ]

g <- ggplot(covid, aes(x = state, y = log10(confirmed), label = rate))
g +  geom_point(stat='identity', fill="black", size=8) + 
geom_segment(aes(y = 0, x = state, yend = log10(confirmed), xend = state), color = "black") +
geom_text(color="white", size=2) + 
labs(y = "Log of Confirmed Cases", x = "State/UT", 
     title = "Covid India Data", subtitle = "Confirmed Cases with rate as text") + 
coord_flip()

