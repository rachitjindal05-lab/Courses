# Load required packages
library(rvest)           # core HTML parsing
library(tidyverse)       # data wrangling and CSV output


url <- "https://home.iitk.ac.in/~akasha/index.html"
page <- read_html(url)
h_elem <- html_element(page, "title") # extract <title>
title <-html_text(h_elem)
print(title)

# Alternatively
title <- page %>% 
  html_element("title") %>% 
  html_text()
print(title)

# A single pipe!
url %>% read_html %>% html_element("title") %>% html_text() %>% print

# Wikipedia
url <- "https://www.wikipedia.org"
url %>% read_html %>% html_element("title") %>% html_text() %>% print


# Research page
url <- "https://home.iitk.ac.in/~akasha/research.html"
page <- read_html(url)
h_elem <- html_elements(page, "a")  # extract <a>
links <- html_attr(h_elem, "href") # extract the "href" attribute 
head(links)

# equivalently
links <- page %>% html_elements("a") %>% html_attr("href")

head(links)
length(links)




## Problem 1

url <- "https://home.iitk.ac.in/~akasha/research.html"

page <- read_html(url)

# Extract all <img> tags and get their src attributes
img_urls <- page %>%
  html_elements("img") %>%
  html_attr("src")

# Print all urls
head(img_urls,length(img_urls))

# Convert relative URLs to absolute
img_urls <- url_absolute(img_urls, base = url)

# Print all urls
head(img_urls,length(img_urls))



## Problem 2

url <- "https://cran.r-project.org/mirrors.html"
page <- read_html(url)

urls <- page %>% html_elements("td a") %>% html_attr("href")
head(urls)

# server names

names <- page %>% html_elements("td") %>% html_text()

mirror_data <- data.frame(Name = names, URL = urls)
head(mirror_data, 5)
head(mirror_data$URL, 5)
head(mirror_data$Name, 5)



## Problem 3

url <- "https://quotes.toscrape.com/"
page <- read_html(url)

quotes <- page %>% html_elements(".quote .text") %>% html_text()
authors <- page %>% html_elements(".quote .author") %>% html_text()

data.frame(Quote = quotes[1:5], Author = authors[1:5])



## Problem 4
html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

# getting a tag in titlecolumn class and the text in there
name <- html %>%  
  html_elements(".article_movie_title a") %>%
  html_text()
length(name)

# Ranking
rank <- html %>%
  html_elements(".countdown-index") %>%
  html_text()

# Removing # using substring and changing to number
rank <- as.numeric(substring(rank, first = 2))


# score
score <- html %>%
  html_elements(".tMeterScore") %>%
  html_text()

# remove % and turn to numeric
extract <- strsplit(score, split = "%")
score <- sapply(extract, c) %>% as.numeric()

# year
year <- html %>%
  html_elements(".subtle.start-year") %>%
  html_text()

# remove brackets
year <- as.numeric(substr(year, start = 2, stop = 5))

# Director  (can be multiple directors)
dir <- html %>%  
  html_elements(".info.director")

# getting a list of directors for each movie
all_dirs <- sapply(dir, function(a) a %>% html_elements("a") %>% html_text())

# combine together
all_dirs <- sapply(all_dirs, paste, collapse = ", ")

# making the dataset
netflix <- data.frame("name" = name,
                      "year" = year,
                      "tomato_score" = score,
                      "rank" = rank,
                      "director" = all_dirs)


head(netflix)



## Problem 5

tennis <- read_html("https://www.espn.com/tenis/rankings/_/tipo/wta")

tabless <- html_table(tennis)
table <- tabless[[1]]
table
clean.table <- data.frame("Rank" = table$RK, 
                          "Name" = table$NAME, 
                          "Points" = table$POINTS, 
                          "Age" = table$AGE)

points <- clean.table$Points
points <- gsub("," , "" , points)
points <- as.numeric(points)

clean.table$Points <- points

clean.table
write.csv(clean.table, "wta_rankings.csv", row.names = FALSE)






















