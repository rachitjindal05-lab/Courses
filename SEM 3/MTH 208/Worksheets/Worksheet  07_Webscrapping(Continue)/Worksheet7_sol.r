library(rvest)
library(tidyverse)

##### Problem 1  ####

library(jsonlite)

html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

# Extract poster links directly (do not text them first)
urls <- html %>%
  html_elements("a.article_movie_poster") %>%
  html_attr("href")

# Extract titles from the corresponding title divs
titles <- html %>%
  html_elements(".article_movie_title") %>%
  html_text2() %>%
  str_squish()

movie_list_df <- tibble(
  title = titles,
  url   = urls
)

# Check
num_movies <- nrow(movie_list_df)
print(num_movies)
print(movie_list_df %>% slice(1:10))

# Collect rows in a list
reviews_clean <- numeric(length = num_movies)
ratings_clean <- numeric(length = num_movies)
tomato_clean <- numeric(length = num_movies)
popcorn_clean <- numeric(length = num_movies)

# Simple for loop over the urls in movie_list_df
for (i in seq_len(num_movies)) {
  this_title <- movie_list_df$title[i]
  this_url   <- movie_list_df$url[i]
  
  cat("Now working on", i, ":", this_title, "\n")
  
  page <- read_html(this_url)
  scripts <- page %>% html_elements("media-scorecard-manager script")
  txt <- scripts %>% html_text() %>% head()
  
  sc <- fromJSON(txt)  # parse JSON into a list
  
  tomato_clean[i]      <- parse_number(sc$criticsScore$scorePercent)   # "100%" -> 100
  popcorn_clean[i]     <- parse_number(sc$audienceScore$scorePercent)  # "91%"  -> 91
  reviews_clean[i]   <- as.integer(sc$criticsScore$reviewCount)      # 137
  ratings_clean[i] <- parse_number(sc$audienceScore$bandedRatingCount) # "500+ Ratings" -> 500
  ratings_clean[i] <- pmin(ratings_clean[i], 1000, na.rm = TRUE)   # cap at 1000
}

data <- data.frame(tomato_clean, popcorn_clean, reviews_clean, ratings_clean)
head(data)
# Save the dataset
save(data, file = "TomatoList.Rdata")
cat("Saved results to TomatoList.Rdata\n")



##### Problem 2 ####

library(imager)

# all poster links are available here
html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

all_poster_links <- html %>% html_elements(".article_poster")%>%
  html_attr("src")


posters <- list(length = length(all_poster_links))
for(i in 1:length(all_poster_links))
{
  file = paste0("movie", i, ".jpeg")
  print(file)
  download.file(all_poster_links[i], file)
  posters[[i]] <- load.image(file)
}

# creating a function that calculates the distance from
# a given color. Code was in Worksheet 4 solutions
diff.col <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  # return the mean distance from color
  return(mean(dist))
}

## Now calculating distance average distance black
# I am going to calculate how dark a movie is
black <- numeric(length = 100)
for(i in c(1:100))
{
  black[i] <- diff.col(posters[[i]], col = c(0,0,0))
  print(i)
}

# Any relationship between dark colors and ratings?
# possibly not!
plot(black, tomato_clean)
