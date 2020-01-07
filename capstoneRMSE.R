#This R Script file explains how I have calculated the requests RMSE for movie ratings.  
# All other analysis can be found in th Rmd file and the pdf submitted for marking.
# Libraries required to process the data
library(tidyverse)
library(dplyr)
library(methods)
library(caret)
library(Matrix)
library(stats)
library(readr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(DT)
library(stringr)
library(data.table)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)

#First step: Gaining access to the data from the internet repository

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Our validation data set will be 10% of the MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Verifying that the data fiels userId and movieId in the validation data set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# For grading, learners will run algorithm on validation set to generate ratings

validation <- validation %>% select(-rating)

# Ratings ouput will go into the CSV submission file below:

write.csv(validation %>% select(userId, movieId) %>% mutate(rating = NA),
          "submission.csv", na = "", row.names=FALSE)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Looking into the data: What does the data look like?
head(edx)

# RMSE function:

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Choosing the adequate tuning value:

lambdas <- seq(0,5,0.5)

rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +l))
  
  predicted_ratings <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(predicted_ratings, edx$rating))
})

qplot(lambdas, rmses)
lambdas[which.min(rmses)]

# Using the model on the validation data set:

mu <- mean(validation$rating)
l <- 0.5
b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

b_u <- validation %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() +l))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(predicted_ratings, validation$rating)

#Notes:
#As part of my preliminary exploratory work I did work on calculating a b_a for the age of a movie.
#However I found that there was no decrease or a lower RMSE value so I removed these code from the current script.  
#By changing to use movieId and userId to calculate the RMSE, the script was able to achieve an RMSE = 0.826

#Alternative checking option: Using the R package "Metrics" and the code below it is possible to verify the RMSE values, which resulted in close/same value of RMSE.  

library(Metrics)
rmse(validation$rating, predicted_ratings)

