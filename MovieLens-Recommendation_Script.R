############################################################
#        Movie Recommendation system (MovieLens Dataset)   #
############################################################

# Library for data operations
library(dplyr)
# Library for data tidying
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# Library for Classification And REgression Training
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Reading data. Used dataset provided by instructor on Google drive as 
# code of building dataset was taking too much time and not ending
# Dataset location https://drive.google.com/drive/folders/1IZcBBX0OmL9wu9AdzMBFUG8GoPbGQ38D
edx <- readRDS("C:/Ragini/MovieLens-Recommender/edx.rds")
validation <- readRDS("C:/Ragini/MovieLens-Recommender/validation.rds")

# Data exploration
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

#Data visualization
head(edx)

##Partitioning edx data to create a model
set.seed(755)
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Building the Recommendation System using edx dataset
# lm(rating ~ as.factor(movieId) + as.factor(userId))

mu_hat <- mean(train_set$rating)
mu_hat

mu <- mean(train_set$rating) 

#Calculating movie averages
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#Calculating user averages
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Predicting ratings on test set
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- data_frame(method="Movie + User Effects Model on edx test set",  
                           RMSE = model_rmse )
rmse_results %>% knitr::kable()

### Validating Recommendation System on Validation set
#Using the model built in previous steps to predict rating for validation set
# lm(rating ~ as.factor(movieId) + as.factor(userId))

mu_hat_v <- mean(validation$rating)
mu_hat_v

mu_v <- mean(validation$rating) 


movie_avgs_v <- validation %>% 
  group_by(movieId) %>% 
  summarize(b_i_v = mean(rating - mu_v))


user_avgs_v <- validation %>% 
  left_join(movie_avgs_v, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u_v = mean(rating - mu_v - b_i_v))

predicted_ratings <- validation %>% 
  left_join(movie_avgs_v, by='movieId') %>%
  left_join(user_avgs_v, by='userId') %>%
  mutate(pred = mu_v + b_i_v + b_u_v) %>%
  .$pred

model_rmse_v <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User Effects Model on Validation Set",  
                                                   RMSE = model_rmse_v ))
rmse_results %>% knitr::kable()