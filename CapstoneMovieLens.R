################################
# Capstone Movie Lens Code #
################################

# Create edx set, validation set
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Adding other libraries for visualizations
install.packages("ggthemes", repos = "http://cran.us.r-project.org")
library(ggthemes)
library(ggplot2)


#Data Inspection
head(edx)
summary(edx)

# Defining a RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2,na.rm=T))
}

# Modify the year as a column in the edx & validation datasets
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

# Splitting edx further to run data cleaning quicker
edx1 <- edx[1:2000000,]
edx2 <- edx[2000001:4000000,]
edx3 <- edx[4000001:6000000,]
edx4 <- edx[6000001:8000000,]
edx5 <- edx[8000001:9000055,]

# Splitting the genres in the datasets (note: this process will take a while)
splitedx1  <- edx1 %>% separate_rows(genres, sep = "\\|")
splitedx2 <- edx2 %>% separate_rows(genres, sep = "\\|")
splitedx3 <- edx3 %>% separate_rows(genres, sep = "\\|")
splitedx4 <- edx4 %>% separate_rows(genres, sep = "\\|")
splitedx5 <- edx5 %>% separate_rows(genres, sep = "\\|")
splitvalid <- validation %>% separate_rows(genres, sep = "\\|")

#Combining the parsed edx datasets
splitedx <- rbind(splitedx1, splitedx2, splitedx3, splitedx4, splitedx5)
#Removing the previously created datasets after combining 
rm(edx1, edx2, edx3, edx4, edx5, splitedx1, splitedx2, splitedx3, splitedx4, splitedx5)

#Number of unique movies and users
edx %>% summarize(no_of_users = n_distinct(userId), no_of_movies = n_distinct(movieId))

# Number of Movie ratings by genre
genre_rating <- splitedx%>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

print(genre_rating)

####Visualization of Variables#####
#Ratings Distribution
edx %>% group_by(rating) %>% summarize(n=n())

#Visualization of Ratings Distribution
edx %>% group_by(rating) %>% 
  summarize(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_line() +
  geom_point() +
  scale_y_log10() +
  ggtitle("Ratings Distribution") + 
  xlab("Rating") +
  ylab("Count") +
  theme_economist()

#Visualization of Movies Distribution based on ratings
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_economist()

# Visualization of User's rating distribution (right skewed)
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(color = "white") + 
  scale_x_log10() + 
  ggtitle("Users") +
  xlab("Number of Ratings") +
  ylab("Number of Users") +
  theme_economist()

# The distribution of mean ratings based on year of release
edx %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth() + 
  xlab("Year") +
  ylab("Rating") +
  theme_economist()
  

####Data Analysis####

# Initiate RMSE results as a data frame to compare results from different models
rmse_results <- data_frame()

# The training dataset's mean rating
mu <- mean(edx$rating)  

# model accounting for the movie effect b_i (rating minus the mean for each rating the movie received)
movieavg_norm <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movieplot<- movieavg_norm %>% qplot(b_i, geom = "histogram", bins = 30, data = .,color = I("black"))
movieplot

# model taking into account the user effect b_u
useravg_norm <- edx %>% 
  left_join(movieavg_norm, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

userplot <- useravg_norm %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))
userplot

#### Model Validation #####

##  Naive Model: mean only 
naive_rmse <- RMSE(validation$rating,mu)
## Test results based on simple prediction
naive_rmse
## Check results
rmse_results <- data_frame(method = "Using mean only", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

## Movie Effect Model ##
predicted_ratings_movie_norm <- validation %>% 
left_join(movieavg_norm, by='movieId') %>%
  mutate(pred = mu + b_i) 
model_1_rmse <- RMSE(validation$rating,predicted_ratings_movie_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
# saving results in a table
rmse_results %>% knitr::kable()


## Movie and User Effects Model ##
# Use test set,join movie averages & user averages
# Prediction equals the mean with user effect b_u & movie effect b_i
predicted_ratings_user_norm <- validation %>% 
  left_join(movieavg_norm, by='movieId') %>%
  left_join(useravg_norm, by='userId') %>%
  mutate(pred = mu + b_i + b_u) 

# test and save rmse results 

model_2_rmse <- RMSE(validation$rating,predicted_ratings_user_norm$pred)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User Effect Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

## Regularized Movie and User Effects Model ##
# lambda is a tuning parameter (cross-validation is used to choose it)
lambdas <- seq(0, 10, 0.25)
# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take a while

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation$rating,predicted_ratings))
})

# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
lambda

# Compute regularized estimates of b_i using lambda
movieavg_reg <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

# Compute regularized estimates of b_u using lambda

useravg_reg <- edx %>% 
  left_join(movieavg_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())

# Predict ratings

predicted_ratings_reg <- validation %>% 
  left_join(movieavg_reg, by='movieId') %>%
  left_join(useravg_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred

# Testing and saving results

model_3_rmse <- RMSE(validation$rating,predicted_ratings_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie and User Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()


##Regularized with all Effects Model ##

# The approach utilized in the above model is implemented below with the added genres and year effects
# b_y and b_g represent the year & genre effects, respectively

lambdas <- seq(0, 20, 1)

# Note: the below code could take some time 

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- splitedx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- splitedx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- splitedx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())
  
  b_g <- splitedx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda), n_g = n())
  
  predicted_ratings <- splitvalid %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    left_join(b_g, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
    .$pred
  
  return(RMSE(splitvalid$rating,predicted_ratings))
})

# Computing new predictions using the optimal lambda
# Testing and saving the results 

qplot(lambdas, rmses)  
lambda_2 <- lambdas[which.min(rmses)]
lambda_2

movie_reg_avgs_2 <- splitedx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_2), n_i = n())

user_reg_avgs_2 <- splitedx %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_2), n_u = n())

year_reg_avgs <- splitedx %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_2), n_y = n())


genre_reg_avgs <- splitedx %>%
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'year') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_2), n_g = n())

predicted_ratings <- splitvalid %>% 
  left_join(movie_reg_avgs_2, by='movieId') %>%
  left_join(user_reg_avgs_2, by='userId') %>%
  left_join(year_reg_avgs, by = 'year') %>%
  left_join(genre_reg_avgs, by = 'genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
  .$pred

model_4_rmse <- RMSE(splitvalid$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie, User, Year, and Genre Effect Model",  
                                     RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()

## Results ##

# RMSE resutls overview
rmse_results %>% knitr::kable()