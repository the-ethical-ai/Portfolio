#NOTE: While (almost) all of the code below appears in the associated Rmd and PDF files,
#the exact order in which each code chunk appears has been altered in the final versions for clarity
#sake. Similarly, while all of the code is documented as to its purpose here, the exact wording has been
#updated/refined for ease of reading in the R Markdown and PDF formats.

#PREAMBLE CODE (downloading/reading dataset and making the edx and validation datasets)

#Setting up the edx and validation datasets (CODE PROVIDED BY COURSE)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

#END OF PREAMBLE CODE

#EXPLORING THE EDX AND VALIDATION SETS

#Installing (if necessary) and calling the lubridate library.
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)

#Confirming the dimensions of the edx dataset
dim(edx) #This should be approx. 9M rows and 6 columns

#Confirming the dimensions of the validation dataset
dim(validation) #This should be approximately 1M rows and 6 columns.

#Looking at the start of the edx dataset
head(edx)

#Checking for missing values in edx or validation
anyNA(edx) 
anyNA(validation)

#Getting a basic numeric summary of the movie ratings
summary(edx$rating) 

#Checking how many unique users gave at least 1 review and how many movies have been rated.
edx %>%
  summarize(number_of_raters = n_distinct(userId), #there are 69878 raters.
            number_of_movies = n_distinct(movieId)) #there are 10677 movies.

#Checking the number of unique movie genres
length(unique(edx$genres)) #there are 797 difference genre listings.

#Checking the frequency of each rating 
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) #this is ordered in terms of most common to least common rating

#Visualizing the proportions of each rating with a bar chart.
edx %>%
  ggplot(aes(rating, y = ..prop..)) +
  geom_bar(color = "black", fill = "red") +
  scale_x_continuous(seq(0, 5, by = 0.5)) +
  ggtitle("Proportional Distribution for the Rating") +
  labs(x = "Rating", y = "Proportion of Ratings")

#Determining the 15 movies with the largest number of reviews.
most_rated_15 <- edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  top_n(15, count) %>%
  arrange(desc(count)) 

#Visualizing the top 15 most rated movies.
most_rated_15 %>%
  top_n(15) %>%
  ggplot(aes(reorder(title, count), count)) +
  geom_bar(color = "black", fill = "red", stat = "identity") +
  ggtitle("Most Commonly Rating Films") +
  coord_flip(y = c(0, 42000)) +
  labs(y = "Number of Ratings", x = "") +
  geom_text(aes(label = count), hjust = -0.2, size = 3)

#Determining the movies with 10 or fewer ratings and counting how many
least_rated <- edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  filter(count <= 10) 
nrow(least_rated) #There are 1139

#Determining the movies with 1000+ ratings and counting how many there are.
most_rated <- edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  filter(count >= 1000)
nrow(most_rated)

#Counting the ratings given by each user (and determining summary statistics)
user_rating_count <- edx %>%
  group_by(userId) %>%
  summarize(count = n())
summary(user_rating_count$count)

#NOTE: The median number of ratings is 62, which is significantly lower than the
#mean (128.8). This suggests that there are some users who rated far more 
#movies than the median number.This is better seen by the following visualization.

#Visualizing the distribution of the number of ratings given by each user.
user_rating_count %>%
  ggplot(aes(count)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "red", bins = 40) +
  scale_x_log10() +
  labs(x = "Number of Ratings", y = "Number of users") +
  ggtitle("Distribution of the Number of Ratings given by Users")

#NOTE: Users also tend to either rate higher or lower on average (i.e. not right at the average).
#To account for this, consider the following visualization of the average rating given by users.
edx %>%
  group_by(userId) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "red", bins = 40) +
  labs(x = "Ratings", y = "Number of Users") +
  ggtitle("Average Movie Rating given by Users") 

#NOTE: There is a general trend to rate higher than a 3. 

#PREPARING AND CHECKING THE TRAINING AND TESTING SETS (Coming from Edx dataset)

#Splitting the edx dataset into train and test sets (90% to train, 10% to test)
set.seed(1, sample.kind = "Rounding")
index <- createDataPartition(edx$movieId, times = 1, p = 0.9, list = FALSE) 
train_edx <- edx %>%
  slice(index)
test_edx <- edx %>%
  slice(-index)

#Checking the dimensions of both sets.
dim(train_edx) #This has 8,100,051 rows and 6 columns.
dim(test_edx) #This has 900,004 rows and 6 columns.

#Looking at the beginning of the sets.
head(train_edx)
head(test_edx)

#Removing any users and movies that are in the test_edx set but are not in the train_edx set.
test_edx <- test_edx %>%
  semi_join(train_edx, by = "movieId") %>%
  semi_join(train_edx, by = "userId")

#Confirming that no NA values have been generated
any(is.na(train_edx))
any(is.na(test_edx))


#BEGINNING OF THE ALGORITHM DEVELOPMENT

#SETTING UP THE RMSE FUNCTION (this is the loss function or the measure of what doing well means)
rmse <- function(true_rating, predicted_rating){
  sqrt(mean((true_rating - predicted_rating)^2))
}

#MODEL 1: Just the average (no other factors considered)

#Calculating the average rating for movies (ignores any possible biases)
mu <- mean(train_edx$rating) 
mu #This is the average rating of the movies in the training set.

#Calculating the RMSE score based on mu alone.
model1_rmse <- rmse(test_edx$rating, mu)
model1_rmse #This is 1.05931

#Making a table to track (and ultimately compare) RMSE scores across models.
model_comparison <- data.frame(method = "Average Movie Rating ONLY",
                               RMSE = model1_rmse)

#MODEL 2: Average + movie bias 

#Since there is a tendency to rate movies above the average rating, as seen by...
edx %>%
  group_by(userId) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "red", bins = 40) +
  labs(x = "Ratings", y = "Number of Users") +
  ggtitle("Average Movie Rating given by Users")
#The model needs to take account of this as well (recall that mu = 3.512478).

mu <- mean(train_edx$rating) #this is the average rating

#I will now use an approximation of the least square estimate of the user bias (because 
  #actually computing the estimate would make my computer die or freeze).
movie_bias <- train_edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

#Plotting the movie_bias b_1 scores (distance each movie's rating was from the overall average rating)
movie_bias %>%
  ggplot(aes(b_i)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "red", bins = 40) +
  labs(x = "Distance from Overall Average Movie Rating", 
       y = "Number of Movies") +
  ggtitle("")

#Predicting the ratings for test_edx
model2_predicted <- mu + test_edx %>%
  left_join(movie_bias, by = "movieId") %>%
  .$b_i

#Computing the RMSE score for model 2
model2_rmse <- rmse(model2_predicted, test_edx$rating)
model2_rmse #Approximately 0.942

#Adding model2_rmse to the model_comparison table (I'll show the whole table at the end).
model_comparison <- bind_rows(model_comparison, 
                              data.frame(method = "Movie Bias Model",
                                         RMSE = model2_rmse))

#MODEL 3: Average + User Bias

#Since model 2 only considered bias that comes from particular movies being rated higher or lower on average,
#I will now consider the effect that particular user's biases has on the ratings.
#NOTE: mu is still the same as before.
user_bias <- train_edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))

#NOTE: b_2 is the average difference between the overall movie rating average and that which a particular
  #user gives movies.

#Plotting the user_bias b_2 scores.
user_bias %>%
  ggplot(aes(b_u)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "red", bins = 40) +
  labs(x = "Distance from Overall Average Movie Rating", 
       y = "Number of Movies") +
  ggtitle("")
#This is mostly symmetric about 0 but there does appear to be a slight trend among users
  #to rate movies above the overall average rating (which is approximately 3.5).

#Predicting the ratings in the test set based on user bias
model3_predicted <- mu + test_edx %>%
  left_join(user_bias, by = "userId") %>%
  .$b_u

#Calculating the RMSE for model 3
model3_rmse <- rmse(model3_predicted, test_edx$rating)
model3_rmse #This is approximately 0.978, which is worse than model 2.

#Adding model 3 to the table
model_comparison <- bind_rows(model_comparison,
                              data.frame(method = "User Bias Model",
                                         RMSE = model3_rmse))

#So, predicting based on movie bias is better than predicting based on user bias, but
  #neither is particular good.

#MODEL 4: Average + User bias + Movie bias

#This model will take account of both the user bias (as in model 3) and the movie bias (as in model 2).
#NOTE: This is still an approximation due to technological restraints.
user_movie_biases <- train_edx %>%
  left_join(movie_bias, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_w = mean(rating - mu - b_i))

#Predicting ratings based on user and movie biases.
model4_predicted <- test_edx %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_movie_biases, by = "userId") %>%
  mutate(prediction = mu + b_i + b_w) %>%
  .$prediction

#Computing the RMSE for model 4
model4_rmse <- rmse(model4_predicted, test_edx$rating)
model4_rmse #This is approximately 0.8645122.

#Adding model 4 to the table.
model_comparison <- bind_rows(model_comparison,
                              data.frame(method = "Movie + User Bias Model",
                                         RMSE = model4_rmse))

#So, We now have an RMSE of 0.8645122, which is a significant improvment.

#CHECKING WHAT THE MODEL COMPARISONS CURRENTLY LOOKS LIKE
model_comparison %>% knitr:: kable()

#MODEL 5: Regularized Model 4

#I will use the tuning parameter lambda to penalize the films that received very few reviews
#as well as the users who only reviewed a handful of films. Since the optimal lambda is unknown,
#I will try multiple values.
lambdas <- seq(0, 10, 0.25)

#Computing the RMSE values based on the range of lambda values
rmse_regularized <- sapply(lambdas, function(l){
  mu <- mean(train_edx$rating)
  
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l)) #this is movie bias.
  
  b_u <- train_edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + l)) #this is movie + user bias.
  
  predicted_rating <- test_edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(prediction = mu + b_i + b_u) %>%
    .$prediction
  
  return(rmse(predicted_rating, test_edx$rating))
}) 

#Showing the lambdas compared to their rmse scores.
qplot(lambdas, rmse_regularized)

#Determining the lambda with the smallest RMSE
lambda <- lambdas[which.min(rmse_regularized)]
lambda #This is 5.5

#Applying the optimal lambda to get the best RMSE score
model5_rmse <- min(rmse_regularized)
model5_rmse #This is approx. 0.8638, which is below the required threshold.

#Adding model 5 to the table
model_comparison <- bind_rows(model_comparison,
                              data.frame(method = "Regularized User + Movie Bias Model",
                                         RMSE = model5_rmse))

#CHECKING WHAT THE MODEL COMPARISONS CURRENTLY LOOKS LIKE
model_comparison %>% knitr:: kable()


#MODEL 6: Regularized User + Movie + Time Bias Model

#In this model, I will also consider the date of each rating (i.e. the time stamp data). 
#To do this, I will convert the time stamp data into a more user-friendly format. 
#I will also provide visualizations of the average rating per year. 
#The dates will then be grouped by year and incorporated into model 4 in order to make
# a regularized model for User + Movie + Time bias. 

#Making a new data version of train_edx that includes the converted dates and rounds them
#to the nearest month.
train_edx_norm_dates <- train_edx %>%
  mutate(round_dates = round_date(as_datetime(timestamp), unit = "month"))
test_edx_norm_dates <- test_edx %>%
  mutate(round_dates = round_date(as_datetime(timestamp), unit = "month"))

#Plotting the average rating over time (based on monthly average rating.)
train_edx_norm_dates %>%
  group_by(round_dates) %>%
  summarize(average_rating = mean(rating)) %>%
  ggplot(aes(round_dates, average_rating)) +
  geom_point() +
  xlab("Date") +
  ylab("Average Movie Rating") +
  ggtitle("Average Movie Ratings over Time")
  
#The plot shows that (besides one rating in 1995), the average rating is slightly higher around 
#1997 to 2001, but the increase is not especially large. So, while the inclusion of 
#time as a possible source of bias should improve the model, the amount of improvement is
#likely to be quite small. 

lambdas <- seq(0, 10, 0.25)

#Computing the RMSE values based on the range of lambda values
rmse_regularized_2 <- sapply(lambdas, function(l){
  mu <- mean(train_edx$rating)
  
  b_i <- train_edx_norm_dates %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l)) #this is movie bias.
  
  b_u <- train_edx_norm_dates %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + l)) #this is movie + user bias.
  
  b_t <- train_edx_norm_dates %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(round_dates) %>%
    summarize(b_t = sum(rating - b_i - b_u - mu) / (n() + l))
  
  predicted_rating <- test_edx_norm_dates %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "round_dates") %>%
    mutate(prediction = mu + b_i + b_u + b_t) %>%
    .$prediction
  
  return(rmse(predicted_rating, test_edx$rating))
}) 

#Showing the lambdas compared to their rmse scores.
qplot(lambdas, rmse_regularized_2)

#Determining the lambda with the smallest RMSE
lambda <- lambdas[which.min(rmse_regularized_2)]
lambda #This is 5.75.

#Applying the optimal lambda to get the best RMSE score
model6_rmse <- min(rmse_regularized_2)
model6_rmse #This is approx. 0.86377, which is a mild improvement on model 5.


#Adding model 6 to the table
model_comparison <- bind_rows(model_comparison,
                              data.frame(method = "Regularized User + Movie + Time Bias Model",
                                         RMSE = model6_rmse))

#MODEL 7: Using the Recosystem package

#Installing the recosystem package if necessary.
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

#Calling the recosystem package.
library(recosystem)

#NOTE: The recosystem is based on matrix factorization and requires that 
#sets be formatted as a sparse matrix triplet. So, the first step is to format the train_edx
#and test_edx sets.
#NOTE: Since this will not need the timestamp information, I will go back to using train_edx
#and test_edx instead of train_edx_norm_dates and test_edx_norm_dates.
train_edx_sparMatTrip <- with(train_edx, 
                              data_memory(user = userId, item_index = movieId, rating = rating))
test_edx_sparMatTrip <- with(test_edx,
                             data_memory(user = userId, item_index = movieId, rating = rating))
#NOTE: The movieIds are being used to index the elements in the matrix.

#Making an r object of class "Recosys" (to be used subsequently)
rec_obj <- Reco()

#Tuning the recosys object (opts is how I am controlling the details).
  #dim is the number of latent factors in the data.
  #costp_12 is the regularization cost for user factors.
  #costq_12 is the regularization cost for item_index factors.
  #lrate is the rate at which the gradient will decline.
  #niter is the number of iterations.
  #NOTE: p and q are in reference to user and item_index in general, respectively.

#WARNING!!!: I do NOT recommend actually running this unless your computer has 32GB+ of RAM. 
  #I ran it on a 16GB, Intel i7 PC and it took over 30 minutes to run.

rec_tuner <- rec_obj$tune(train_edx_sparMatTrip,
                          opts = list(dim = c(10, 30),
                                      costp_12 = c(0.01, 0.1),
                                      costq_12 = c(0.01, 0.1),
                                      lrate = c(0.01, 0.1),
                                     niter = 15))
#Training the model
rec_obj$train(train_edx_sparMatTrip, 
              opts = c(rec_tuner$min, nthread = 4, niter = 30))

#Making the predictions
rec_predictions <- rec_obj$predict(test_edx_sparMatTrip, out_memory())

#Calculating the rmse
model7_rmse <- rmse(rec_predictions, test_edx$rating)
model7_rmse #this is approx. 0.785, which is by far the best model.

#Adding model 7 to list of models
model_comparison <- bind_rows(model_comparison,
                              data.frame(method = "Recosystem Matrix Factorization",
                                         RMSE = model7_rmse))

#Showing what all of the models look like based on Edx dataset.
model_comparison %>% knitr::kable()

#MODEL 8: Final Version (same idea as model 7 but with edx and validation sets)

#Making Sparse Matrix Triplet versions of Edx and Validation.
edx_sparMatTrip <- with(edx, data_memory(user = userId, item_index = movieId, rating = rating))
valid_sparMatTrip <- with(validation, data_memory(user = userId, item_index = movieId, rating = rating))


#Making an r object of class "Recosys" (to be used subsequently)
rec_obj_valid <- Reco()

#Tuning the recosys object rec_obj_valid

#WARNING!!!: I do NOT recommend actually running this unless your computer has 32GB+ of RAM. 
#I ran it on a 16GB, Intel i7 PC and it took over 60 minutes to run.

rec_tuner <- rec_obj_valid$tune(edx_sparMatTrip,
                          opts = list(dim = c(10, 30),
                                      costp_12 = c(0.01, 0.1),
                                      costq_12 = c(0.01, 0.1),
                                      lrate = c(0.01, 0.1),
                                      niter = 15))
#Training the model
rec_obj_valid$train(edx_sparMatTrip, 
              opts = c(rec_tuner$min, nthread = 4, niter = 30))

#Making the predictions
rec_predictions <- rec_obj_valid$predict(valid_sparMatTrip, out_memory())

#Calculating the rmse
model8_rmse <- rmse(rec_predictions, validation$rating)
model8_rmse #This is approx. 0.7809.

#Adding the final model to the table of model comparisons
model_comparison <- bind_rows(model_comparison,
                              data.frame(method = "Final Model (Recosystem)",
                                         RMSE = model8_rmse))

#Reviewing all of the models
model_comparison %>% knitr::kable()


#Sources used
#https://cran.r-project.org/web/packages/recosystem/recosystem.pdf
#https://github.com/yixuan/recosystem
#https://rdrr.io/cran/recosystem/man/tune.html
#Book: R for Everyone: Advanced Analytics and Graphics (Jared P. Lander)


