library(tidyverse)
library(lubridate)
library(caret)


edx <- readRDS(file = "./rda/full_edx.rds")
train_set <- edx

mu <- mean(train_set$rating)

# factorize userId and movieId
# create a reviewed_date and reviewed_year feature
train_set <- train_set %>%
  mutate(#userId = factor(userId),
    #movieId = factor(movieId),
    review_date = date(as_datetime(timestamp)),
    review_year = year(review_date))


cv_splits <- createFolds(train_set$rating, k = 5, returnTrain = FALSE)

mjb <- data.frame(matrix(nrow = 0, ncol = 3))

for (lambda1 in 1:24) {
  print(paste("lambda1", lambda1))
  for (lambda2 in 1:9) {
        r <- numeric()
        print(paste("..lambda2", lambda2))
        for (i in 1:length(cv_splits)) {
          print(paste("....fold", i))
          test_i <- unlist(cv_splits[i])
          cv_test_set <- train_set[test_i,]
          cv_train_set <- train_set[-test_i,]
          
            # find b_i(t), the rating bias for each movie as a function of time (year)
          #lambda1 <- 0 #3
          movie_time_bias <- cv_train_set %>% 
            group_by(movieId, review_year) %>% 
            summarize(b_i_time = sum(rating - mu) / (n() + lambda1))
          cv_train_set <- left_join(cv_train_set, movie_time_bias, by = c("movieId", "review_year"))
          
          # find the user rating bias for each user
          #lambda2 <- 0 #5
          user_bias <- cv_train_set %>% 
            group_by(userId) %>% 
            summarize(b_u = sum(rating - mu - b_i_time) / (n() + lambda2) )
          cv_train_set <- left_join(cv_train_set, user_bias, by = "userId")
          
          cv_pred <- cv_test_set %>% 
            left_join(movie_time_bias, by = c("movieId", "review_year")) %>% 
            mutate(b_i_time = if_else(is.na(b_i_time), 0, b_i_time)) %>%   
            left_join(user_bias, by = "userId") %>% 
            mutate(pred_c = mu + b_i_time + b_u)
        
            r <- rbind(r, RMSE(cv_pred$pred_c, cv_test_set$rating))
        
        }
      
    rmse <- mean(r)
    mjb <- rbind(mjb, data.frame(lambda1, lambda2, rmse))
      
    }
}

mjb <- arrange(mjb, rmse)
print(mjb)
