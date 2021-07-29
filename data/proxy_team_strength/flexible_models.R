
#read in data
together_final <- read_csv(
  "./data/proxy_team_strength/clean_team_strength_columns.csv")
   
#Elastic Net just team strength

library(glmnet)
model_data <-  together_final %>%
  dplyr::select(net_rating_diff, games_played, game_net_rating) %>% 
  mutate(games_playedXnet_rating_diff = games_played * net_rating_diff)


matrix_x <- model_data %>%
  dplyr::select(-game_net_rating) %>%
  as.matrix()
response_y <- model_data$game_net_rating

set.seed(2020)
fold_id <- sample(rep(1:10, length.out = nrow(matrix_x)))

# Run cross-validation over different alpha values
cv_en_25 <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 0.25)
cv_en_50 <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 0.50)
cv_ridge <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 1)


# Find which had the lowest holdout error rate:
cv_en_25$cvm
which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm), min(cv_ridge$cvm),
            min(cv_lasso$cvm)))
# [1] 4

library(broom)

tidy(cv_lasso) %>%
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cv_lasso$lambda.min) +
  geom_vline(xintercept = cv_lasso$lambda.1se,
             linetype = "dashed",
             color = "red") +
  scale_x_log10() +
  theme_bw()

set.seed(200)
model_data <- model_data %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
holdout_predictions <- 
  map_dfr(unique(model_data$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- model_data %>% filter(test_fold == holdout)
            train_data <- model_data %>% filter(test_fold != holdout)
            # Repeat for matrices
            test_x <- as.matrix(dplyr::select(test_data, -game_net_rating))
            train_x <- as.matrix(dplyr::select(train_data, -game_net_rating))
            # Train models:
            lm_model <- lm(game_net_rating ~ ., data = train_data)
            ridge_model <- cv.glmnet(train_x, train_data$game_net_rating, alpha = 0)
            lasso_model <- cv.glmnet(train_x, train_data$game_net_rating, alpha = 1)
            en_model <- cv.glmnet(train_x, train_data$game_net_rating, alpha = .25)
            # Return tibble of holdout results:
            tibble(lm_preds = predict(lm_model, newdata = test_data),
                   ridge_preds = as.numeric(predict(ridge_model, newx = test_x)),
                   lasso_preds = as.numeric(predict(lasso_model, newx = test_x)),
                   en_preds = as.numeric(predict(en_model, newx = test_x)),
                   test_actual = test_data$game_net_rating, test_fold = holdout) 
          })

holdout_predictions %>%
  pivot_longer(lm_preds:en_preds, 
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>% 
  ggplot(aes(x = type, y = rmse)) + 
  geom_point() + theme_bw() +
  stat_summary(fun = mean, geom = "point", 
               color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "red")


#Elastic Net With fatigue metrics
model_data_fatigue <-  together_final %>%
  dplyr::select(net_rating_diff, games_played, game_net_rating, b2b_2nd, three_in_four, rest_diff, travel_3_hours_back) %>% 
  mutate(games_playedXnet_rating_diff = games_played * net_rating_diff)

matrix_x <- model_data_fatigue %>%
  dplyr::select(-game_net_rating) %>%
  as.matrix()
response_y <- model_data_fatigue$game_net_rating

set.seed(2020)
fold_id <- sample(rep(1:10, length.out = nrow(matrix_x)))

# Run cross-validation over different alpha values
cv_en_25 <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 0.25)
cv_en_50 <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 0.50)
cv_ridge <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(matrix_x, response_y, foldid = fold_id, alpha = 1)


# Find which had the lowest holdout error rate:
cv_en_25$cvm
which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm), min(cv_ridge$cvm),
            min(cv_lasso$cvm)))
# [1] 1

library(broom)

tidy(cv_en_25) %>%
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cv_en_25$lambda.min) +
  geom_vline(xintercept = cv_en_25$lambda.1se,
             linetype = "dashed",
             color = "red") +
  scale_x_log10() +
  theme_bw()

set.seed(200)
model_data_fatigue <- model_data_fatigue %>% mutate(test_fold = sample(rep(1:5, length.out = n())))
holdout_predictions <- 
  map_dfr(unique(model_data_fatigue$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- model_data_fatigue %>% filter(test_fold == holdout)
            train_data <- model_data_fatigue %>% filter(test_fold != holdout)
            # Repeat for matrices
            test_x <- as.matrix(dplyr::select(test_data, -game_net_rating))
            train_x <- as.matrix(dplyr::select(train_data, -game_net_rating))
            # Train models:
            lm_model <- lm(game_net_rating ~ ., data = train_data)
            ridge_model <- cv.glmnet(train_x, train_data$game_net_rating, alpha = 0)
            lasso_model <- cv.glmnet(train_x, train_data$game_net_rating, alpha = 1)
            en_model <- cv.glmnet(train_x, train_data$game_net_rating, alpha = .25)
            # Return tibble of holdout results:
            tibble(lm_preds = predict(lm_model, newdata = test_data),
                   ridge_preds = as.numeric(predict(ridge_model, newx = test_x)),
                   lasso_preds = as.numeric(predict(lasso_model, newx = test_x)),
                   en_preds = as.numeric(predict(en_model, newx = test_x)),
                   test_actual = test_data$game_net_rating, test_fold = holdout) 
          })

holdout_predictions %>%
  pivot_longer(lm_preds:en_preds, 
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>% 
  ggplot(aes(x = type, y = rmse)) + 
  geom_point() + theme_bw() +
  stat_summary(fun = mean, geom = "point", 
               color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "red")

#PCA ----

model_x <- together_final %>%
  dplyr::select(net_rating_diff, games_played, game_net_rating) %>% 
  mutate(games_playedXnet_rating_diff = games_played * net_rating_diff) %>%
  as.matrix()
pca_together <- prcomp(model_x, center = TRUE, scale = TRUE)
summary(pca_together)

library(broom)
pca_together %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1 / ncol(model_x),
             color = "darkred", 
             linetype = "dashed") +
  theme_bw()

# this shows us we should use 2 components



#PCA Regression


model_data <-  together_final %>%
  dplyr::select(net_rating_diff, games_played, game_net_rating) %>% 
  mutate(games_playedXnet_rating_diff = games_played * net_rating_diff)


library(pls)
nba_pcr_fit <- pcr(game_net_rating ~ ., ncomp = 2, scale = TRUE, data = model_data)
summary(nba_pcr_fit)

set.seed(2013)
library(caret)
cv_model_pcr <- train(
  game_net_rating ~ ., 
  data = model_data, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("center", "scale"),
  tuneLength = ncol(model_data) - 1)
ggplot(cv_model_pcr) + theme_bw()

summary(cv_model_pcr$finalModel)


#PCR with fatigue metrics

model_data_fatigue <-  together_final %>%
  dplyr::select(net_rating_diff, games_played, game_net_rating, b2b_2nd, three_in_four, rest_diff, travel_3_hours_back) %>% 
  mutate(games_playedXnet_rating_diff = games_played * net_rating_diff)

library(pls)
nba_pcr_fit_fatigue <- pcr(game_net_rating ~ ., ncomp = 2, scale = TRUE, data = model_data_fatigue)
summary(nba_pcr_fit_fatigue)

set.seed(2013)
library(caret)
cv_model_pcr_fatigue <- train(
  game_net_rating ~ ., 
  data = model_data_fatigue, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("center", "scale"),
  tuneLength = ncol(model_data_fatigue) - 1)
ggplot(cv_model_pcr_fatigue) + theme_bw()

summary(cv_model_pcr_fatigue$finalModel)



