visitors <- filter(together, Visitor == TRUE)

travel_visitors_lm <- lm(game_net_rating ~ win_percent_diff + rest_diff+ b2b_2nd + three_in_four + travel_3_hours_back, 
                         data = visitors)

summary(travel_visitors_lm)

opp_rest <- travel %>%
  select("Date", "Team", "Rest") %>%
  rename(opp_rest = "Rest") 

together <- merge(x = together, y = opp_rest,
                  by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

set.seed(2004)
together_train <- together_netRating_allWindows_15_30 %>%
  mutate(is_train = sample(rep(0:1, length.out = nrow(together_netRating_allWindows_15_30))))

library(mgcv)
init_logit_gam <- gam(game_net_rating ~ net_rating_5gameWindow + net_rating_diff,
                      data = filter(together_train, is_train == 1), 
                      family = binomial, method = "REML")


# lm with interactions ---------------------------------------------------------------------

visitor <- check_togetherNet35_40_csv %>%
  filter(Visitor == TRUE) 

home <- check_togetherNet35_40_csv %>%
  filter(Visitor == FALSE)

v_h_data <- left_join(visitor, home, by = "game_id", suffix = c("_v", "_hm"))

summary(lm(game_net_rating ~ net_rating_5gameWindow + net_rating_diff, data = check_togetherNet35_40_csv))



# best team strength ------------------------------------------------------

clean_team_strength_columns <- read_csv("./data1/clean_team_strength_columns1.csv")

together_Windows <- merge(together, clean_team_strength_columns, by =  c("Date", "Team", "Opponent", "game_id", "game_net_rating", "score_diff", "net_rating_diff"))

summary(lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + 
             g11_15_centered_rolling_net_diff+ g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + 
             g26_30_centered_rolling_net_diff + net_rating_diff + games_played * net_rating_diff + b2b_2nd_vis +
           three_in_four_vis + rest_diff + travel_3_hours_back_vis, data = together_final))


# xgboost -----------------------------------------------------------------

model_data <- together_Windows %>%
  dplyr::select(game_net_rating, wind_10_center, hollow_wind20_cent, hollow_wind30_cent, hollow_wind40_cent, hollow_wind50_cent, 
                  hollow_wind60_cent, net_rating_diff, games_played) %>%
  mutate(interaction = games_played * net_rating_diff)

library(vip)
library(caret)
library(xgboost)
xgboost_tune_grid <- expand.grid(nrounds = seq(from = 20, to = 200, by = 20),
                                 eta = c(0.025, 0.05, 0.1, 0.3), gamma = 0,
                                 max_depth = c(1, 2, 3, 4), colsample_bytree = 1,
                                 min_child_weight = 1, subsample = 1)
xgboost_tune_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
set.seed(1937)
xgb_tune <- train(x = as.matrix(dplyr::select(model_data, -game_net_rating)),
                  y = model_data$game_net_rating, trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid, 
                  objective = "reg:squarederror", method = "xgbTree",
                  verbose = TRUE)
xgb_tune$bestTune

xgb_fit_final <- xgboost(data = as.matrix(dplyr::select(model_data, -game_net_rating)),
                         label = model_data$game_net_rating, objective = "reg:squarederror",
                         nrounds = xgb_tune$bestTune$nrounds,
                         params = as.list(dplyr::select(xgb_tune$bestTune,
                                                        -nrounds)), 
                         verbose = 0)
vip(xgb_fit_final) + theme_bw()



# final data set ----------------------------------------------------------

together_final <- read.csv("./data1/together3.csv")

clean_team_strength <- read_csv("./data1/clean_team_strength_columns3.csv")

clean_team_strength <- clean_team_strength %>%
  select(c(Date, visitor, games_played, win_percent_diff, 
           hollow_wind60_cent, hollow_wind50_cent,hollow_wind40_cent, hollow_wind30_cent, 
           hollow_wind20_cent, wind_10_center,wind_40_hollow, wind_35_hollow, wind_30_hollow, 
           wind_25_hollow, wind_20_hollow, wind_15_hollow, wind_10_hollow_5,wind_5))

together <- merge(together_final, strength_proxies,
                  by.x = c("Date", "visitor"), by.y = c("Date","visitor"))

together <- together %>%
  rename(g1_5_rolling_net_diff = wind_5,
         g6_10_rolling_net_diff = wind_10_hollow_5,
         g11_15_rolling_net_diff = wind_15_hollow,
         g16_20_rolling_net_diff = wind_20_hollow,
         g21_25_rolling_net_diff = wind_25_hollow,
         g26_30_rolling_net_diff = wind_30_hollow,
         g31_35_rolling_net_diff = wind_35_hollow,
         g36_40_rolling_net_diff = wind_40_hollow,
         g1_5_centered_rolling_net_diff= wind_10_center,
         g6_10_centered_rolling_net_diff= hollow_wind20_cent,
         g11_15_centered_rolling_net_diff= hollow_wind30_cent,
         g16_20_centered_rolling_net_diff= hollow_wind40_cent,
         g21_25_centered_rolling_net_diff= hollow_wind50_cent,
         g26_30_centered_rolling_net_diff= hollow_wind60_cent)

together <- together %>%
  rename(win_percent_diff = "win_percent_diff.y") %>%
  select(-c("win_percent_diff.x"))

summary(lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + 
             g11_15_centered_rolling_net_diff+ g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + 
             g26_30_centered_rolling_net_diff + net_rating_diff + games_played * net_rating_diff,
   data = together_final))
   

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

#PCA

model_x <- together_final %>%
  dplyr::select(g1_5_centered_rolling_net_diff, g6_10_centered_rolling_net_diff, g11_15_centered_rolling_net_diff, 
                g16_20_centered_rolling_net_diff, g21_25_centered_rolling_net_diff, g26_30_centered_rolling_net_diff, 
                net_rating_diff, games_played) %>% 
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


pca_together %>%
  augment(nfl_model_data) %>%
  bind_cols({
    nfl_teams_data %>% 
      filter(season >= 2006) %>%
      dplyr::select(season, team)
  }) %>%
  unite("team_id", team:season, sep = "-",
        remove = FALSE) %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, 
             color = season)) +
  geom_text(aes(label = team_id), alpha = 0.9) +
  scale_color_gradient(low = "purple", high = "green") +
  theme_bw() + theme(legend.position = "bottom")


library(factoextra)
fviz_eig(pca_nfl)

#PCR


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

set.seed(2013)
cv_model_pcr_onese <- train(
  game_net_rating ~ ., 
  data = model_data, 
  method = "pcr",
  trControl = 
    trainControl(method = "cv", number = 10,
                 selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(model_data) - 1)

summary(cv_model_pcr_onese$finalModel)

cv_model_pcr_onese$finalModel$coefficients

set.seed(2013)
cv_model_pls <- train(
  game_net_rating ~ ., 
  data = model_data, 
  method = "pls",
  trControl = 
    trainControl(method = "cv", number = 10,
                 selectionFunction = "oneSE"), 
  preProcess = c("center", "scale"),
  tuneLength = ncol(model_data) - 1)
ggplot(cv_model_pls) + theme_bw()

cv_model_pls$finalModel$coefficients

library(vip)
vip(cv_model_pls, num_features = 10,
    method = "model") +
  theme_bw()

library(pdp)
partial(cv_model_pls, "g1_5_centered_rolling_net_diff", plot = TRUE)

#PCR with fatigue

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

set.seed(2013)
cv_model_pcr_fatigue_onese <- train(
  game_net_rating ~ ., 
  data = model_data_fatigue, 
  method = "pcr",
  trControl = 
    trainControl(method = "cv", number = 10,
                 selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(model_data_fatigue) - 1)

summary(cv_model_pcr_fatigue_onese$finalModel)

cv_model_pcr_fatigue_onese$finalModel$coefficients

set.seed(2013)
cv_model_pls_fatigue <- train(
  game_net_rating ~ ., 
  data = model_data_fatigue, 
  method = "pls",
  trControl = 
    trainControl(method = "cv", number = 10,
                 selectionFunction = "oneSE"), 
  preProcess = c("center", "scale"),
  tuneLength = ncol(model_data_fatigue) - 1)
ggplot(cv_model_pls_fatigue) + theme_bw()

cv_model_pls_fatigue$finalModel$coefficients

library(vip)
vip(cv_model_pls_fatigue, num_features = 10,
    method = "model") +
  theme_bw()

library(pdp)
partial(cv_model_pls_fatigue, "g1_5_centered_rolling_net_diff", plot = TRUE)

# best model 

together_final <- read_csv("./data1/final_together.csv")

summary(lm(game_net_rating ~ wind_10_center + hollow_wind20_cent + hollow_wind30_cent+
             hollow_wind40_cent + hollow_wind50_cent + hollow_wind60_cent + net_rating_diff +
             games_played * net_rating_diff,
           data = together_final))


summary(lm(game_net_rating ~ net_rating_diff,
           data = together_final))

summary(lm(game_net_rating ~ net_rating_diff + games_played + game_net_rating + b2b_2nd + three_in_four +
             rest_diff + travel_3_hours_back + travel_3_hours_forward + games_played * net_rating_diff,
           data = together_final))

lm(score_diff ~ wind_5 + wind_10_hollow_5 + wind_15_hollow + wind_20_hollow +
     wind_25_hollow + wind_30_hollow + wind_35_hollow + wind_40_hollow,
   data = together_net_Rating_allWindows))
