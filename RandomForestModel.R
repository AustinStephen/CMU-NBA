# Decision Tree -----------------------------------------------------------
library(rpart)
library(rpart.plot)
library(caret)
together <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/together.csv")


caret_nba_tree <- train(game_net_rating ~ g1_5_rolling_net_diff + rest_diff + windowed_distance_diff + travel_3_hours_back_vis,
                        data = together, method = "rpart",
                        trControl = trainControl(method = "cv", number = 10),
                        tuneLength = 20)

#RMSE and complexity parameter
ggplot(caret_nba_tree) + theme_bw()

#Plot of decision tree
rpart.plot(caret_nba_tree$finalModel)


library(tidyverse)
library(dplyr)

# Random Forest -----------------------------------------------------------
columns <- c("game_net_rating", "g1_5_centered_rolling_net_diff", "g6_10_centered_rolling_net_diff", 
             "g11_15_centered_rolling_net_diff", "g16_20_centered_rolling_net_diff" , 
             "g21_25_centered_rolling_net_diff" , "g26_30_centered_rolling_net_diff" , "net_rating_diff",
             "rest_diff",  "b2b_2nd_vis", "three_in_four_vis",
              "travel_3_hours_back_vis", 
              "windowed_distance_diff") 

together_rf <- dplyr:: select(together, columns)

library(ranger)
library(Rcpp)

init_nba_rf <- 
  ranger(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff 
         + g11_15_centered_rolling_net_diff + g16_20_centered_rolling_net_diff 
         + g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff, 
         data = together_rf,
         num.trees = 500, importance = "impurity", mtry = 2, min.node.size = 10)

init_nba_rf

#high OOB prediction error 
#low R squared

#Gives us the results of ranger



#Importance graph
library(vip)
vip(init_nba_rf, geom = "point") + theme_bw()

#Caret doesnt let us tune # of trees so instead we tune m try
rf_tune_grid <-
  expand.grid(mtry = 2,
              splitrule = "variance",
              min.node.size = 5)

set.seed(1917)

caret_nba_rf <-
  train(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff +
          g11_15_centered_rolling_net_diff +  g16_20_centered_rolling_net_diff + 
          g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff + net_rating_diff
        + rest_diff + b2b_2nd_vis + three_in_four_vis + travel_3_hours_back_vis + windowed_distance_diff,
        data = together_rf,
        method = "ranger",
        num.trees = 200,
        trControl = trainControl(method = "cv", number = 10),
        tuneGrid = rf_tune_grid)

#RMSE versus number of randomly selected predictors mtry
ggplot(caret_nba_rf) + theme_bw()


library(xgboost)
#Tune
xgboost_tune_grid <- expand.grid(nrounds = seq(from = 20, to = 200, by = 20),
                                 eta = c(0.025, 0.05, 0.1, 0.3), gamma = 0,
                                 max_depth = c(1, 2, 3, 4), colsample_bytree = 1,
                                 min_child_weight = 1, subsample = 1)

xgboost_tune_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
set.seed(1937)
xgb_tune <- train(x = as.matrix(dplyr::select(together_rf, -game_net_rating)),
                  y = model_mlb_data$war, trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid, 
                  objective = "reg:squarederror", method = "xgbTree",
                  verbose = TRUE)
xgb_tune$bestTune

xgb_fit_final <- xgboost(data = as.matrix(dplyr::select(model_mlb_data, -war)),
                         label = model_mlb_data$war, objective = "reg:squarederror",
                         nrounds = xgb_tune$bestTune$nrounds,
                         params = as.list(dplyr::select(xgb_tune$bestTune,
                                                        -nrounds)), 
                         verbose = 0)
vip(xgb_fit_final) + theme_bw()


library(pdp)
partial(xgb_fit_final, pred.var = "off", train = as.matrix(dplyr::select(model_mlb_data, -war)),
        plot.engine = "ggplot2", plot = TRUE,
        type = "regression") + theme_bw()




