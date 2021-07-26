
# Logistic Regression -----------------------------------------------------
together_logit <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/together.csv") %>%
  mutate(hours_shift = as.factor(shift_vis))

together_logit$hours_shift <-relevel(together_logit$hours_shift, ref= "0")

table(together_logit$Win)

init_logit <- glm(Win ~  
                  + rest_diff + b2b_2nd_vis + three_in_four_vis  + travel_3_hours_back_vis, 
                  data = together_logit,
                  family = "binomial")

summary(init_logit)


#coefficents are log odds 
#e to the whatever coefficient tells us the factor by which the odds change
#1 percent increase in win_percent_diff increase odds of winning by times 1.05

#positive coefficient -> multiply by more than than 1 (increase odds)
#negative coefficient -> multiply by less than than 1 (decrease)

#low deviance is good

pred_game_outcome <-
  ifelse(init_logit$fitted.values >= 0.5, "Win", "Loss")

table("Predictions" = pred_game_outcome, 
      "Observed" = together_logit$Win)


#in sample misclassification rate
mean(ifelse(init_logit$fitted.values >= 0.5, 1, 0) != together_logit$Win)
#0.31

#Brier Score
mean((together_logit$Win - fitted(init_logit))^2)
#0.20

#Calibration
together_logit %>%
  mutate(pred_prob = init_logit$fitted.values,
         bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  # Group by bin_pred_prob:
  group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_attempts = n(),
            bin_actual_prob = mean(Win)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point(aes(size = n_attempts)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, 
              color = "red", linetype = "dashed") +
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of games",
       x = "Estimated Win probability",
       y = "Observed Win probability") + 
  theme_bw() +
  theme(legend.position = "bottom")
