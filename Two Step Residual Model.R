two_step_together <- together

strength_lm <- lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + g11_15_centered_rolling_net_diff +
                    g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff +
                    games_played* net_rating_diff,
                  data = two_step_together)

summary(strength_lm)

sum(together$travel_3_hours_back_vis)


# Assess Model Predictions ------------------------------------------------
two_step_together %>%
  mutate(pred_vals = predict(strength_lm)) %>%
  ggplot(aes(x = pred_vals, y = game_net_rating)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "red",
              size = 2) +
  theme_bw()

library(broom)
two_step_together <-
  broom::augment(strength_lm, two_step_together)

two_step_together %>%
  ggplot(aes(x = .fitted, y = game_net_rating)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "red",
              size = 2) +
  theme_bw()

# Model Diagnostics -------------------------------------------------------
#Residuals are centered around 0!!!!!!!
two_step_together %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", color = "red",
             size = 2) +
  geom_smooth(se = FALSE) +
  theme_bw()

#Next step is to regress these residuals on our travel and density metrics
strength_residuals <- resid(strength_lm)
second_lm <- lm(strength_residuals ~ rest_diff + b2b_2nd_vis + three_in_four_vis + travel_3_hours_back_vis,
                data = two_step_together)

summary(second_lm)

#the coefficients in second_lm are very similar to the coefficients in check_lm


