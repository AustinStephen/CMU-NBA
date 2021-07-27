two_step_together = together_net_Rating_allWindows

summary(strength_lm <- lm(game_net_rating ~ wind_10_center + hollow_wind20_cent + hollow_wind30_cent+
                    hollow_wind40_cent + hollow_wind50_cent + hollow_wind60_cent
                    ,
                  data = two_step_together))


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
summary(second_lm <- lm(strength_residuals ~ rest_diff  + three_in_four + travel_3_hours_back,
                data = two_step_together))


#the coefficients in second_lm are very similar to the coefficients in check_lm


