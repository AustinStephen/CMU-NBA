
# Bootstrap Model ---------------------------------------------------------
coefficients <- c("intercept", "g1_5_centered_rolling_net_diff", "g6_10_centered_rolling_net_diff", 
                  "g11_15_centered_rolling_net_diff", "g16_20_centered_rolling_net_diff", 
                  "g21_25_centered_rolling_net_diff", "g26_30_centered_rolling_net_diff", "net_rating_diff",
                  "rest_diff", "b2b_2nd_vis", "three_in_four_vis","travel_3_hours_back_vis" )

coefs_intercepts = c()
coefs_g1_5_centered_rolling_net_diff = c()
coefs_g6_10_centered_rolling_net_diff = c()
coefs_g11_15_centered_rolling_net_diff = c()
coefs_g16_20_centered_rolling_net_diff = c()
coefs_g21_25_centered_rolling_net_diff = c()
coefs_g26_30_centered_rolling_net_diff = c()
coefs_net_rating_diff = c()
coefs_rest_diff = c()
coefs_b2b_2nd_vis = c()
coefs_three_in_four_vis = c()
coefs_travel_3_hours_back_vis = c()

iterations <- 100
for (i in c(1:iterations)) {
  set.seed(i)
  temp_rows <- sample(nrow(together), replace = TRUE)
  temp_data <- together[temp_rows, ]
  temp_lm <- lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + g11_15_centered_rolling_net_diff +
                  g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff +
                  net_rating_diff + rest_diff + b2b_2nd_vis + three_in_four_vis + travel_3_hours_back_vis, 
                data = temp_data)
  temp_summary <- summary(temp_lm)
  temp_coefs = as.numeric(temp_summary$coefficients[,1])
  coef_intercepts = c(coefs_intercepts, temp_coefs[1])
  coefs_g1_5_centered_rolling_net_diff = c(coefs_g1_5_centered_rolling_net_diff, temp_coefs[2])
  coefs_g6_10_centered_rolling_net_diff = c(coefs_g6_10_centered_rolling_net_diff, temp_coefs[3])
  coefs_g11_15_centered_rolling_net_diff = c(coefs_g11_15_centered_rolling_net_diff, temp_coefs[4])
  coefs_g16_20_centered_rolling_net_diff = c(coefs_g16_20_centered_rolling_net_diff, temp_coefs[5])
  coefs_g21_25_centered_rolling_net_diff = c(coefs_g21_25_centered_rolling_net_diff, temp_coefs[6])
  coefs_g26_30_centered_rolling_net_diff = c(coefs_g26_30_centered_rolling_net_diff, temp_coefs[7])
  coefs_net_rating_diff = c(coefs_net_rating_diff, temp_coefs[8])
  coefs_rest_diff = c(coefs_rest_diff, temp_coefs[9])
  coefs_b2b_2nd_vis = c(coefs_b2b_2nd_vis, temp_coefs[10])
  coefs_three_in_four_vis = c(coefs_three_in_four_vis, temp_coefs[11])
  coefs_travel_3_hours_back_vis = c(coefs_travel_3_hours_back_vis, temp_coefs[12])
}

#Average bootstrapped coefficients
mean_intercept <- mean(coef_intercepts)
mean_g1_5_centered_rolling_net_diff<- mean(coefs_g1_5_centered_rolling_net_diff)
mean_g6_10_centered_rolling_net_diff<- mean(coefs_g6_10_centered_rolling_net_diff)
mean_g11_15_centered_rolling_net_diff<- mean(coefs_g11_15_centered_rolling_net_diff)
mean_g16_20_centered_rolling_net_diff<- mean(coefs_g16_20_centered_rolling_net_diff)
mean_g21_25_centered_rolling_net_diff<- mean(coefs_g21_25_centered_rolling_net_diff)
mean_g26_30_centered_rolling_net_diff<- mean(coefs_g26_30_centered_rolling_net_diff)
mean_net_rating_diff <- mean(coefs_net_rating_diff)
mean_rest_diff <- mean(coefs_rest_diff)
mean_b2b_2nd <- mean(coefs_b2b_2nd_vis)
mean_three_in_four <- mean(coefs_three_in_four_vis)
mean_travel_3_hours_back <- mean(coefs_travel_3_hours_back_vis)


bootstrap_coefficients <- c(mean_intercept, mean_g1_5_centered_rolling_net_diff,mean_g6_10_centered_rolling_net_diff,
                            mean_g11_15_centered_rolling_net_diff, mean_g16_20_centered_rolling_net_diff, 
                            mean_g21_25_centered_rolling_net_diff, mean_g26_30_centered_rolling_net_diff, 
                            mean_net_rating_diff,mean_rest_diff, mean_b2b_2nd, mean_three_in_four, 
                            mean_travel_3_hours_back)

#Calculating standard errors of list of coefficients
se_intercept <- sd(coef_intercepts)/sqrt(iterations)
se_g1_5_centered_rolling_net_diff <- sd(coefs_g1_5_centered_rolling_net_diff)/sqrt(iterations)
se_g6_10_centered_rolling_net_diff <- sd(coefs_g6_10_centered_rolling_net_diff)/sqrt(iterations)
se_g11_15_centered_rolling_net_diff <- sd(coefs_g11_15_centered_rolling_net_diff)/sqrt(iterations)
se_g16_20_centered_rolling_net_diff <- sd(coefs_g16_20_centered_rolling_net_diff)/sqrt(iterations)
se_g21_25_centered_rolling_net_diff <- sd(coefs_g21_25_centered_rolling_net_diff)/sqrt(iterations)
se_g26_30_centered_rolling_net_diff <- sd(coefs_g26_30_centered_rolling_net_diff)/sqrt(iterations)
se_net_rating_diff <- sd(coefs_net_rating_diff)/sqrt(iterations)
se_rest_diff <- sd(coefs_rest_diff)/sqrt(iterations)
se_b2b_2nd <- sd(coefs_b2b_2nd_vis)/sqrt(iterations)
se_three_in_four <- sd(coefs_three_in_four_vis)/sqrt(iterations)
se_travel_3_hours_back <- sd(coefs_travel_3_hours_back_vis)/sqrt(iterations)

bootstrap_standard_errors <- c(se_intercept, se_g1_5_centered_rolling_net_diff, se_g6_10_centered_rolling_net_diff,
                               se_g11_15_centered_rolling_net_diff, se_g16_20_centered_rolling_net_diff, se_g21_25_centered_rolling_net_diff,
                               se_g26_30_centered_rolling_net_diff, se_net_rating_diff, se_rest_diff, se_b2b_2nd, se_three_in_four, se_travel_3_hours_back )

#Calculating t statistics
t_intercept <- mean_intercept/se_intercept
t_g1_5_centered_rolling_net_diff <- mean_g1_5_centered_rolling_net_diff/se_g1_5_centered_rolling_net_diff
t_g6_10_centered_rolling_net_diff <- mean_g6_10_centered_rolling_net_diff/se_g6_10_centered_rolling_net_diff
t_g11_15_centered_rolling_net_diff <- mean_g11_15_centered_rolling_net_diff/se_g11_15_centered_rolling_net_diff
t_g16_20_centered_rolling_net_diff <- mean_g16_20_centered_rolling_net_diff/se_g16_20_centered_rolling_net_diff
t_g21_25_centered_rolling_net_diff <- mean_g21_25_centered_rolling_net_diff/se_g21_25_centered_rolling_net_diff
t_g26_30_centered_rolling_net_diff <- mean_g26_30_centered_rolling_net_diff/se_g26_30_centered_rolling_net_diff
t_net_rating_diff <- mean_net_rating_diff/se_net_rating_diff
t_rest_diff <- mean_rest_diff/se_rest_diff
t_b2b_2nd <- mean_b2b_2nd/se_b2b_2nd
t_three_in_four <- mean_three_in_four/se_three_in_four
t_travel_3_hours_back <- mean_travel_3_hours_back/ se_travel_3_hours_back


bootstrap_t_statistics <- c(t_intercept,t_g1_5_centered_rolling_net_diff,t_g6_10_centered_rolling_net_diff,t_g11_15_centered_rolling_net_diff,
                            t_g16_20_centered_rolling_net_diff, t_g21_25_centered_rolling_net_diff, t_g26_30_centered_rolling_net_diff,
                            t_net_rating_diff, t_rest_diff, t_b2b_2nd,t_three_in_four, t_travel_3_hours_back)



bootstrap_model_summary <- data.frame(coefficients, bootstrap_coefficients, bootstrap_standard_errors, bootstrap_t_statistics)


ggplot(data = bootstrap_model_summary) +
  geom_point(aes(x = coefficients, y = bootstrap_coefficients))




