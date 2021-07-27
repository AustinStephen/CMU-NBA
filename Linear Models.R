# Strength Proxy Linear Model ---------------------------------------------
mutate(hours_shift = as.factor(shift))

together$hours_shift <- relevel(together$hours_shift, ref = "0")


strength <- lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + g11_15_centered_rolling_net_diff +
                 g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff +
                  games_played* net_rating_diff,
               data = together)

summary(strength)



# Strength and Travel Linear Model ------------------------------------------------------------
travel_lm <- lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + g11_15_centered_rolling_net_diff +
                  g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff +
                  net_rating_diff + rest_diff + b2b_2nd_vis + three_in_four_vis + travel_3_hours_back_vis, 
                data = together)
summary(travel_lm)

subset_together <- select(together, c(g1_5_rolling_net_diff, net_rating_diff, rest_diff ,travel_3_hours_back_vis, game_net_rating))

ggpairs(data = subset_together)



# Data for error bar plots ------------------------------------------------
summ <- summary(travel_lm)
coefficients <- c("intercept", "Game 1-5", "Game 6-10", 
                  "Game 11-15", "Game 16-20", 
                  "Game 21-25", "Game 26-30", "net_rating_diff",
                  "rest_diff", "b2b_2nd_vis", "three_in_four_vis","travel_3_hours_back_vis")

coefficients <- ordered(coefficients, levels = c("Game 1-5", "Game 6-10", 
                                                 "Game 11-15", "Game 16-20", 
                                                 "Game 21-25", "Game 26-30",
                                                 "intercept","net_rating_diff", 
                                                 "rest_diff","travel_3_hours_back_vis","b2b_2nd_vis", "three_in_four_vis"))
betas <- as.numeric(summ$coefficients[,1])
standard_errors <- as.numeric(summ$coefficients[,2])
p_values <- as.numeric(summ$coefficients[,4])
p <- sprintf("p= %s", p_values)

table <- data.frame(coefficients, betas, standard_errors, p)
centered_ratings_table <- table[c(2:7),]
other_variables_table <- table[c(1, 8:12),]

library(ggrepel)
ggplot(data = centered_ratings_table, aes(x = coefficients, y = betas)) +
  geom_point(color = "brown", size = 2) +
  geom_text_repel(aes(label=p), size = 2.5) + 
  geom_errorbar(aes(ymin = betas - 2*standard_errors, ymax = betas + 2*standard_errors), color = "brown",width = 0.2) +
  xlab("Window Span") +
  ylab("Coefficient Value") +
  labs(title = "Coefficients of the Centered Rolling Net Rating Variables") +
  theme(panel.background = element_rect(fill = "burlywood"),
        panel.grid.major=element_blank()
  )


ggplot(data = other_variables_table, aes(x = coefficients, y = betas)) +
  geom_point(color = "brown", size = 2) +
  geom_text_repel(aes(label=p), size = 2.5) + 
  geom_errorbar(aes(ymin = betas - 2*standard_errors, ymax = betas + 2*standard_errors), color = "brown", width = 0.2) +
  xlab("Regression Variable") +
  ylab("Coefficient Value") +
  labs(title = "Coefficients of the Density and Schedule Metrics") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(panel.background = element_rect(fill = "burlywood"),
        panel.grid.major=element_blank()
        )
  


# pre vs post 15 ----------------------------------------------------------


pre15 <- filter(together, season %in% c("2010-11","2011-12","2012-13","2013-14","2014-15"))
post15 <- filter(together, season %in% c("2015-16","2016-17","2017-18","2018-19"))

pre15_lm <- lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + g11_15_centered_rolling_net_diff +
                 g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff +
                 net_rating_diff + rest_diff + b2b_2nd_vis + three_in_four_vis + travel_3_hours_back_vis, 
               data = pre15)

summary(pre15_lm)

post15_lm <- lm(game_net_rating ~ g1_5_centered_rolling_net_diff + g6_10_centered_rolling_net_diff + g11_15_centered_rolling_net_diff +
                  g16_20_centered_rolling_net_diff + g21_25_centered_rolling_net_diff + g26_30_centered_rolling_net_diff +
                  net_rating_diff + rest_diff + b2b_2nd_vis + three_in_four_vis + travel_3_hours_back_vis, 
                data = post15)

summary(post15_lm)




# Linear Model with Interaction Terms -------------------------------------
travel_lm2 <- lm(game_net_rating ~ net_rating_5gameWindow_diff + net_rating_diff
                 + rest_diff + b2b_2nd_vis + three_in_four_vis   
                 + distance_vis + distance_home, 
                 data = together)
summary(travel_lm2)

# Mixed random effects model that adds intercept terms for each team and season -------------------------
library(lme4)

together2 <- together %>%
  mutate(visitor_year = paste0(visitor, "-", season)) %>%
  mutate(home_year = paste0(home, "-", season)) 

travel_lmer <- lmer(game_net_rating ~ g1_5_rolling_net_diff + g6_10_rolling_net_diff + g11_15_rolling_net_diff +
                      g16_20_rolling_net_diff + g21_25_rolling_net_diff + g26_30_rolling_net_diff +
                      g31_35_rolling_net_diff + g36_40_rolling_net_diff + games_played* net_rating_diff  
                    + rest_diff + b2b_2nd_vis + three_in_four_vis + travel_3_hours_back_vis 
                    + (1|visitor_year) + (1|home_year), 
                    data = together2)

summary(travel_lmer)

VarCorr(travel_lmer) %>% as_tibble() %>% mutate(icc = vcov / sum(vcov)) %>% dplyr::select(grp, icc)

library(merTools)
team_season_effects <- REsim(travel_lmer)
plotREsim(team_season_effects)


team_season_effects %>%
  as_tibble() %>%
  group_by(groupFctr) %>%
  arrange(desc(mean)) %>%
  slice(1:5, (n() - 4):n()) %>%
  ggplot(aes(x = reorder(groupID, mean))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd,
                    ymax = mean + 2 * sd)) +
  facet_wrap(~groupFctr, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  coord_flip() +
  theme_bw()

library(vip)
vip(travel_visitors_lm, geom = "point")

cor(visitors$score_diff, visitors$win_percent_diff)

visitors %>%
  mutate(init_preds = predict(travel_visitors_lm)) %>%
  ggplot(aes(x = init_preds, y = score_diff)) +
  geom_point(alpha = 0.75) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  theme_bw() +
  labs(x = "Predictions", y = "Observed Score Diff")

  


