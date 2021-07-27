together_net_Rating_allWindows <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/austin_everything.csv")

together_net_Rating_allWindows <- together_net_Rating_allWindows %>%
  select(-c(fgm:opp_blk)) %>%
  select(-c(fgm_diff:blk_diff))

together_net_Rating_allWindows <- together_net_Rating_allWindows %>%
  mutate(distance_diff = Distance - opp_distance) %>%
  mutate(windowed_distance_diff = dist_30dayWindow - opp_dist_30dayWindow) %>%
  mutate(distance_advantage = (distance_diff < 0)) %>%
  mutate(windowed_distance_advantage = (windowed_distance_diff) < 0)

#Using just net_rating_diff
summary(lm(game_net_rating ~ net_rating_diff*games_played + three_in_four 
           + travel_3_hours_back + rest_diff,
           data = together_net_Rating_allWindows))


# Using centered hollowed net ratings -------------------------------------
summary(lm(game_net_rating ~ wind_10_center + hollow_wind20_cent + hollow_wind30_cent+
             hollow_wind40_cent + hollow_wind50_cent + hollow_wind60_cent +
              + three_in_four + travel_3_hours_back + rest_diff,
           data = together_net_Rating_allWindows))


# Using hollowed net ratings ----------------------------------------------
summary(lm(game_net_rating ~ wind_5 + wind_10_hollow_5 + wind_15_hollow + wind_20_hollow +
     wind_25_hollow + wind_30_hollow + wind_35_hollow + wind_40_hollow
      + three_in_four + travel_3_hours_back + rest_diff,
   data = together_net_Rating_allWindows))


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





