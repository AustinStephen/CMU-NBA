
# Seeing how travel and density metrics affect visitors performance across multiple seasons ---------------------
together_net_Rating_allWindows$dist_30_one_fifth <- (together_net_Rating_allWindows$dist_30dayWindow)^0.2



visitors1011 <- filter(together_net_Rating_allWindows, season == "2010-11")
visitors1112 <- filter(together_net_Rating_allWindows, season == "2011-12")
visitors1213 <- filter(together_net_Rating_allWindows, season == "2012-13")
visitors1314 <- filter(together_net_Rating_allWindows, season == "2013-14")
visitors1415 <- filter(together_net_Rating_allWindows, season == "2014-15")
visitors1516 <- filter(together_net_Rating_allWindows, season == "2015-16")
visitors1617 <- filter(together_net_Rating_allWindows, season == "2016-17")
visitors1718 <- filter(together_net_Rating_allWindows, season == "2017-18")
visitors1819 <- filter(together_net_Rating_allWindows, season == "2018-19")
visitors1819 <- visitors1819[-c(832),]
visitors1112 <- visitors1112[-c(344,422),]

#filtering out blowout games



lm1011 <- lm(game_net_rating  ~ net_rating_diff +  dist_30_one_fifth, 
                             data = visitors1011)
lm1112 <- lm(game_net_rating  ~ net_rating_diff+  dist_30_one_fifth,
             data = visitors1112)
lm1213 <- lm(game_net_rating ~ net_rating_diff  + dist_30_one_fifth, 
             data = visitors1213)
lm1314 <- lm(game_net_rating  ~ net_rating_diff + dist_30_one_fifth, 
                            data = visitors1314)
lm1415 <- lm(game_net_rating ~ net_rating_diff + dist_30_one_fifth, 
             data = visitors1415)
lm1516 <- lm(game_net_rating ~ net_rating_diff + dist_30_one_fifth, 
             data = visitors1516)
lm1617 <- lm(game_net_rating ~ net_rating_diff + dist_30_one_fifth, 
             data = visitors1617)
lm1718 <- lm(game_net_rating ~ net_rating_diff + dist_30_one_fifth, 
             data = visitors1718)
lm1819 <- lm(game_net_rating  ~ net_rating_diff + dist_30_one_fifth,
             data = visitors1819)

coeffs1011 <- as.numeric(summary(lm1011)$coefficients[,1])
standard_errors1011 <- as.numeric(summary(lm1011)$coefficients[,2])
cfse1011 <- c(coeffs1011, standard_errors1011)


coeffs1112 <- as.numeric(summary(lm1112)$coefficients[,1])
standard_errors1112 <- as.numeric(summary(lm1112)$coefficients[,2])
cfse1112 <- c(coeffs1112, standard_errors1112)

coeffs1213 <- as.numeric(summary(lm1213)$coefficients[,1])
standard_errors1213 <- as.numeric(summary(lm1213)$coefficients[,2])
cfse1213 <- c(coeffs1213, standard_errors1213)

coeffs1314 <- as.numeric(summary(lm1314)$coefficients[,1])
standard_errors1314 <- as.numeric(summary(lm1314)$coefficients[,2])
cfse1314 <- c(coeffs1314 , standard_errors1314 )

coeffs1415 <- as.numeric(summary(lm1415)$coefficients[,1])
standard_errors1415 <- as.numeric(summary(lm1415)$coefficients[,2])
cfse1415 <- c(coeffs1415, standard_errors1415)

coeffs1516 <- as.numeric(summary(lm1516)$coefficients[,1])
standard_errors1516 <- as.numeric(summary(lm1516)$coefficients[,2])
cfse1516 <- c(coeffs1516, standard_errors1516)

coeffs1617 <- as.numeric(summary(lm1617)$coefficients[,1])
standard_errors1617 <- as.numeric(summary(lm1617)$coefficients[,2])
cfse1617 <- c(coeffs1617, standard_errors1617)

coeffs1718 <- as.numeric(summary(lm1718)$coefficients[,1])
standard_errors1718 <- as.numeric(summary(lm1718)$coefficients[,2])
cfse1718 <- c(coeffs1718, standard_errors1718)

coeffs1819 <- as.numeric(summary(lm1819)$coefficients[,1])
standard_errors1819 <- as.numeric(summary(lm1819)$coefficients[,2])
cfse1819 <- c(coeffs1819, standard_errors1819)

coeffs_and_standard_errors <- rbind(cfse1011, cfse1112)
coeffs_and_standard_errors <- rbind(coeffs_and_standard_errors, cfse1213)
coeffs_and_standard_errors <- rbind(coeffs_and_standard_errors, cfse1314)
coeffs_and_standard_errors <- rbind(coeffs_and_standard_errors, cfse1415)
coeffs_and_standard_errors <- rbind(coeffs_and_standard_errors, cfse1516)
coeffs_and_standard_errors <- rbind(coeffs_and_standard_errors, cfse1617)
coeffs_and_standard_errors <- rbind(coeffs_and_standard_errors, cfse1718)
coeffs_and_standard_errors <- rbind(coeffs_and_standard_errors, cfse1819)

coeffs_and_standard_errors <- as.data.frame(coeffs_and_standard_errors)


# coeffs_and_standard_errors <- coeffs_and_standard_errors %>%
#  rename(intercept = V1,
#          net_rating_diff = V2,
#          three_in_four_vis = V3,
#          b2b_2nd_vis = V4,
#          windowed_distance_diff = V5,
#          travel_3_hours_back_vis = V6,
#          rest_diff= V7,
#          dist_30 = V8,
#          intercept_se = V9,
#          net_rating_diff_se = V10,
#          three_in_four_vis_se = V11,
#          b2b_2nd_vis_se = V12,
#          windowed_distance_diff_se = V13,
#          travel_3_hours_back_vis_se = V14,
#          rest_diff_se= V15,
#          dist_30_se = V16
#          ) 

coeffs_and_standard_errors <- coeffs_and_standard_errors %>%
  rename(intercept = V1,
         net_rating_diff = V2,
         dist_30 = V3,
         intercept_se = V4,
         net_rating_diff_se = V5,
         dist_30_se = V6
  ) 

coeffs_and_standard_errors <- mutate(coeffs_and_standard_errors, season = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

#Need to improve this with windowed distance and data with no duplicates
dy = coeffs_and_standard_errors$dist_30_se
var = coeffs_and_standard_errors$dist_30
ggplot(data = coeffs_and_standard_errors, aes(x = season, y = var)) + 
  geom_line(color = "blue") + 
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = var - 2*dy, ymax = var + 2*dy), width = 0.2) +
  ylab("Game Net Rating")







