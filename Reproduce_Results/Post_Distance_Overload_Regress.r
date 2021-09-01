## Author: Austin Stephen 
## Date: 8/18/2021
## Purpose: Self contained code to reproduce the results 
#           for paper section: "Post Distance Overload Regress" 
#       
# The reader who is only interested in the analysis and not data manipulation 
# procedures should jump to line 54.

# Libraries (may need to install) -----------------------------------------
library(tidyverse)
library(dplyr)

# Creating the main data frame --------------------------------------------

# Reading in player season summaries and game by game ---------------------

gbg_player_w_distance <- read_csv(
  "./data/in_game_distance_data/in_game_dist_box_join.csv")


# Reading in season box scores --------------------------------------------
season_points <- read.csv(
  "data/box_scores/season_box_scores_player_2010_20.csv") %>%
  filter(season %in% c( 2014, 2015,2016)) %>%
  select("player_id","pts") %>%
  rename("pts_sn" = pts)

# Reading in windows of time over the 2015 season -------------------------

player_Window_dist_daily_2014_15 <- read_csv(
  "./data/in_game_distance_data/player_Window_dist_daily_2014_15.csv")%>%
  select("player_id","date","dist_5dayWindow","dist_3dayWindow","dist_7dayWindow",
         "dist_10dayWindow")


# Reading in windows of time over 2015 speed ------------------------------

player_Window_speed_daily_2014_15 <- read_csv(
  "./data/in_game_distance_data/player_Window_speed_daily_2014_15.csv") %>%
  select("player_id","date","speed_5dayWindow","speed_7dayWindow")


# Date Reformat -----------------------------------------------------------
# Changing date format for join Speed
player_Window_speed_daily_2014_15$date <- as.Date(
  player_Window_speed_daily_2014_15$date, "%m/%d/%Y")

# Changing date format for join Dist
player_Window_dist_daily_2014_15$date <- as.Date(
  player_Window_dist_daily_2014_15$date, "%m/%d/%Y")


# joins -------------------------------------------------------------------

# joining window distance data do game by game data 
gbg_player_w_distance <- merge(x = gbg_player_w_distance,
                               y = player_Window_dist_daily_2014_15,
                               by.x =  c("player_id","date" ),
                               by.y = c("player_id","date" ))

# joining window speed data to running table 
gbg_player_w_distance <- merge(x = gbg_player_w_distance,
                               y = player_Window_speed_daily_2014_15,
                               by.x =  c("player_id","date" ),
                               by.y = c("player_id","date" ))

## joining season points
gbg_player_w_distance <- merge(x = gbg_player_w_distance,
                               y = season_points,
                               by.x =  c("player_id"),
                               by.y = c("player_id"))

# Difference Computations -------------------------------------------------

## Adding a difference from mean columns for regression 
gbg_player_w_distance <- gbg_player_w_distance %>%
  group_by(player_id) %>%
  ## distance 
  ## window size 3
  mutate(sn_avg_wind_3_dist = mean(dist_3dayWindow)) %>%
  mutate(diff_sn_avg_wind_3_dist = dist_3dayWindow - sn_avg_wind_3_dist ) %>%
  ## window size 5
  mutate(sn_avg_wind_5_dist = mean(dist_5dayWindow)) %>%
  mutate(diff_sn_avg_wind_5_dist = dist_5dayWindow - sn_avg_wind_5_dist ) %>%
  ## window size 7
  mutate(sn_avg_wind_7_dist = mean(dist_7dayWindow)) %>%
  mutate(diff_sn_avg_wind_7_dist = dist_7dayWindow - sn_avg_wind_7_dist ) %>%
  ## window size 10
  mutate(sn_avg_wind_10_dist = mean(dist_10dayWindow)) %>%
  mutate(diff_sn_avg_wind_10_dist = dist_10dayWindow - sn_avg_wind_10_dist ) %>%
  ## speed
  ## window size 5
  mutate(sn_avg_wind_5_speed = mean(speed_5dayWindow)) %>%
  mutate(diff_sn_avg_wind_5_speed = dist_10dayWindow - sn_avg_wind_5_speed ) %>%
  ## window size 7
  mutate(sn_avg_wind_7_speed = mean(speed_5dayWindow)) %>%
  mutate(diff_sn_avg_wind_7_speed = dist_10dayWindow - sn_avg_wind_5_speed ) %>%
  ## points
  mutate(diff_from_sn_avg_pts = pts - pts_sn)

# removing any rows with missing data across the two tables and less than 15 min
# must be done after to get accurate computations of cumulative distance
gbg_player_w_distance <- gbg_player_w_distance %>% 
  filter(!is.na(dist_miles), min > 15)




# Analysis ----------------------------------------------------------------


# Window size 3 -----------------------------------------------------------
plot_3 <- gbg_player_w_distance %>%
  ggplot(aes(x = diff_sn_avg_wind_3_dist,y = diff_from_sn_avg_dist))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")+
  labs(title = "3 Days",
       y = "",
       x = ""
       )+
  theme_minimal()+
  theme(title = element_text(size=8, face = "plain"))

summary(lm(diff_from_sn_avg_dist ~ diff_sn_avg_wind_3_dist, 
           data =gbg_player_w_distance ))

# Window size 5 -----------------------------------------------------------

plot_5 <- gbg_player_w_distance %>%
  ggplot(aes(x = diff_sn_avg_wind_5_dist,y = diff_from_sn_avg_dist))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")+
  labs(title = "5 Days",
       y = "",
       x = ""
  )+
  theme_minimal()+
  theme(title = element_text(size=8, face = "plain"))
  

summary(lm(diff_from_sn_avg_dist ~ diff_sn_avg_wind_5_dist, 
           data =gbg_player_w_distance ))

# Window size 7 -----------------------------------------------------------

plot_7 <-gbg_player_w_distance %>%
  ggplot(aes(x = diff_sn_avg_wind_7_dist,y = diff_from_sn_avg_dist))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")+
  labs(title = "7 Days",
       y = "",
       x = ""
  )+
  theme_minimal()+
  theme(title = element_text(size=8, face = "plain"))

summary(lm(diff_from_sn_avg_dist ~ diff_sn_avg_wind_5_dist, 
           data =gbg_player_w_distance ))

# Window size 10 -----------------------------------------------------------

plot_10 <- gbg_player_w_distance %>%
  ggplot(aes(x = diff_sn_avg_wind_7_dist,y = diff_from_sn_avg_dist))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")+
  labs(title = "10 Days",
       y = "",
       x = ""
  )+
  theme_minimal()+
  theme(title = element_text(size=8, face = "plain"))

summary(lm(diff_from_sn_avg_dist ~ diff_sn_avg_wind_5_dist, 
           data =gbg_player_w_distance ))


# Figure for paper --------------------------------------------------------

library(grid)
library(gridExtra)
grid.arrange(plot_3, plot_5, plot_7, plot_10, nrow = 2, ncol = 2,
             top = 
      textGrob("Windows of Prior and Observation Game Distance",
               gp = gpar(fontsize=22)),
             left = 
      textGrob("Observation Game Deviation From Season Mean (mi)",
               gp = gpar(fontsize=16),
               rot = 90),
             bottom = 
        textGrob("Window Deviation From Season Mean (mi)",
                gp = gpar(fontsize=16)))


# Window size 5 speed -----------------------------------------------------

plot_5_speed <- gbg_player_w_distance %>%
  ggplot(aes(x = diff_sn_avg_wind_5_dist ,y = diff_from_sn_avg_speed))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")+
  labs(title = "5 Days",
       y = "",
       x = ""
  )+
  theme_minimal()+
  theme(title = element_text(size=8, face = "plain"))+
  ylim(-.75,1)

summary(lm(diff_from_sn_avg_speed ~ diff_sn_avg_wind_5_dist, 
           data =gbg_player_w_distance ))

# Window size 7 speed -----------------------------------------------------

plot_7_speed <- gbg_player_w_distance %>%
  ggplot(aes(x = diff_sn_avg_wind_5_speed, y = diff_from_sn_avg_speed))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")+
  labs(title = "7  Days",
       y = "",
       x = ""
  )+
  theme_minimal()+
  theme(title = element_text(size=8, face = "plain"))+
  ylim(-.75,1)

summary(lm(diff_from_sn_avg_speed ~ diff_sn_avg_wind_7_dist, 
           data =gbg_player_w_distance ))



# Figure for paper --------------------------------------------------------

library(grid)
library(gridExtra)
grid.arrange(plot_5_speed, plot_7_speed, nrow = 1, ncol = 2,
             top = 
               textGrob("Windows of Prior Distance and Observation Speed",
                        gp = gpar(fontsize=22)),
             left = 
               textGrob("Observation Deviation From Season Mean (mi/min)",
                        gp = gpar(fontsize=14),
                        rot = 90),
             bottom = 
               textGrob("Window Deviation From Season Mean (mi)",
                        gp = gpar(fontsize=16)))


# Performance and workload -------------------------------------------------

summary(lm(diff_from_sn_avg_fgpct ~ diff_sn_avg_wind_3_dist, data =
             gbg_player_w_distance))
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              0.005392   0.001355   3.979 6.96e-05 ***
#   diff_sn_avg_wind_3_dist -0.002783   0.001057  -2.634  0.00845 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1705 on 15932 degrees of freedom
# (29 observations deleted due to missingness)
# Multiple R-squared:  0.0004353,	Adjusted R-squared:  0.0003726 
# F-statistic: 6.938 on 1 and 15932 DF,  p-value: 0.008446


summary(lm(diff_from_sn_avg_fgpct ~ diff_sn_avg_wind_5_dist, data =
             gbg_player_w_distance))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              0.0053811  0.0013583   3.962 7.48e-05 ***
#   diff_sn_avg_wind_5_dist -0.0015684  0.0007878  -1.991   0.0465 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1705 on 15932 degrees of freedom
# (29 observations deleted due to missingness)
# Multiple R-squared:  0.0002487,	Adjusted R-squared:  0.000186 
# F-statistic: 3.963 on 1 and 15932 DF,  p-value: 0.04652


summary(lm(diff_from_sn_avg_fgpct ~ diff_sn_avg_wind_7_dist, data =
             gbg_player_w_distance))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              0.0052895  0.0013594   3.891   0.0001 ***
#   diff_sn_avg_wind_7_dist -0.0007829  0.0006182  -1.266   0.2054    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1705 on 15932 degrees of freedom
# (29 observations deleted due to missingness)
# Multiple R-squared:  0.0001006,	Adjusted R-squared:  3.788e-05 
# F-statistic: 1.604 on 1 and 15932 DF,  p-value: 0.2054


summary(lm(diff_from_sn_avg_fgpct ~ diff_sn_avg_wind_10_dist, data =
          gbg_player_w_distance))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               0.0052168  0.0013589   3.839 0.000124 ***
#   diff_sn_avg_wind_10_dist -0.0003744  0.0004606  -0.813 0.416284    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1705 on 15932 degrees of freedom
# (29 observations deleted due to missingness)
# Multiple R-squared:  4.148e-05,	Adjusted R-squared:  -2.129e-05 
# F-statistic: 0.6608 on 1 and 15932 DF,  p-value: 0.4163


# Table for paper FG --------------------------------------------------------

metric <- c("3","5","7","10")

coeff <- c(-0.0028, -0.0015, -0.0008,-0.0004 )

p_val <- c(0.00845,0.0465,0.2054,0.416284)

r_squared <- c(-2.129e-05, 3.788e-05, 0.000186, 0.0003726)

table<- data.frame(metric, coeff, p_val, r_squared)

table <- table %>% rename("p-value" = p_val,
                          "adjusted R^2" = r_squared,
                          "Window size (days)"= metric)
library(gt)
save <- table %>%
  gt() %>%
  tab_header(
    title = md("**FG % and Window Distance**")
  ) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% 
  cols_align(align = "left",
             columns = c(2,3,4)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "grey")
    ),
    locations = cells_body(
      rows = c(2,4))
  )

library(webshot)
gtsave(save,"FG_window.png",path= "./Paper/")


summary(lm(diff_from_sn_avg_pts ~ diff_sn_avg_wind_3_dist, data =
             gbg_player_w_distance))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1.10086    0.02734  40.273   <2e-16 ***
#   diff_sn_avg_wind_3_dist  0.19395    0.02193   8.843   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.4 on 55018 degrees of freedom
# (93 observations deleted due to missingness)
# Multiple R-squared:  0.001419,	Adjusted R-squared:  0.001401 
# F-statistic:  78.2 on 1 and 55018 DF,  p-value: < 2.2e-16


summary(lm(diff_from_sn_avg_pts ~ diff_sn_avg_wind_5_dist, data =
             gbg_player_w_distance))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1.09330    0.02735   39.98   <2e-16 ***
#   diff_sn_avg_wind_5_dist  0.17875    0.01627   10.99   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.398 on 55018 degrees of freedom
# (93 observations deleted due to missingness)
# Multiple R-squared:  0.00219,	Adjusted R-squared:  0.002172 
# F-statistic: 120.8 on 1 and 55018 DF,  p-value: < 2.2e-16

summary(lm(diff_from_sn_avg_pts ~ diff_sn_avg_wind_7_dist, data =
             gbg_player_w_distance))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1.08570    0.02733   39.72   <2e-16 ***
#   diff_sn_avg_wind_7_dist  0.18298    0.01271   14.39   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.393 on 55018 degrees of freedom
# (93 observations deleted due to missingness)
# Multiple R-squared:  0.00375,	Adjusted R-squared:  0.003732 
# F-statistic: 207.1 on 1 and 55018 DF,  p-value: < 2.2e-16

summary(lm(diff_from_sn_avg_pts ~ diff_sn_avg_wind_10_dist, data =
             gbg_player_w_distance))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1.084898   0.027321   39.71   <2e-16 ***
#   diff_sn_avg_wind_10_dist 0.144295   0.009464   15.25   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.392 on 55018 degrees of freedom
# (93 observations deleted due to missingness)
# Multiple R-squared:  0.004207,	Adjusted R-squared:  0.004189 
# F-statistic: 232.5 on 1 and 55018 DF,  p-value: < 2.2e-16

# Table for paper pts --------------------------------------------------------

metric <- c("3","5","7","10")

coeff <- c(0.193, 0.179, 0.182, 0.144)

p_val <- c(2e-16, 2e-16, 2e-16, 2e-16)

r_squared <- c(0.001401, 0.002172, 0.003732,0.004189)

table<- data.frame(metric, coeff, p_val, r_squared)

table <- table %>% rename("p-value" = p_val,
                          "adjusted R^2" = r_squared,
                          "Window size (days)"= metric)
library(gt)
save <- table %>%
  gt() %>%
  tab_header(
    title = md("**Points and Window Distance**")
  ) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% 
  cols_align(align = "left",
             columns = c(2,4)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "grey")
    ),
    locations = cells_body(
      rows = c(2,4))
  )

library(webshot)
gtsave(save,"points_window.png",path= "./Paper/")
