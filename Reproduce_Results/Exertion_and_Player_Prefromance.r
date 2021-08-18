## Author: Austin Stephen 
## Date: 8/11/2021
## Purpose: Self contained code to reproduce the results 
#           for paper section: "Exertion and Player Performance" 


## libraries
library(nbastatR)
library(tidyverse)
library(readr)


# Reading in the tracking data --------------------------------------------

sn_16_17   <- read_csv(
  "data/in_game_distance_data/tracking_by_player_season_daily_2016_17.csv")

sn_15_16   <- read_csv(
  "data/in_game_distance_data/tracking_by_player_season_daily_2015_16.csv")

sn_14_15   <- read_csv(
  "data/in_game_distance_data/tracking_by_player_season_daily_2014_15.csv")


# Reading in the box score data -------------------------------------------

bx_16_17   <- read_csv(
  "data/box_scores/box_scores_game_by_game_player_2016_17.csv")

bx_15_16   <- read_csv(
  "data/box_scores/box_scores_game_by_game_player_2015_16.csv")

bx_14_15   <- read_csv(
  "data/box_scores/box_scores_game_by_game_player_2014_15.csv")

season_box_scores_player_2010_20 <- read.csv(
  "data/box_scores/season_box_scores_player_2010_20.csv") %>%
  filter(season %in% c( 2014, 2015,2016)) %>%
  mutate(season = case_when(
        season == 2014 ~ "2014_15",
        season == 2015 ~ "2015_16",
        season == 2016 ~ "2016_17")) %>%
  select("player_id","season", "fgm", "fga", "fg_pct", "fg3_pct","ft_pct", 
         "oreb", "dreb", "ast","tov","stl","blk", "pts") %>%
  rename("fgm_sn" = fgm,
         "fga_sn" = fga,
         "fg_pct_sn" = fg_pct,
         "fg3_pct_sn" = fg3_pct,
         "ft_pct_sn" = ft_pct,
         "oreb_sn" = oreb,
         "dreb_sn" = dreb,
         "ast_sn" = ast,
         "tov_sn" = tov,
         "stl_sn" = stl,
         "blk_sn" = blk,
         "pts_sn" = pts
         )

# Joining the tracking data to the box score data -------------------------

## Getting the seasons game ids
schedules_2014 <- read.csv("data/schedules/season_2014_2015.csv")
schedules_2015 <- read.csv("data/schedules/season_2015_2016.csv")
schedules_2016 <- read.csv("data/schedules/season_2016_2017.csv")

schedules <- rbind(schedules_2014, schedules_2015, schedules_2016)

## de-clutter the global env
rm(schedules_2014, schedules_2015,schedules_2016)

## dropping unneeded schedule rows 
schedules <- schedules %>% select(c(idGame, dateGame))

## connecting all tracking seasons into 1 data frame
sn_14_15 <- sn_14_15 %>% mutate(season = "2014_15")
sn_15_16 <- sn_15_16 %>% mutate(season = "2015_16")
sn_16_17 <- sn_16_17 %>% mutate(season = "2016_17")

tracking <- rbind(sn_14_15, sn_15_16, sn_16_17)

## connecting all box scores into 1 data frame
bx_14_15 <- bx_14_15 %>% mutate(season = "2014_15")
bx_15_16 <- bx_15_16 %>% mutate(season = "2015_16")
bx_16_17 <- bx_16_17 %>% mutate(season = "2016_17")

box_scores <- rbind(bx_14_15, bx_15_16, bx_16_17)

## Changing date format for tracking 
tracking$date <- as.Date(tracking$date, "%m/%d/%Y")

# joining box scores with schedules on game id for date
box_scores <- merge( schedules, box_scores, by= "idGame"
)

# joining with distance on game id
gbg_player_w_distance <- merge( tracking, box_scores,
  by.x= c("player_id","date","season"),
  by.y= c("idPlayer","dateGame", "season")
) %>%
  select("player_id","idGame","team_id","player_name", "groupStartPosition",
         "season",w:plusminus, -c("namePlayer", "gp", "slugTeam", "idTeam", "min1"))

## joining season averages 
gbg_player_w_distance <- merge( gbg_player_w_distance,
                                season_box_scores_player_2010_20,
                                by.x = c("player_id", "season"),
                                by.y = c("player_id", "season"))

## cleanning the global env 
rm(bx_14_15, bx_15_16, bx_16_17, sn_14_15, sn_15_16, sn_16_17, schedules,
   tracking, season_box_scores_player_2010_20, box_scores)


# Adding difference from season avg cols ----------------------------------

gbg_player_w_distance <- gbg_player_w_distance %>%
  group_by(player_id) %>% mutate(
    ftpct = ifelse(fta > 0,(ftm / fta), NA),
    avg_dist_miles_sn = mean(dist_miles),
    avg_dist_miles_off_sn = mean(dist_miles_off),
    avg_dist_miles_def_sn = mean(dist_miles_def),
    avg_speed_sn = mean(avg_speed),
    avg_speed_off_sn = mean(avg_speed_off),
    avg_speed_def_sn = mean(avg_speed_def),
    diff_from_sn_avg_dist = dist_miles - avg_dist_miles_sn,
    diff_from_sn_avg_off = dist_miles_off - avg_dist_miles_off_sn,
    diff_from_sn_avg_def = dist_miles_def - avg_dist_miles_def_sn,
    diff_from_sn_avg_speed = avg_speed - avg_speed_sn,
    diff_from_sn_avg_speed_def = avg_speed_def - avg_speed_def_sn,
    diff_from_sn_avg_speed_off = avg_speed_off - avg_speed_off_sn,
    diff_from_sn_avg_fgpct = pctFG - fg_pct_sn,
    diff_from_sn_avg_fg3pct = pctFG3 - fg3_pct_sn,
    diff_from_sn_avg_oreb = oreb - oreb_sn,
    diff_from_sn_avg_dreb = dreb - dreb_sn,
    diff_from_sn_avg_stl = stl - stl_sn,
    diff_from_sn_avg_tov = tov - tov_sn,
    diff_from_sn_avg_ftpct = ftpct - ft_pct_sn,
    diff_from_sn_avg_pts = pts - pts_sn
  )


# Subsetting the data -----------------------------------------------------
gbg_12_plus_min <- gbg_player_w_distance %>% filter(min >12 )

gbg_8_plus_min <- gbg_player_w_distance %>% filter(min >8 )

gbg_4_plus_min <- gbg_player_w_distance %>% filter(min >4 )


# Player points per game and in-game speed --------------------------------

model <- lm(diff_from_sn_avg_pts ~ diff_from_sn_avg_speed_off, 
            data = gbg_12_plus_min )
summary(model)

# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.56854    0.02234  25.451  < 2e-16 ***
#   diff_from_sn_avg_speed_off  0.61772    0.07546   8.186 2.74e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.659 on 64399 degrees of freedom
# (126 observations deleted due to missingness)
# Multiple R-squared:  0.00104,	Adjusted R-squared:  0.001024 
# F-statistic: 67.02 on 1 and 64399 DF,  p-value: 2.741e-16


model <- lm(diff_from_sn_avg_pts ~ diff_from_sn_avg_speed_off, 
            data = gbg_8_plus_min )
summary(model) 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.36222    0.02129  17.012  < 2e-16 ***
#   diff_from_sn_avg_speed_off  0.43085    0.06729   6.403 1.53e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.586 on 69243 degrees of freedom
# (184 observations deleted due to missingness)
# Multiple R-squared:  0.0005917,	Adjusted R-squared:  0.0005773 
# F-statistic:    41 on 1 and 69243 DF,  p-value: 1.533e-10

model <- lm(diff_from_sn_avg_pts ~ diff_from_sn_avg_speed_off, 
            data = gbg_4_plus_min )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 0.16445    0.02044   8.047 8.64e-16 ***
#   diff_from_sn_avg_speed_off  0.14210    0.05601   2.537   0.0112 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.524 on 73711 degrees of freedom
# (246 observations deleted due to missingness)
# Multiple R-squared:  8.733e-05,	Adjusted R-squared:  7.376e-05 
# F-statistic: 6.438 on 1 and 73711 DF,  p-value: 0.01118



# fg pct and in-game speed/dist ----------------------------------------------

## Player must have attempted 1 fg
gbg_12_plus_min_filt <-  gbg_12_plus_min %>%
  filter(fga > 0 )

gbg_12_plus_min_filt %>%
  ggplot(aes(x=diff_from_sn_avg_fgpct, y = diff_from_sn_avg_speed_off ))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")

model <- lm(diff_from_sn_avg_fgpct ~ diff_from_sn_avg_speed_off, 
            data = gbg_12_plus_min_filt )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.002753   0.000765  -3.599  0.00032 ***
#   diff_from_sn_avg_speed_off  0.024494   0.002588   9.463  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1935 on 64149 degrees of freedom
# (118 observations deleted due to missingness)
# Multiple R-squared:  0.001394,	Adjusted R-squared:  0.001379 
# F-statistic: 89.56 on 1 and 64149 DF,  p-value: < 2.2e-16

gbg_8_plus_min_filt <-  gbg_8_plus_min %>%
  filter(fga > 0 )

gbg_8_plus_min_filt %>%
  ggplot(aes(x=diff_from_sn_avg_fgpct, y = diff_from_sn_avg_speed_off ))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")

model <- lm(diff_from_sn_avg_fgpct ~ diff_from_sn_avg_speed_off, 
            data = gbg_8_plus_min_filt )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.0065151  0.0007801  -8.352  < 2e-16 ***
#   diff_from_sn_avg_speed_off  0.0189890  0.0024744   7.674 1.69e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2039 on 68730 degrees of freedom
# (172 observations deleted due to missingness)
# Multiple R-squared:  0.0008561,	Adjusted R-squared:  0.0008416 
# F-statistic: 58.89 on 1 and 68730 DF,  p-value: 1.688e-14


gbg_4_plus_min_filt <-  gbg_4_plus_min %>%
  filter(fga > 0 )

gbg_4_plus_min_filt %>%
  ggplot(aes(x=diff_from_sn_avg_fgpct, y = diff_from_sn_avg_speed_off ))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")

model <- lm(diff_from_sn_avg_fgpct ~ diff_from_sn_avg_speed_off, 
            data = gbg_4_plus_min_filt )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.0103579  0.0008036 -12.889  < 2e-16 ***
#   diff_from_sn_avg_speed_off  0.0130508  0.0022653   5.761 8.39e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2154 on 72421 degrees of freedom
# (216 observations deleted due to missingness)
# Multiple R-squared:  0.0004581,	Adjusted R-squared:  0.0004443 
# F-statistic: 33.19 on 1 and 72421 DF,  p-value: 8.386e-09

## distance not speed
gbg_12_plus_min_filt %>%
  ggplot(aes(x=diff_from_sn_avg_fgpct, y = diff_from_sn_avg_off ))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")

model <- lm(diff_from_sn_avg_fgpct ~ diff_from_sn_avg_speed_off, 
            data = gbg_12_plus_min_filt )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.002753   0.000765  -3.599  0.00032 ***
#   diff_from_sn_avg_speed_off  0.024494   0.002588   9.463  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1935 on 64149 degrees of freedom
# (118 observations deleted due to missingness)
# Multiple R-squared:  0.001394,	Adjusted R-squared:  0.001379 
# F-statistic: 89.56 on 1 and 64149 DF,  p-value: < 2.2e-16



# Ft ----------------------------------------------------------------------

model <- lm(diff_from_sn_avg_ftpct ~ diff_from_sn_avg_speed_off, 
            data = gbg_12_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                -0.003136   0.001288  -2.434   0.0149 *
#   diff_from_sn_avg_speed_off  0.001163   0.004560   0.255   0.7986  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2632 on 41917 degrees of freedom
# (22608 observations deleted due to missingness)
# Multiple R-squared:  1.553e-06,	Adjusted R-squared:  -2.23e-05 
# F-statistic: 0.0651 on 1 and 41917 DF,  p-value: 0.7986

model <- lm(diff_from_sn_avg_ftpct ~ diff_from_sn_avg_speed_off, 
            data = gbg_8_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                -3.549e-03  1.277e-03  -2.778  0.00547 **
#   diff_from_sn_avg_speed_off -7.703e-05  4.343e-03  -0.018  0.98585   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2653 on 43394 degrees of freedom
# (26033 observations deleted due to missingness)
# Multiple R-squared:  7.249e-09,	Adjusted R-squared:  -2.304e-05 
# F-statistic: 0.0003145 on 1 and 43394 DF,  p-value: 0.9859


# fg3 pct -----------------------------------------------------------------
gbg_12_plus_min_filt_3 <- gbg_12_plus_min %>%
  filter(fg3a > 0 )


model <- lm(diff_from_sn_avg_fg3pct ~ diff_from_sn_avg_speed_off, 
            data = gbg_12_plus_min_filt_3 )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.0134083  0.0013549  -9.896   <2e-16 ***
#   diff_from_sn_avg_speed_off  0.0004391  0.0046169   0.095    0.924    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2932 on 46965 degrees of freedom
# (53 observations deleted due to missingness)
# Multiple R-squared:  1.926e-07,	Adjusted R-squared:  -2.11e-05 
# F-statistic: 0.009045 on 1 and 46965 DF,  p-value: 0.9242

gbg_8_plus_min_filt_3 <- gbg_8_plus_min %>%
  filter(fg3a > 0 )

model <- lm(diff_from_sn_avg_fg3pct ~ diff_from_sn_avg_speed_off, 
            data = gbg_8_plus_min_filt_3 )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.016675   0.001338 -12.465   <2e-16 ***
#   diff_from_sn_avg_speed_off -0.001065   0.004331  -0.246    0.806    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2965 on 49381 degrees of freedom
# (83 observations deleted due to missingness)
# Multiple R-squared:  1.225e-06,	Adjusted R-squared:  -1.903e-05 
# F-statistic: 0.06049 on 1 and 49381 DF,  p-value: 0.8057

gbg_4_plus_min_filt_3 <- gbg_4_plus_min %>%
  filter(fg3a > 0 )

model <- lm(diff_from_sn_avg_fg3pct ~ diff_from_sn_avg_speed_off, 
            data = gbg_4_plus_min_filt_3 )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.018838   0.001331  -14.15   <2e-16 ***
#   diff_from_sn_avg_speed_off  0.001034   0.003973    0.26    0.795    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2996 on 50996 degrees of freedom
# (102 observations deleted due to missingness)
# Multiple R-squared:  1.329e-06,	Adjusted R-squared:  -1.828e-05 
# F-statistic: 0.06779 on 1 and 50996 DF,  p-value: 0.7946

# Turnovers ---------------------------------------------------------------

model <- lm(diff_from_sn_avg_tov ~ diff_from_sn_avg_speed, 
            data = gbg_12_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.061170   0.004802  12.739   <2e-16 ***
#   diff_from_sn_avg_speed 0.032852   0.025106   1.309    0.191    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.216 on 64399 degrees of freedom
# (126 observations deleted due to missingness)
# Multiple R-squared:  2.659e-05,	Adjusted R-squared:  1.106e-05 
# F-statistic: 1.712 on 1 and 64399 DF,  p-value: 0.1907

model <- lm(diff_from_sn_avg_tov ~ diff_from_sn_avg_speed, 
            data = gbg_8_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.043475   0.004552   9.551   <2e-16 ***
#   diff_from_sn_avg_speed 0.006255   0.022536   0.278    0.781    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.193 on 69243 degrees of freedom
# (184 observations deleted due to missingness)
# Multiple R-squared:  1.113e-06,	Adjusted R-squared:  -1.333e-05 
# F-statistic: 0.07704 on 1 and 69243 DF,  p-value: 0.7813


model <- lm(diff_from_sn_avg_tov ~ diff_from_sn_avg_speed, 
            data = gbg_4_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.021821   0.004337   5.031 4.89e-07 ***
#   diff_from_sn_avg_speed -0.025066   0.019394  -1.292    0.196    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.171 on 73711 degrees of freedom
# (246 observations deleted due to missingness)
# Multiple R-squared:  2.266e-05,	Adjusted R-squared:  9.096e-06 
# F-statistic:  1.67 on 1 and 73711 DF,  p-value: 0.1962


# Offensive Rebounds ----------------------------------------------------------------

model <- lm(diff_from_sn_avg_oreb ~ diff_from_sn_avg_speed, 
            data = gbg_12_plus_min )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.066117   0.004622  14.304   <2e-16 ***
#   diff_from_sn_avg_speed -0.046087   0.024168  -1.907   0.0565 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.171 on 64399 degrees of freedom
# (126 observations deleted due to missingness)
# Multiple R-squared:  5.646e-05,	Adjusted R-squared:  4.094e-05 
# F-statistic: 3.636 on 1 and 64399 DF,  p-value: 0.05653


model <- lm(diff_from_sn_avg_oreb ~ diff_from_sn_avg_speed, 
            data = gbg_8_plus_min )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.045507   0.004391  10.365  < 2e-16 ***
#   diff_from_sn_avg_speed -0.082551   0.021737  -3.798 0.000146 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.151 on 69243 degrees of freedom
# (184 observations deleted due to missingness)
# Multiple R-squared:  0.0002082,	Adjusted R-squared:  0.0001938 
# F-statistic: 14.42 on 1 and 69243 DF,  p-value: 0.0001462

model <- lm(diff_from_sn_avg_oreb ~ diff_from_sn_avg_speed, 
            data = gbg_4_plus_min )
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.022350   0.004191   5.334 9.66e-08 ***
#   diff_from_sn_avg_speed -0.100168   0.018737  -5.346 9.02e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.131 on 73711 degrees of freedom
# (246 observations deleted due to missingness)
# Multiple R-squared:  0.0003876,	Adjusted R-squared:  0.000374 
# F-statistic: 28.58 on 1 and 73711 DF,  p-value: 9.024e-08


# Defensive Rebounds ------------------------------------------------------

model <- lm(diff_from_sn_avg_dreb ~ diff_from_sn_avg_speed, 
            data = gbg_12_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.193864   0.008327  23.283   <2e-16 ***
#   diff_from_sn_avg_speed 0.028470   0.043537   0.654    0.513    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.109 on 64399 degrees of freedom
# (126 observations deleted due to missingness)
# Multiple R-squared:  6.64e-06,	Adjusted R-squared:  -8.888e-06 
# F-statistic: 0.4276 on 1 and 64399 DF,  p-value: 0.5132

model <- lm(diff_from_sn_avg_dreb ~ diff_from_sn_avg_speed, 
            data = gbg_8_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.132161   0.007935  16.655   <2e-16 ***
#   diff_from_sn_avg_speed -0.056542   0.039286  -1.439     0.15    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.08 on 69243 degrees of freedom
# (184 observations deleted due to missingness)
# Multiple R-squared:  2.991e-05,	Adjusted R-squared:  1.547e-05 
# F-statistic: 2.071 on 1 and 69243 DF,  p-value: 0.1501

model <- lm(diff_from_sn_avg_dreb ~ diff_from_sn_avg_speed, 
            data = gbg_4_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             0.063355   0.007617   8.317  < 2e-16 ***
#   diff_from_sn_avg_speed -0.122862   0.034059  -3.607  0.00031 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.057 on 73711 degrees of freedom
# (246 observations deleted due to missingness)
# Multiple R-squared:  0.0001765,	Adjusted R-squared:  0.0001629 
# F-statistic: 13.01 on 1 and 73711 DF,  p-value: 0.0003096

# Steals ------------------------------------------------------------------

model <- lm(diff_from_sn_avg_stl ~ diff_from_sn_avg_speed, 
            data = gbg_12_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.044025   0.003717  11.845  < 2e-16 ***
#   diff_from_sn_avg_speed 0.135970   0.019434   6.996 2.65e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9416 on 64399 degrees of freedom
# (126 observations deleted due to missingness)
# Multiple R-squared:  0.0007595,	Adjusted R-squared:  0.000744 
# F-statistic: 48.95 on 1 and 64399 DF,  p-value: 2.651e-12

model <- lm(diff_from_sn_avg_stl~ diff_from_sn_avg_speed, 
            data = gbg_8_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.028971   0.003517   8.237  < 2e-16 ***
#   diff_from_sn_avg_speed 0.093894   0.017412   5.392 6.97e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.922 on 69243 degrees of freedom
# (184 observations deleted due to missingness)
# Multiple R-squared:  0.0004198,	Adjusted R-squared:  0.0004053 
# F-statistic: 29.08 on 1 and 69243 DF,  p-value: 6.974e-08


model <- lm(diff_from_sn_avg_stl ~ diff_from_sn_avg_speed, 
            data = gbg_4_plus_min)
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.013760   0.003344   4.115 3.87e-05 ***
#   diff_from_sn_avg_speed 0.049214   0.014951   3.292 0.000996 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9028 on 73711 degrees of freedom
# (246 observations deleted due to missingness)
# Multiple R-squared:  0.000147,	Adjusted R-squared:  0.0001334 
# F-statistic: 10.84 on 1 and 73711 DF,  p-value: 0.000996



# Table generation --------------------------------------------------------
name <- c("points", "points","fg_pct", "fg_pct", "fg3_pct", "fg3_pct", "turnovers",
          "turnovers","off_rebound", "off_rebound", "def_rebound", "def_rebound",
          "ft_pct", "ft_pct", "steals","steals")
min_played <- rep(c(4,12),8)

n <- c(73959,64527,72639,64527,73959,64527, 47020, 51100, 73959, 64527, 73959,
         64527, 73959, 64527, 73959, 64527)

coeff_val <- c(0.14210, 0.61772, 0.0130508, 0.024494, -7.703e-05, 0.001163, 0.001034,
           0.0004391, -0.025066, 0.032852, -0.100168, -0.046087, -0.122862, 
           0.028470, 0.049214, 0.135970)

p_val <- c(0.0112, 2.74e-16, 8.39e-09, 2e-16, 0.98585, 0.7986, 0.795, 0.924,
           0.196, 0.191, 9.02e-08, 0.0565 , 0.00031, 0.513, 0.000996,2.65e-12) 

adj_r_sq <- c( 7.376e-05 ,0.001024, 0.0004443, 0.001379, -2.304e-05, -2.23e-05,
               -1.828e-05, -2.11e-05, 9.096e-06, 1.106e-05, 0.000374, 4.094e-05,
               0.0001629, -8.888e-06, 0.0001334, 0.000744)

table <- data.frame(name, min_played, n,coeff_val, p_val, adj_r_sq)
table$coeff_val <- round(table$coeff_val, 4)
table_4 <- table%>% filter(min_played == 4) %>% select(-min_played)
table_12 <- table%>% filter(min_played == 12) %>% select(-min_played)

table_4 <- table_4 %>% rename("p-value"= p_val,
                              "coeff value" = coeff_val,
                               "adj r-squared" = adj_r_sq)

table_12 <- table_12 %>% rename("p-value"= p_val,
                                "coeff value" = coeff_val,
                                "adj r-squared" = adj_r_sq)
library(gt)

res_4 <- table_4 %>%
gt(rowname_col = "name") %>%
  tab_header(
    title = md("**Work and Player Perfromance**"),
    subtitle = md("Only players in the game for >4 min")
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
      rows = c(2,4,6,8))
  )%>%
  tab_footnote(
    footnote = "one field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg_pct")
  ) %>%
  tab_footnote(
    footnote = "one 3-point field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg3_pct")
  )


res_12 <- table_12 %>%
  gt(rowname_col = "name") %>%
  tab_header(
    title = md("**Work and Player Perfromance**"),
    subtitle = md("Only players in the game for >12 min")
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
      rows = c(2,4,6,8))
  ) %>%
  tab_footnote(
    footnote = "one field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg_pct")
    ) %>%
  tab_footnote(
    footnote = "one 3-point field goal had to be attempted to get included",
    locations = cells_stub(
      rows = "fg3_pct")
  )

library(webshot)
gtsave(res_4,"work_and_player_perfromance_4min.png",path= "./Paper/")
gtsave(res_12,"work_and_player_perfromance_12min.png",path= "./Paper/")

