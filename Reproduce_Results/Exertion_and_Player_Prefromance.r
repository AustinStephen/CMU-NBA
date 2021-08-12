## Author: Austin Stephen 
## Date: 8/11/2021
## Purpose: Self contained code to reproduce the results 
#           for paper section: "Exertion and Player Performance" 


## libraries
library(nbastatR)
library(tidyverse)
library(readr)


# Reading in the tracking data --------------------------------------------
# sn_19_20  <- read_csv(
#   "data/in_game_distance_data/tracking_by_player_season_daily_2019_20.csv")
# 
# sn_18_19   <- read_csv(
#   "data/in_game_distance_data/tracking_by_player_season_daily_2018_19.csv")
# 
# sn_17_18   <- read_csv(
#   "data/in_game_distance_data/tracking_by_player_season_daily_2017_18.csv")

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
  "data/box_scores/season_box_scores_player_2010_20.csv")


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
  by.x= c("player_id","date"),
  by.y= c("idPlayer","dateGame")
) %>%
  select("player_id","idGame","team_id","player_name", "groupStartPosition", date:season,
         -c("namePlayer"))


## FG percentage




##fgpct all distance
gbg_player_w_distance_at <-  gbg_player_w_distance %>%
  filter(fga > 0 )

gbg_player_w_distance %>%
  ggplot(aes(x=diff_from_sn_avg_fgpct, y =diff_from_sn_avg_dist ))+
  geom_point(alpha = .05)+
  geom_smooth(method="lm")

model <- lm(diff_from_sn_avg_fgpct ~ diff_from_sn_avg_dist, 
            data = gbg_player_w_distance_at )
summary(model)





