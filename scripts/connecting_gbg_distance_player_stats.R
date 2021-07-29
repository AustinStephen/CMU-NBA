## PURPOSE: Connect game by game distance data to player metrics
## AUTHOR: Austin 
library(dplyr)
library(tidyverse)
library(readr)


## 2013-2020 schedule data 
schedules_2013 <- read.csv("data/schedules/season_2013_2014.csv")
schedules_2014 <- read.csv("data/schedules/season_2014_2015.csv")
schedules_2015 <- read.csv("data/schedules/season_2015_2016.csv")
schedules_2016 <- read.csv("data/schedules/season_2016_2017.csv")
schedules_2017 <- read.csv("data/schedules/season_2017_2018.csv")
schedules_2018 <- read.csv("data/schedules/season_2018_2019.csv")
schedules_2019 <- read.csv("data/schedules/season_2019_2020.csv")
schedules_2020 <- read.csv("data/schedules/season_2020_2021.csv")

schedules_2013_20 <- rbind(schedules_2013, schedules_2014, schedules_2015, 
                           schedules_2016, schedules_2017, schedules_2018, 
                           schedules_2019, schedules_2020)

## clutter the environment
rm(schedules_2013, schedules_2014, schedules_2015, 
   schedules_2016, schedules_2017, schedules_2018, 
   schedules_2019, schedules_2020)

## player game by game distance 14/15 season
tracking_by_player_season_daily_2014_15 <- read.csv(
  "data/in_game_distance_data/tracking_by_player_season_daily_2014_15.csv") %>%
  select(-c("gp","min1","team_abbreviation" ))

## player game by game box scores 14/15 season
box_game_by_game_player_2014_15 <- read.csv(
  "data/box_scores/box_scores_game_by_game_player_2014_15.csv") %>%
  select(-c("slugTeam","isStarter", "cityTeam", "teamName", "minExact"))

## Player season box scores
season_box_scores_player_2010_20 <- read.csv(
  "data/box_scores/season_box_scores_player_2010_20.csv")

## Joining game by game box scores with distance
# fixing date format

schedules_2013_20 <- schedules_2013_20 %>% select(c(idGame, dateGame))

tracking_by_player_season_daily_2014_15$date <- as.Date(
  tracking_by_player_season_daily_2014_15$date, "%m/%d/%Y")

# joining with schedules for game id
box_game_by_game_player_2014_15 <- merge(
  schedules_2013_20,
  box_game_by_game_player_2014_15,
  by= "idGame",
)

# joining with distance on game id

gbg_player_w_distance <- merge(
  tracking_by_player_season_daily_2014_15,
  box_game_by_game_player_2014_15,
  by.x= c("player_id","date"),
  by.y= c("idPlayer","dateGame")
) %>%
  select("player_id","idGame","team_id","player_name", "groupStartPosition", date:season,
         -c("namePlayer"))

## adding season summaries column 
merge_sn <-  season_box_scores_player_2010_20 %>%
  filter(season == 2014) %>%
  select(c("player_id","w","w_pct","ft_pct","fg3_pct","fg_pct","oreb", "stl",
           "tov")) %>%
  rename("player_wins_sn" = w, "w_pct_team_sn" = w_pct, "ft_pct_sn" = ft_pct,
         "fg3_pct_sn" = fg3_pct,  "fg_pct_sn" = fg_pct, "oreb_sn" = oreb, 
         "stl_sn" = stl, "tov_sn" = tov)

gbg_player_w_distance <- merge(
  gbg_player_w_distance,
  merge_sn,
  by = "player_id")


## adding baselines and deviations columns

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
  diff_from_sn_avg_stl = stl - stl_sn,
  diff_from_sn_avg_tov = tov - tov_sn,
  diff_from_sn_avg_ftpct = ftpct - ft_pct_sn
  )

## adding out of game distance
together <- read_csv("matthew_data/together.csv") %>%
  select(c("Opponent","Team","game_id", "team_id", "Date","Season","Month","Week","Visitor", "City", "Win",
           "Distance", "Rest", "shift", "direction", "flight_duration", "B2B",
           "b2b_2nd","b2b_1st","three_in_four","four_in_five","five_in_seven",
           ))

write.csv(together, 
          "data/between_game_dist/between_game_dist.csv",
          row.names = FALSE)

write.csv(gbg_player_w_distance, 
          "data/in_game_distance_data/in_game_dist_box_join.csv",
          row.names = FALSE)



# Last 5 days summary -----------------------------------------------------

tmp <- gbg_player_w_distance %>%
  group_by(player_id) %>%
  mutate(total = cumsum(dist_miles)) %>%
  mutate(current_pos = row_number()) %>%
  mutate(total_5 = 
           total - ifelse(current_pos < 6, total,total - total[current_pos -5])) %>%
  select(player_id, date, dist_miles, total, total_5, current_pos )


accumulate_5_day_summaries <- c()
sum_n_days <- function(n,data){
  
  for(day in data$date){
    print(paste(day,player_id,sep=" "))
    subset_days <- data %>% filter(date %in% c(seq(day-n,day-1,1)))
  
    total_dist_for_window <- subset_days %>% group_by(player_id) %>%
      summarise(total = sum(dist_miles), date = day)
    
    accumulate_5_day_summaries <- rbind(accumulate_5_day_summaries,
                                        total_dist_for_window )
  }
  
}

 
gbg_player_w_distance <- merge(
  accumulate_5_day_summaries,
  accumulate_5_day_summaries,
  by.x= c("player_id","date"),
  by.y= c("player_id","date"))


write.csv(gbg_player_w_distance, 
          "data/in_game_distance_data/in_game_dist_prev_games_sum.csv",
          row.names = FALSE)







 