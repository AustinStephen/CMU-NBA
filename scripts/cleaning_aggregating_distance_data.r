## PURPOSE: Create and Clean Distance Traveled Data 
## Author: Austin


# Setup and global constants ----------------------------------------------

library(tidyverse)
library(dplyr)
library(nbastatR)
library(NBAr)

# Season ------------------------------------------------------------------


# 2013 to 2020 player data ------------------------------------------------


accumlate_player <- c()

## binding the results from different years from the teams_players_stats call 
for(i in 2013:2020){
  
  ## adding season column to interior data
  tracking_players <- get_tracking(season = i, 
                                   type = c("Player"),
                                   measure_type = c("SpeedDistance")
                                  ) %>%
    mutate(season = i)
  
  ## combining the data into one frame
  accumlate_player <- rbind(accumlate_player, tracking_players)
  
}

write.csv(accumlate_player,
          "./data/tracking_by_player_season_summaries_2013_to_20.csv",
          row.names = FALSE)


# 2013 to 2020 team data --------------------------------------------------

accumlate_team <- c()

## binding the results from different years from the teams_players_stats call 
for(i in 2013:2020){
  
  ## adding season column to interior data
  tracking_teams <- get_tracking(season = i, 
                                   type = "Team",
                                   measure_type = c("SpeedDistance")
  ) %>%
    mutate(season = i)
  
  ## combining the data into one frame
  accumlate_team <- rbind(accumlate_team, tracking_teams)
  
}

write.csv(accumlate_team,
          "./data/tracking_by_team_season_summaries_2013_to_20.csv",
          row.names = FALSE)


# Game by game tracking data ----------------------------------------------


