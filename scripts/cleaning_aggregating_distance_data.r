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


# Day by day tracking data ----------------------------------------------

## Pass the function a vector with the start and end years of the season 
## ex. c(2016,2017)

distance_by_day <- function(season){

## clearing out tmp variables pre run
accumlate_games <- c()
west <- c()
east <- c()
all_for_day <- c()
tmp <- c()
start<- c()
finish <- c()

## Setting up season parse
start <- season[1]
finish <- season[2]

## first half of the season binding the results from each day 
for(month in c(10,11,12)){
  
  for(day in 1:31)
  {
    Sys.sleep(1)
    ## setting the date to scrape
    date <- paste(month, day, start, sep = "/")
    print(paste("Getting ",date))
    
    ## getting the date 
    single_day <- get_tracking(season = start, measure_type = "SpeedDistance",
                        type =c("Player"), date_from = date,
                        date_to = date
                        )
    
    ## Checking if there was a record on that given day, else do nothing
    if(length(single_day) != 1)
    {
      single_day <- single_day %>% 
          mutate("date" = date)

      accumlate_games <- rbind(accumlate_games,single_day)
    }
  }
  
}

## second half of the season binding the results from each day 
for(month in 1:6){
  
  for(day in 1:31)
  {
    Sys.sleep(1)
    ## setting the date to scrape
    date <- paste(month, day, finish, sep = "/")
    print(paste("Getting ",date))
    
    ## getting the date 
    single_day <- get_tracking(season = start, measure_type = "SpeedDistance",
                               type =c("Player"), date_from = date,
                               date_to = date
                                )
    
    ## Checking if there was a record on that given day, else do nothing
    if(length(single_day) != 1)
    {
      single_day <- single_day %>% 
        mutate("date" = date)
      
      accumlate_games <- rbind(accumlate_games,single_day)
    }
  }
  }
return(accumlate_games) 
}
# Generating and writing results to CSV ----------------------------------

## Storing the results of game by game data by season.

## 2019/20 
season_2019_20 <- distance_by_day(c(2019,2020))

write.csv(season_2019_20,
          "./data/distance_by_player_by_day/tracking_by_player_season_daily_2019_20.csv",
          row.names = FALSE)

## 2018/19
season_2018_19 <- distance_by_day(c(2018,2019))

write.csv(season_2018_19,
          "./data/distance_by_player_by_day/tracking_by_player_season_daily_2018_19.csv",
          row.names = FALSE)

## 2017/18
season_2017_18 <- distance_by_day(c(2017,2018))

write.csv(season_2017_18,
          "./data/distance_by_player_by_day/tracking_by_player_season_daily_2017_18.csv",
          row.names = FALSE)

## 2016/17
season_2016_17 <- distance_by_day(c(2016,2017))

write.csv(season_2016_17,
          "./data/distance_by_player_by_day/tracking_by_player_season_daily_2016_17.csv",
          row.names = FALSE)

## 2015/16
season_2015_16 <- distance_by_day(c(2015,2016))

write.csv(season_2015_16,
          "./data/distance_by_player_by_day/tracking_by_player_season_daily_2015_16.csv",
          row.names = FALSE)

## 2014/15
season_2014_15 <- distance_by_day(c(2014,2015))

write.csv(season_2014_15,
          "./data/distance_by_player_by_day/tracking_by_player_season_daily_2014_15.csv",
          row.names = FALSE)




