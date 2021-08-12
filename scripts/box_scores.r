## PURPOSE: Box scores
## AUTHOR: Austin

devtools::install_github("abresler/nbastatR")

library(nbastatR)
library(NBAr)
library(ballr)

library(tidyverse)
library(dplyr)
library(readr)
library(future)



# Team Box Scores By Game -------------------------------------------------


accumlating_teams_box <- c()

for(year in 2013:2020){

season <- get_team_boxscore(season = year)
accumlating_teams_box<- rbind(accumlating_teams_box,season)

}

## write to csv
write.csv(accumlating_teams_box,
          "../data/box_scores/box_scores_game_by_game_team_2013_20.csv",
          row.names = FALSE)

# Players Box Scores By Game -------------------------------------
plan(multiprocess)

# Getting 2013 to 2015 ------------------------------------------

accumlating_players_box <- c()

for(year in 2017){
  
  ## Printing progress
  print(paste("getting",year))
  
  ## getting game ids
  schedule <- seasons_schedule(seasons = year)
  start<- min(schedule$idGame)
  end <- max(schedule$idGame)
  
  ## getting start to end for 
  print("getting box scores (lord help us)")
  season <- box_scores(
    box_score_types = c("Traditional"),
    game_ids = c(start:end),
    result_types = "player",
    assign_to_environment = FALSE
    
  )
  season <- season[[2]][[1]] %>% mutate(season = year)
  
  accumlating_players_box<- rbind(accumlating_players_box,season)
  
}

## write to csv
write.csv(accumlating_players_box,
          "data/box_scores/box_scores_game_by_game_player_2016_17.csv",
          row.names = FALSE)



# Getting 2016-2020 -------------------------------------------------

accumlating_players_box <- c()

for(year in 2013:2015){
  
  ## Printing progress
  print(paste("getting",year))
  
  ## getting game ids
  schedule <- seasons_schedule(seasons = year)
  start<- min(schedule$idGame)
  end <- max(schedule$idGame)
  
  ## getting start to end for 
  season <- box_scores(
    box_score_types = c("Traditional"),
    game_ids = c(start:end),
    result_types = "player",
    assign_to_environment = FALSE
    
  )
  season <- season[[2]][[1]] %>% mutate(season = year)
  
  accumlating_players_box<- rbind(accumlating_players_box,season)
  
}

## write to csv
write.csv(accumlating_players_box,
          "data/box_scores_game_by_game_player_2016_20.csv",
          row.names = FALSE)

# Season box scores -------------------------------------------------------

## Player Level
accumulate_player <- c()

for(season in 2010:2020){
  print(paste("Getting ", season))
  current_season<- get_general(season = season,
                               type = "Player",
                               measure_type = "Base" )
  
  current_season <- current_season %>% mutate("season" = season)
  
  accumulate_player <- rbind(accumulate_player,current_season)
  
}

write.csv(accumulate_player,
          "./data/box_scores/season_box_scores_player_2010_20.csv",
          row.names = FALSE)

## Team Level
accumulate_team <- c()

for(season in 2010:2020){
  print(paste("Getting ", season))
  current_season<- get_general(season = season,
                               type = "Team",
                               measure_type = "Base" )
  
  current_season <- current_season %>% mutate("season" = season)
  
  accumulate_team <- rbind(accumulate_team,current_season)
  
}
write.csv(accumulate_team,
          "./data/box_scores/season_box_scores_team_2010_20.csv",
          row.names = FALSE)
