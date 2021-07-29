# PURPOSE: cleaning and gathering data on individual players
# AUTHOR: Austin

library(tidyverse)
library(dplyr)
library(nbastatR)


# Constants ---------------------------------------------------------------

years <- c(2010:2021)
years_smaller <- c("2017-18","2018-19","2019-2020","2020-21")

# Player Position Table Key---------------------------------------------------

## Getting players positions 
positions_players <- teams_players_stats(seasons= years, 
                                         types = c("player"), 
                                         players_positions = c("C", "G", "F") )


## initializing an accumulator data frame
position_id_key <- c()

## binding the results from different years from the teams_players_stats call 
for(i in 1:24){
  
  ## adding season column to interior data
  positions_players[[7]][[i]] <- positions_players[[7]][[i]] %>% 
    mutate(season = positions_players[[4]][[i]] ) 
  
  ## combining the data into one frame
  position_id_key <- rbind(position_id_key, 
                           positions_players[[7]][[i]])
  
}

## writing player data including muliple seasons
position_id_key %>%
  select(PlayerPosition, idPlayer, namePlayer, season) %>%
write.csv( ".//data//player_table_all_2010_to_21.csv", 
          row.names = FALSE)

## writing just IDs, names and positions with no multiple seasons removed 
position_id_key %>%
  select(PlayerPosition, idPlayer, namePlayer) %>%
  distinct(idPlayer,PlayerPosition, .keep_all = TRUE) %>%
write.csv( ".//data//player_table_reduced_2010_to_21.csv", 
          row.names = FALSE)


# Player Tables with stats -------------------------------------------------

## making a vector of player IDs
player_id_2017_on <- read.csv(".//data//player_table_all_2010_to_21.csv") %>%
                      distinct(idPlayer, .keep_all = TRUE) %>%
                      filter(season %in% years_smaller) 
player_id_2017_on <- player_id_2017_on[,2]

## getting list of all players names
dictonary_players <- dictionary_bref_players()

## using players names to get ids (creates df_nba_player_dict automatically)
nba_player_ids(players = dictonary_players$namePlayerBREF)

## dropping url rows of player names and ids
df_nba_player_dict <- df_nba_player_dict %>% 
  select(isActive, isRookie, namePlayer, idPlayer,
         countSeasons, yearSeasonFirst, yearSeasonLast ) %>%
  filter(yearSeasonFirst %in% years)

## Must generate the player table for all player of interest in the current dir
players_tables(player_ids = player_id_2017_on, 
               tables = c("game splits"),
               modes = "PerGame" )

write.csv(dataGameSplitsPlayerBase, ".//data//player_game_splits_2017_on.csv")

