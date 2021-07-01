## Setting up the packages
library(tidyverse)
library(dplyr)
library(nbastatR)

# Reference Dataframes ----------------------------------------------------
schedule_2018_19 <- seasons_schedule(seasons = 2019)

teams_dictonary <- nba_teams()
  
game_ids_2019_20 <- c(21800001:21801230)

## data collection from API and writing to
dictonary_terms <- dictionary_nba_names()

## getting list of all players names
dictonary_players <- dictionary_bref_players()

# Box Score ---------------------------------------------------------------

box_scores_imported <- box_scores(game_ids = c(21800001:21800005), 
                         join_data = TRUE, 
                         assign_to_environment = FALSE)## needs game ids 

## code removes the nested data frame so it can be written to  csv
player_box_scores <- unnest(box_scores_imported[1,2], cols = c(dataBoxScore))

## writing to csv
write.csv(player_box_scores, file= "sample_box_scores_5_games.csv",)


# Players Tables ----------------------------------------------------------
player_table_data <- players_tables( 
                        tables = c("game splits"),
                        modes = "PerGame",
                        n_games = 50,
                        player_ids = c(203507),
                        assign_to_environment = FALSE) 

player_table_data <- player_table_data %>% select(
                                      tableSlugName, nameTable, modeSearch,
                                      slugSeason, idPlayer, dataTable)
game_splits <- unnest(player_table_data[11][1],c(dataTable))

write.csv(game_splits, "sample_game_splits_1_player.csv")
splits <- read.csv("sample_game_splits_1_player.csv")

player_table_data <- players_tables( 
  tables = c("general splits"),
  modes = "PerGame",
  n_games = 50,
  player_ids = c(203507),
  assign_to_environment = FALSE) 

game_splits <- unnest(player_table_data[11][1],c(dataTable))



# Visualizing distance -----------------------------------------------------

## scatter plot distance per game and wins
ggplot(tracking_teams, aes(x = dist_miles, y= w)) +
    geom_point()+
    theme_bw()
    

summary(tracking_players)


