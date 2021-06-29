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

## using list of players names to get ids
nba_player_ids(players = dictonary_players$namePlayerBREF)

## dropping extra rows of player names and ids
df_nba_player_dict <- df_nba_player_dict %>% 
              select(isActive, isRookie, namePlayer, idPlayer,
                     countSeasons, yearSeasonFirst, yearSeasonLast )

## getting players profiles  
testing <- player_profiles( players = "Stephen Curry") %>% 
      select(idPlayer, idTeam, namePlayer, yearDraft, numberOverallPick, slugTeam,
             heightInches, weightLBS, pts, ast, treb, ratioPIE )

positions_players <- teams_players_stats(seasons= c(2015,2016,2017,2018,2019,2020) , 
                                        types = c("player"), 
                                        players_positions = c("C", "G", "F") )


position_id_key <- do.call("rbind", list(positions_players[[7]][[1]],
                      positions_players[[7]][[2]],
                      positions_players[[7]][[3]],
                      positions_players[[7]][[4]],
                      positions_players[[7]][[5]],
                      positions_players[[7]][[6]],
                      positions_players[[7]][[7]],
                      positions_players[[7]][[8]],
                      positions_players[[7]][[9]],
                      positions_players[[7]][[10]],
                      positions_players[[7]][[11]],
                      positions_players[[7]][[12]],
                      positions_players[[7]][[13]],
                      positions_players[[7]][[14]],
                      positions_players[[7]][[15]],
                      positions_players[[7]][[16]],
                      positions_players[[7]][[17]],
                      positions_players[[7]][[18]]
                      )) %>%
                      select(PlayerPosition, idPlayer, namePlayer)

write.csv(df_nba_player_dict, ".//data//player_names_ids_dictonary.csv")
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



# Looking for systemic relationship with distance traveled -------------------
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("PatrickChodowski/NBAr",force =TRUE)

library(NBAr)

hustle <- get_hustle(21800001)

## cannot get the function get_shooting() to work 

traditional <- map(c(21800001), ~get_boxscore(.,boxscore_type = 'traditional')) %>% compact() %>%  bind_rows()

## getting the tracking data for 2018 
tracking_players <- get_tracking(season = c(2018), 
                        type = c("Player"),
                        measure_type = c("SpeedDistance"),
                        game_segment = c(""))

tracking_players <- merge(x = dictonary_players, y= tracking_players, 
                          by= "Player")

write.csv(tracking_players,"player_tracking_distance_2018.csv")

tracking_teams <- get_tracking(season = c(2018), 
                      type = c("Team"),
                      measure_type = c("SpeedDistance"),
                      game_segment = c("First+Half"),
                      period = 1)

write.csv(tracking_teams,"team_tracking_distance_2018.csv")

play_by_play <- get_shooting(game_id = 21800001)


# Visualizing distance -----------------------------------------------------

## CDF for distance
tracking_players %>%
  filter(avg_speed >.25 & avg_speed < 6) %>%
ggplot(aes(x = avg_speed)) + 
  stat_ecdf() + 
  theme_bw() +
  geom_rug(alpha = .005)+
  geom_abline( intercept = 0.5, slope = 0, color = "red")+
  labs(title = "CDF for distance traveled in a game", caption = "")

## scatter plot distance per game and wins
ggplot(tracking_teams, aes(x = dist_miles, y= w)) +
    geom_point()+
    theme_bw()
    
summary(tracking_players)

