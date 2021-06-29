library(devtools)
library(airball)
library(nbastatR)
library(tidyverse)
library(ggplot2)
library(echarts4r)
library(echarts4r.assets) 
library(nbastatR)
library(ballr)
library(NBAr)
library(lubridate)




# Messing around with airball ---------------------------------------------
season2021 <- nba_travel(start_season = 2021,
                         end_season = 2021,
                         return_home = 3,
                         phase = "RS")

warriors_travel <- nba_travel(start_season = 2021,
           end_season = 2021,
           team = c("Golden State Warriors"),
           return_home = 3,
           phase = "RS")

nuggets2021 <- nba_travel(start_season = 2021,
                           end_season = 2021,
                           team = c("Denver Nuggets"),
                           return_home = 3,
                           phase = "RS")

#find a way to add cumulative win percent to the table
looney <- nba_player_travel(start_season = 2018,
                  end_season = 2020,
                  return_home = 3,
                  team = "Golden State Warriors",
                  player = "Kevon Looney")

#Flight path for a couple of teams
nba_travel_plot(data = season2021,
                team = c("Boston Celtics", "Golden State Warriors", "Denver Nuggets", "Atlanta Hawks"),
                city_color = "white",
                plot_background_fill = "black",
                land_color = "gray",
                caption_color = "lightblue",
                ncolumns = 2)
#Interactive Globe Map
warriors_travel %>% 
  filter(Route != "No Travel") %>%
  e_charts() %>% 
  e_globe(
    environment = ea_asset("starfield"),
    base_texture = ea_asset("world"), 
    height_texture = ea_asset("world"),
    displacementScale = 0.05
  ) %>% 
  e_lines_3d(
    Longitude, 
    Latitude, 
    d.Longitude, 
    d.Latitude,
    name = "flights",
    effect = list(show = TRUE)
  ) %>% 
  e_legend(FALSE)

#Arbitrary Density Indicators
warriors_density <- nba_density(warriors_travel)

#Injury Data
warriors_booboos <- nba_injuries(start_date = "2020-12-22",
                                 end_date = "2021-05-16",
                                 team = "Warriors")

warriors_schedule <- get_schedule(2020) %>%
  filter(visitor == "GSW" | home == "GSW") %>%
  rename(Date = game_date) %>%
  filter(season_type == "REG")

#Merge travel with arbitrary density indicators
warriors2021 <- merge(x = warriors_travel, 
                      y = warriors_density, by = "Date", suffix = c("", ".y"))  %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate("Date" = as.character(Date)) %>%
  merge(warriors_schedule, by = "Date") %>% 
  select(-c(series, game_date_utc, season_type, season_type_id, season)) 




# trying to convert timezone ----------------------------------------------


str(warriors2021$game_time_utc[1])
hour_minute <- hm(warriors2021$game_time_utc)
h <- parse_date_time(warriors2021$game_time_utc)
p <- with_tz(hm, tzone = "PDT")


              

# Messing around with NBAr ------------------------------------------------
sunsLakersGame1 <- get_all_tracking("2020", 
             measure_type = "SpeedDistance", 
             season_type = "Playoffs",
             type = "Player",
             date_from = "05/23/2021",
             date_to = "05/23/2021") %>%
  filter(team_abbreviation %in% c("PHX","LAL"))

sunsLakersGame2 <- get_all_tracking("2020", 
                                    measure_type = "SpeedDistance", 
                                    season_type = "Playoffs",
                                    type = "Player",
                                    date_from = "05/25/2021",
                                    date_to = "05/25/2021") %>%
  filter(team_abbreviation %in% c("PHX","LAL"))

sunsLakersGame3 <- get_all_tracking("2020", 
                                    measure_type = "SpeedDistance", 
                                    season_type = "Playoffs",
                                    type = "Player",
                                    date_from = "05/27/2021",
                                    date_to = "05/27/2021") %>%
  filter(team_abbreviation %in% c("PHX","LAL"))

sunsLakersGame4 <- get_all_tracking("2020", 
                                    measure_type = "SpeedDistance", 
                                    season_type = "Playoffs",
                                    type = "Player",
                                    date_from = "05/30/2021",
                                    date_to = "05/30/2021") %>%
  filter(team_abbreviation %in% c("PHX","LAL"))

sunsLakersGame5 <- get_all_tracking("2020", 
                                    measure_type = "SpeedDistance", 
                                    season_type = "Playoffs",
                                    type = "Player",
                                    date_from = "06/01/2021",
                                    date_to = "06/01/2021") %>%
  filter(team_abbreviation %in% c("PHX","LAL"))

sunsLakersGame6 <- get_all_tracking("2020", 
                                    measure_type = "SpeedDistance", 
                                    season_type = "Playoffs",
                                    type = "Player",
                                    date_from = "06/03/2021",
                                    date_to = "06/03/2021") %>%
  filter(team_abbreviation %in% c("PHX","LAL"))


sunsVsLakers <- rbind(sunsLakersGame1,sunsLakersGame2, sunsLakersGame3, 
                      sunsLakersGame4, sunsLakersGame5, sunsLakersGame6) %>%
  filter(player_name %in% c("LeBron James", "Anthony Davis", "Dennis Schroder", 
                            "Kentavious Caldwell-Pope", "Alex Caruso", "Marc Gasol",
                            "Devin Booker", "Chris Paul", "Mikal Bridges",
                            "Jae Crowder", "Deandre Ayton", "Cameron Payne", "Cameron Johnson")) %>%
  mutate(dist_per_min = dist_feet / min )


#Plotting distance per minute over the course of the first round playoff series
ggplot(data = filter(sunsVsLakers, team_abbreviation == "PHX"), aes(x = date_from, y = dist_per_min, group = player_name)) +
  geom_line(aes(color = player_name))

ggplot(data = filter(sunsVsLakers, team_abbreviation == "LAL"), aes(x = date_from, y = dist_per_min, group = player_name)) +
  geom_line(aes(color = player_name))

#Plotting avg speed over the course of the first round playoff series
ggplot(data = filter(sunsVsLakers, team_abbreviation == "PHX"), aes(x = date_from, y = avg_speed, group = player_name)) +
  geom_line(aes(color = player_name))

ggplot(data = filter(sunsVsLakers, team_abbreviation == "LAL"), aes(x = date_from, y = avg_speed, group = player_name)) +
  geom_line(aes(color = player_name))

shot_chart <- get_shotchart(1626164, 2018)

schedule <- get_schedule(2020)

drives <- get_all_tracking("2020", 
                           measure_type = "Drives", 
                           season_type = "Playoffs",
                           type = "Player",
                           date_from = "06/24/2021",
                           date_to = "06/25/2021")

hustle <- get_hustle(21800001)

schedule_2018_19 <- seasons_schedule(seasons = 2019)



# Messing around with nbastatR --------------------------------------------
player_table_data <- players_tables( 
  tables = c("game splits"),
  modes = "PerGame",
  n_games = 50,
  player_ids = c(203507),
  assign_to_environment = FALSE) 

player_table_data <- player_table_data %>% select(
  tableSlugName, nameTable, modeSearch,
  slugSeason, idPlayer, dataTable)

game_splits <- unnest(player_table_data[6][1],c(dataTable))









