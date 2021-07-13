library(tidyverse)


# nbastatsR package -------------------------------------------------------


devtools::install_github("abresler/nbastatR")

library(nbastatR)


seasons_schedule(
  seasons = 2019,
  season_types = "Regular Season",
  parse_boxscores = F,
  box_score_tables = c("Traditional"),
  nest_data = FALSE,
  return_message = TRUE
)

seasons_schedule(seasons = c(2012, 2018))


teams <- all_nba_teams(return_message = TRUE)

win_probability(
  game_ids = c(21700002, 21700003),
  nest_data = FALSE,
  filter_non_plays = FALSE,
  return_message = TRUE
)
win_probability(game_ids = c(21700002, 21700005), filter_non_plays = T,
                nest_data = FALSE,
                return_message = TRUE)

team_player_stats_example <- teams_players_stats(
  seasons = 2018,
  types = c("player", "team"),
  tables = c("defense"),
  season_types = "Regular Season",
  measures = "Base",
  modes = "PerGame",
  defenses = "Overall",
  is_plus_minus = F,
  is_pace_adjusted = F,
  periods = 0,
  is_rank = F,
  game_segments = NA,
  divisions_against = NA,
  conferences_against = NA,
  date_from = NA,
  date_to = NA,
  last_n_games = 0,
  locations = NA,
  months = 0,
  season_segments = NA,
  opponents = NA,
  countries = NA,
  weights = NA,
  outcomes = NA,
  playoff_rounds = 0,
  players_experience = NA,
  players_positions = NA,
  colleges = NA,
  draft_picks = NA,
  draft_years = NA,
  game_scopes = NA,
  heights = NA,
  shot_clock_ranges = NA,
  clutch_times = "Last 5 Minutes",
  ahead_or_behind = "Ahead or Behind",
  general_ranges = "Overall",
  dribble_ranges = "0 Dribbles",
  shot_distance_ranges = "By Zone",
  touch_time_ranges = NA,
  closest_defender_ranges = NA,
  point_diffs = 5,
  starters_bench = NA,
  assign_to_environment = TRUE,
  add_mode_names = T,
  return_message = TRUE
)

box_score_exp <-
box_scores(game_ids = c(21700002, 21700003), 
           box_score_types = c("Traditional", "Advanced", "Scoring", "Misc", "Usage", "Four Factors", "Tracking"), 
           result_types = c("player", "team"), 
           join_data = TRUE, 
           assign_to_environment = TRUE, 
           return_message = TRUE)

player_tables<-
players_tables(players = "Caris LeVert", 
               tables =  c("rebounding", "shots"),   
               modes = "PerGame", measures = c("Base", "Advanced"), 
               date_from = "01-01-2018",
               date_to = "01-01-2019",
               periods = 4,
               assign_to_environment = TRUE)

schedule_2018_19 <- seasons_schedule(seasons = 2019)


player_tables1<-
  players_tables(players = "Caris LeVert", 
                 tables =  c("rebounding", "shots"),   
                 modes = "PerGame", measures = c("Base", "Advanced"), 
                 date_from = "01-01-2018",
                 date_to = "01-01-2019",
                 game_segments = "Second Half",
                 periods = 3,
                 assign_to_environment = TRUE)

player_table_data <- players_tables(tables = c("game splits"),
                                    modes = "PerGame",
                                    n_games = 50,
                                    player_ids = c(203507),
                                    assign_to_environment = FALSE)

game_splits <- unnest(player_table_data[11][1], c(dataTable))

write.csv(game_splits, "sample_game_splits_1_player.csv")
splits <- read.csv("sample_game_splits_1_player.csv")

player_table_data <- players_tables(table = c("general splits"),
                                    modes = "PerGame",
                                    n_games = 50,
                                    player_ids = c(203507),
                                    assign_to_environment = FALSE)

game_splits <- unnest(player_table_data[11][1], c(dataTable))

team_id <- nba_teams_ids(teams = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets",
                        "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",
                        "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers",
                        "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", "Miami Heat",
                        "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans","New York Knicks",
                        "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns",
                        "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs",
                        "Toronto Raptors", "Utah Jazz", "Washington Wizards")) 

tibble(team_id)

Team <- c("Atlanta Hawks", "Boston Celtics", "Cleveland Cavaliers", "New Orleans Pelicans", 
          "Chicago Bulls","Dallas Mavericks", "Denver Nuggets",
          "Golden State Warriors", "Houston Rockets", "Los Angeles Clippers", "Los Angeles Lakers",
          "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "Brooklyn Nets",
          "New York Knicks", "Orlando Magic", "Indiana Pacers", "Philadelphia 76ers", "Phoenix Suns",
          "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs",
          "Oklahoma City Thunder", "Toronto Raptors", "Utah Jazz", "Memphis Grizzlies", 
          "Washington Wizards", "Detroit Pistons", "Charlotte Hornets")

team_abbreviation <- c("ATL", "BOS", "CLE", "NOP",
                       "CHI", "DAL","DEN", "GSW", "HOU",
                       "LAC", "LAL", "MIA", "MIL", "MIN",
                       "BKN", "NYK", "ORL", "IND", "PHI", "PHX", 
                       "POR", "SAC", "SAS", "OKC",
                       "TOR", "UTA", "MEM", "WAS", "DET", "CHA")

team_name_id <- tibble(x = team_id, y =Team)

team_name_id <- team_name_id %>%
  mutate("team_id" = x, "Team" = y)

team_name_id <- select(team_name_id, -x , -y)

nba_teams_ids(teams = c("Atlanta Hawks"))
                        
teams <- nba_teams(join_blg = F) %>%
  #filter(idLeague == 4)

# airball package ---------------------------------------------------------


install.packages("devtools")
devtools::install_github("josedv82/airball")
library(airball)

nba_travel(start_season = 2017,
           end_season = 2020,
           team = c("Los Angeles Lakers", "Boston Celtics"),
           return_home = 3,
           phase = "RS")

nba_player_travel(start_season = 2018,
                  end_season = 2020,
                  return_home = 4,
                  team = "Cleveland Cavaliers",
                  player = "Jose Calderon")

# flight paths plot

datos <- nba_travel(start_season = 2015, end_season = 2018)

nba_travel_plot(data = datos,
                season = 2017,
                team = c("Boston Celtics", "Miami Heat"),
                city_color = "white",
                plot_background_fill = "black",
                land_color = "gray",
                caption_color = "lightblue",
                ncolumns = 2)

nba_travel_plot(data = datos,
                season = 2017,
                city_color = "white",
                plot_background_fill = "black",
                land_color = "gray",
                caption_color = "lightblue")

install.packages("echarts4r")
library(echarts4r)
remotes::install_github("JohnCoene/echarts4r.assets")
library(echarts4r.assets)
library(airball)
library(tidyverse)

data <- nba_travel(start_season = 2015, end_season = 2019)

data %>% 
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

#density indicators

datos <- nba_travel()
density <- nba_density(df = data)

#injury transactions

nba_injuries(start_date = "2012-01-01",
             end_date = "2014-01-01",
             team = "")



nba_injuries(start_date = "2019-01-01",
             end_date = "2020-01-01",
             player = "Luka Doncic",
             team = "")

nba_names <- dictionary_nba_names()



# NBAr package ------------------------------------------------------------

devtools::install_github("PatrickChodowski/NBAr")
library(NBAr)

season <- 2018
gamelist<- c(21800001:21800003)
game_id = 21800001

#boxscores:
traditional <- map(gamelist, ~get_boxscore(.,boxscore_type = 'traditional')) %>% compact() %>%  bind_rows()

#matchups
matchup <- get_matchups(game_id)


#play by play
pbp2 <- map(gamelist, ~get_playbyplay2(.)) %>% compact() %>%  bind_rows()

#lineups
lineups <- get_lineups(season,5,'Base')
bulls_lineups <- get_lineups(season,5,'Base', team_id = 1610612741)

#on/off court
onoff_bulls <- get_on_off(season,1610612741)

#player list
players <- get_players(season)
player_bio <- get_playerbio(season)

#shot charts
shots_abrines <- get_shotchart(203518, season)

#tracking data
tracking = get_tracking(season, 'Team', measure_type = 'Defense')

#playtype data
playtype <- get_playtype(season, 'T',  'Postup')

#defense data
defense <- get_defense(season, 'Team', 'Overall')

#general data
general <- get_general(season, type = 'Team', 'Base')

#hustle data
hustle <- get_hustle(21800001)


#shooting data
shooting <- get_shooting(season, 'Team', "By+Zone", "Base")


#working with tracking data
test_tracking <-
get_tracking("2020",
             measure_type = "SpeedDistance", 
             season_type = "Playoffs",
             type = "Player",
             date_from = "06/27/2021",
             date_to = "06/27/2021"
)

test_tracking %>%
  ggplot(aes(x = dist_miles)) +
  geom_density() +
  geom_rug(alpha = .3) +
  theme_bw()

#MIL v ATL distance miles defense

MIL_def_total <- test_tracking %>%
  filter(team_abbreviation == "MIL") %>%
  pull(dist_miles_def) %>%
  sum() 

ATL_def_total <- test_tracking %>%
  filter(team_abbreviation == "ATL") %>%
  pull(dist_miles_def) %>%
  sum() 

test_tracking %>%
  filter(team_abbreviation == "MIL") %>%
  pull(dist_miles_off) %>%
  sum()

ATL_off_total <- test_tracking %>%
  filter(team_abbreviation == "ATL") %>%
  pull(dist_miles_off) %>%
  sum()


test_tracking %>%
  group_by(team_abbreviation) %>%
  ggplot(aes(x = team_abbreviation, y = dist_miles_def)) +
  geom_bar(stat = "identity") +
  theme_bw()



test_tracking %>%
  #group_by(w) %>%
  ggplot(aes(x = w, y = dist_miles_def)) +
  geom_bar(stat = "identity") +
  theme_bw()

tracking2 <-
  get_tracking("2020",
               measure_type = "SpeedDistance", 
               season_type = "Playoffs",
               type = "Player",
               date_from = "06/23/2021",
               date_to = "06/27/2021"
  )

tracking2 %>%
  #group_by(w) %>%
  ggplot(aes(x = w, y = dist_miles_def)) +
  geom_bar(stat = "identity") +
  theme_bw()


tracking3 <-
  get_tracking("2020",
               measure_type = "SpeedDistance", 
               season_type = "Playoffs",
               type = "Player",
               date_from = "06/19/2021",
               date_to = "06/19/2021",
  )

tracking3 %>%
  #group_by(w) %>%
  ggplot(aes(x = w, y = dist_miles_def)) +
  geom_bar(stat = "identity") +
  theme_bw()

dates <- c("06/22/2021", "06/23/2021","06/24/2021")


for(date1 in dates) {
    
    tracking3 <-
    get_tracking("2020",
                 measure_type = "SpeedDistance", 
                 season_type = "Playoffs",
                 type = "Player",
                 date_from = date1,
                 date_to = date1
    )
   
  
  }

tracking4 <-
get_tracking("2020",
             measure_type = "SpeedDistance", 
             season_type = "Regular+Season",
             type = "Player",
             date_from = "04/22/2021",
             date_to = "04/22/2021"
  ) 

tracking4 %>%
  filter(team_abbreviation == "ATL" | team_abbreviation == "MIL") %>%
  ggplot(aes(x = w, y = dist_miles_def)) +
  geom_bar(stat = "identity") +
  theme_bw()


tracking4 %>%
  filter(team_abbreviation == "BKN" | team_abbreviation == "BOS") %>%
  ggplot(aes(x = w, y = dist_miles_def)) +
  geom_bar(stat = "identity") +
  theme_bw()


dates <- c("06/22/2021", "06/23/2021","06/24/2021")

for(month in 1:12){
  
  for(day in 1:31)
  {
    ## setting the date to scrape
    date <- paste(day, month, "2017", sep = "-")
    print(paste("Getting ",date))
    
    
    tracking3 <- 
      (get_tracking("2020", 
                    measure_type = "SpeedDistance", 
                    season_type = "Playoffs",
                    type = "Player",
                    date_from = date,
                    date_to = date)) %>%
      mutate(date = date)
    
    ## getting the date 
    #tmp <- NBAStandingsByDate(date)
    ##east <- tmp[["East"]]
    ## west <- tmp[["West"]]
    ## west <- west %>% mutate(date = date)
    ## all_for_day <- rbind(east,west)
    ## accumlate_games <- rbind(accumlate_games,all_for_day)
  }
  
  
  for(date1 in dates) {
    
    tracking3 <- 
      (get_tracking("2020",
                    measure_type = "SpeedDistance", 
                    season_type = "Playoffs",
                    type = "Player",
                    date_from = date1,
                    date_to = date1
      ))
    
  }
}


# tracking loop -----------------------------------------------------------

 



get_record_by_game <- function(season)
{
  
  ## clearing out tmp variables pre run
  accumlate_games <- c()
  west <- c()
  east <- c()
  all_for_day <- c()
  tmp <- c()
  
  ## Setting up season parse
  start <- season[1]
  finish <- season[2]
  
  ## 2016 binding the results from each day from the teams_players_stats call 
  for(month in c(10,11,12)){
    
    for(day in 1:31)
    {
      ## setting the date to scrape
      date <- paste(month, day, start, sep = "-")
      print(paste("Getting ",date))
      
      ## getting the date 
      #tmp <- NBAStandingsByDate(date)
      # east <- tmp[["East"]]
      # west <- tmp[["West"]]
      tracking <-
        get_tracking(start,
                     measure_type = "SpeedDistance", 
                     season_type = "Regular+Season",
                     type = "Player",
                     date_from = date,
                     date_to = date) 
      
      ## Checking if there was a record on that given day
      if(length(tracking) != 0)
      {
        ## adding date of standings and matching col titles 
        tracking <- tracking %>% 
          mutate("date" = date)
        
        accumlate_games <- rbind(accumlate_games,tracking)
      }
    }
    
  }
  
  ## 2017 binding the results from each day 
  for(month in 1:6){
    
    for(day in 1:31)
    {
      ### setting the date to scrape
      date <- paste(month, day, finish, sep = "-")
      print(paste("Getting ",date))
      
      ## getting the date 
      #tmp <- NBAStandingsByDate(date)
      #east <- tmp[["East"]]
      #west <- tmp[["West"]]
      tracking <-
        get_tracking(start,
                     measure_type = "SpeedDistance", 
                     season_type = "Regular+Season",
                     type = "Player",
                     date_from = date,
                     date_to = date) 
      
      ## Checking if there was a record on that given day
      if(length(tracking) != 0)
      {
        ## adding date of standings and matching col titles 
        tracking <- tracking %>% 
          mutate("date" = date)
        
        accumlate_games <- rbind(accumlate_games,tracking)
      }
    }
    
  }
  return(accumlate_games)
}


tracking2 <-
  get_tracking("2020",
               measure_type = "SpeedDistance", 
               season_type = "Playoffs",
               type = "Player",
               date_from = "06/21/2021",
               date_to = "06/21/2021"
  )

if(length(tracking2) != 0)
{
  ## adding date of standings and matching col titles 
  tracking <- tracking2 %>% 
    mutate("date" = date)
  
}

length(tracking2)
head(tracking2)

rm(season)


## second half of the season binding the results from each day 
finish <- 2017
for(month in 1:6){
  
  for(day in 1:31)
  {
    ## setting the date to scrape
    date <- paste(month, day, finish, sep = "/")
    print(paste("Getting ",date))
  }
}



  
season_2014_15 %>%
  ggplot(aes(x = dist_miles, y = avg_speed)) +
  geom_line() +
  theme_bw()


season_2014_15 %>%
  ggplot(aes(x = dist_miles)) +
  geom_density() +
  facet_wrap(~ team_abbreviation) +
  theme_bw()

library(NBAr)

accumlate_games <- c()
west <- c()
east <- c()
all_for_day <- c()
tmp <- c()

## Setting up season parse
start <- 2014
finish <- 2015

for(month in 1:6){
  
  for(day in 1:31)
  {
    Sys.sleep(1)
    ## setting the date to scrape
    date <- paste(month, day, finish, sep = "/")
    print(paste("Getting ",date))
    
    ## getting the date 
    single_day <- get_tracking(season = start, measure_type = c("Drives"),
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
