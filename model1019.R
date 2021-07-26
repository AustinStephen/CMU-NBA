library(devtools)
library(airball)
library(nbastatR)
library(tidyverse)
library(ggplot2)
library(echarts4r)
library(echarts4r.assets) 
library(NBAr)
library(lubridate)
library(ballr)
library(dplyr)
library(tidyverse)
library(GGally)

data1011 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1011.csv")
data1112 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1112.csv")
data1213 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1213.csv")
data1314 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1314.csv")
data1415 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1415.csv")
data1516 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1516.csv")
data1617 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1617.csv")
data1718 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1718.csv")
data1819 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1819.csv")
#data_bubble <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason_bubble.csv")

together <- rbind(data1011, data1112)
together <- rbind(together, data1213)
together <- rbind(together, data1314)
together <- rbind(together, data1415)
together <- rbind(together, data1516)
together <- rbind(together, data1617)
together <- rbind(together, data1718)
together <- rbind(together, data1819)
#together <- rbind(together, data_bubble)


capped_score_diffs <- c() 
for (i in c(1:nrow(together))) {
  sd = together$score_diff[i]
  if (sd < (-30)) {
    sd = -30
  }
  else if (sd > 30) {
    sd = 30
  }
  capped_score_diffs <- c(capped_score_diffs, sd)
}

together <- together %>%
  mutate(capped_score_diff = capped_score_diffs)

together <- together %>%
  select(-c('gp', "w.y", "l.y", 'min', "w_pct", "plus_minus":"cfparams"))

together <- together %>% 
  mutate(off_rating_diff = off_rating - opp_off_rating) %>%
  mutate(def_rating_diff = def_rating - opp_def_rating) %>%
  mutate(fgm_diff = fgm - opp_fgm) %>%
  mutate(fg_pct_diff = fg_pct - opp_fg_pct) %>%
  mutate(fg3m_diff = fg3m - opp_fg3m) %>%
  mutate(fg3_pct_diff = fg3_pct - opp_fg3_pct) %>%
  mutate(ftm_diff = ftm - opp_ftm) %>%
  mutate(ft_pct_diff = ft_pct - opp_ft_pct) %>%
  mutate(reb_diff = reb - opp_reb) %>%
  mutate(ast_diff = ast- opp_ast) %>%
  mutate(tov_diff = tov - opp_tov) %>%
  mutate(stl_diff = stl - opp_stl) %>%
  mutate(blk_diff = blk - opp_blk) %>%
  mutate(win_percent_diff = win_percent_diff *100)

gameratings1011 <- get_team_boxscore(2010, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1112 <- get_team_boxscore(2011, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1213 <- get_team_boxscore(2012, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1314 <- get_team_boxscore(2013, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1415 <- get_team_boxscore(2014, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1516 <- get_team_boxscore(2015, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1617 <- get_team_boxscore(2016, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1718 <- get_team_boxscore(2017, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
gameratings1819 <- get_team_boxscore(2018, measure_type = "Advanced") %>%
  select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))
#gameratings_bubble <- get_team_boxscore(2019, measure_type = "Advanced") %>%
  #select(c("team_name", "game_id", "off_rating", "def_rating", "net_rating"))

gameratings_together <-rbind(gameratings1011, gameratings1112)
gameratings_together <-rbind(gameratings_together, gameratings1213)
gameratings_together <-rbind(gameratings_together, gameratings1314)
gameratings_together <-rbind(gameratings_together, gameratings1415)
gameratings_together <-rbind(gameratings_together, gameratings1516)
gameratings_together <-rbind(gameratings_together, gameratings1617)
gameratings_together <-rbind(gameratings_together, gameratings1718)
gameratings_together <-rbind(gameratings_together, gameratings1819)
#gameratings_together <-rbind(gameratings_together, gameratings_bubble)


gameratings_together <- gameratings_together %>%
  rename(Team = "team_name", game_off_rating = "off_rating", game_def_rating = "def_rating",
         game_net_rating = net_rating)

together <- merge(x= together, y = gameratings_together,
                  by = c("game_id", "Team"))

travel <- nba_travel(start_season = 2011,
                     end_season = 2019,
                     return_home = 3,
                     phase = "RS") 

opp_rest <- travel %>%
  select("Date", "Team", "Rest") %>%
  rename(opp_rest = "Rest") 

together <- merge(x = together, y = opp_rest,
                  by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together <- together %>%
  mutate(rest_diff = Rest - opp_rest) %>%
  select(-"Season.y") %>%
  rename(season = "Season.x")


together<- together %>%
  mutate(rest_advantage = rest_diff > 0) %>%
  mutate(travel_3_hours_back = (shift ==  -3)) %>%
  mutate(travel_3_hours_forward = (shift ==  3))

distance_windows <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/together_5dayWindow_update.csv")
together <- distance_windows[!duplicated(distance_windows),]

together <- together %>%
  mutate(distance_diff = Distance - opp_distance) %>%
  mutate(windowed_distance_diff = dist_5dayWindow - opp_dist_5dayWindow) %>%
  mutate(distance_advantage = (distance_diff < 0)) %>%
  mutate(windowed_distance_advantage = (windowed_distance_diff) < 0)


together <- together %>%
  select(-c(fgm:opp_blk)) %>%
  select(-c(fgm_diff:blk_diff))

net_rating_grace <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/together_net_Rating_update.csv") %>%
  select(c(Date, Team, net_rating_5gameWindow, opp_net_rating_5gameWindow))

together <- merge(together, net_rating_grace,
                  by = c("Date", "Team")) %>%
  mutate(net_rating_5gameWindow_diff = net_rating_5gameWindow - opp_net_rating_5gameWindow) %>%
  mutate(hours_shift = as.factor(shift))

visitors <- filter(together, Visitor == TRUE)
home <- filter(together, Visitor == FALSE)
subset_home <- select(home, c(game_id, shift, b2b_2nd, three_in_four, four_in_five, five_in_seven, travel_3_hours_back, travel_3_hours_forward))
together <- left_join(visitors, subset_home, by = "game_id", suffix = c("_vis", "_home"))
together <- together %>%
  rename(visitor = "Team",
         home = "Opponent",
         rest_vis = "Rest",
         distance_vis = "Distance",
         flight_duration_vis = "flight_duration",
         wins_vis = "w.x",
         loss_vis = "l.x",
         w_lpercent_vis = "w_lpercent",
         ps_g_vis = "ps_g",
         pa_g_vis = "pa_g",
         w_lpercent_home = "opp_win_percent",
         off_rating_vis = "off_rating",
         def_rating_vis = "def_rating",
         net_rating_vis = "net_rating",
         pace_vis = "pace",
         off_rating_home = "opp_off_rating",
         def_rating_home = "opp_def_rating",
         net_rating_home = "opp_net_rating",
         pace_home = "opp_pace",
         rest_home = "opp_rest",
         distance_home = "opp_distance",
         dist_5dayWindow_vis = "dist_5dayWindow",
         dist_5dayWindow_home = "opp_dist_5dayWindow",
         net_rating_5gameWindow_vis = "net_rating_5gameWindow",
         net_rating_5gameWindow_home = "opp_net_rating_5gameWindow",
  ) %>%
  select(-c("Visitor", "avg_score_diff_home", "avg_score_diff_away", "B2B", "b2b_1st"))

write_csv(together, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/together.csv")



strength_proxies <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/clean_team_strength_columns.csv") %>%
  select(c(Date, Team, games_played, win_percent_diff, hollow_wind60_cent, hollow_wind50_cent,hollow_wind40_cent, hollow_wind30_cent, hollow_wind20_cent, wind_10_center,wind_40_hollow, wind_35_hollow, wind_30_hollow, wind_25_hollow, wind_20_hollow, wind_15_hollow, wind_10_hollow_5,wind_5))

together <- merge(together, strength_proxies,
                  by.x = c("Date", "visitor"), by.y = c("Date","Team"))

together <- together %>%
  rename(g1_5_rolling_net_diff = wind_5,
         g6_10_rolling_net_diff = wind_10_hollow_5,
         g11_15_rolling_net_diff = wind_15_hollow,
         g16_20_rolling_net_diff = wind_20_hollow,
         g21_25_rolling_net_diff = wind_25_hollow,
         g26_30_rolling_net_diff = wind_30_hollow,
         g31_35_rolling_net_diff = wind_35_hollow,
         g36_40_rolling_net_diff = wind_40_hollow,
         g1_5_centered_rolling_net_diff= wind_10_center,
         g6_10_centered_rolling_net_diff= hollow_wind20_cent,
         g11_15_centered_rolling_net_diff= hollow_wind30_cent,
         g16_20_centered_rolling_net_diff= hollow_wind40_cent,
         g21_25_centered_rolling_net_diff= hollow_wind50_cent,
         g26_30_centered_rolling_net_diff= hollow_wind60_cent)

together <- together %>%
  rename(win_percent_diff = "win_percent_diff.y") %>%
  select(-c("win_percent_diff.x"))
         

write_csv(together, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/together.csv")

together <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/together.csv")


library(car)
vif(travel_visitors_lm)


