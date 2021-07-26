

# Info about flight distance, time, direction, coordinates
travel_bubble <- nba_travel(start_season = 2020,
                         end_season = 2020,
                         return_home = 3,
                         phase = "RS") #airball

# Data about back to back, first or second leg etc. 
density_bubble <- nba_density(travel_bubble) #airball


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density_bubble <- merge(x = travel_bubble, y = density_bubble,
                                by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away"))

regseason_bubble <- mutate(travel_and_density_bubble, Win = (travel_and_density_bubble$"W/L" == "W")) %>%
  select(c("Season", "Month", "Week", "Date", "Team", "Opponent", "Visitor", "City", "Win", 
           "Distance", "Route", "Rest", "TZ", "Shift (hrs)", "Flight Time", "Direction (E/W)", 
           "Latitude", "Longitude", "B2B", "B2B-1st", "B2B-2nd", "3in4", "4in5", "5in7")) %>%
  rename(b2b_1st = "B2B-2nd") %>%
  rename(b2b_2nd = "B2B-1st") %>%
  rename(direction = "Direction (E/W)") %>%
  rename(shift = "Shift (hrs)") %>%
  rename(three_in_four = "3in4") %>%
  rename(four_in_five = "4in5") %>%
  rename(five_in_seven = "5in7")

#The given flight durations are funky and have strings but I just wanna extra the time
flight_durations_bubble = c()

for (d in regseason_bubble$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations_bubble = c(flight_durations_bubble, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations_bubble = c(flight_durations_bubble, hours)
  }
  else {
    flight_durations_bubble = c(flight_durations_bubble, 0)
  }
}

regseason_bubble <- mutate(regseason_bubble, flight_duration = flight_durations_bubble) %>%
  select(- "Flight Time")

team_boxscores_bubble <- get_team_boxscore(2019) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus)) 


regseason_bubble <- merge(x = regseason_bubble, y = team_boxscores_bubble,
                       by = c("Date", "Team")) %>%
  rename(score_diff = plus_minus) %>%
  mutate(traveling_west = (direction == "West")) %>% 
  mutate(traveling_east = (direction == "East")) %>%
  mutate(B2B = (B2B == "Yes")) %>%
  mutate(b2b_1st = (b2b_1st == "Yes")) %>%
  mutate(b2b_2nd = (b2b_2nd == "Yes")) %>%
  mutate(three_in_four = (three_in_four == "Yes")) %>%
  mutate(four_in_five = (four_in_five == "Yes")) %>%
  mutate(five_in_seven = (five_in_seven == "Yes"))

avg_score_diffs_home_bubble <- regseason_bubble %>%
  filter(Visitor == FALSE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_home = mean))

avg_score_diffs_away_bubble <- regseason_bubble %>%
  filter(Visitor == TRUE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_away = mean))

regseason_bubble <- merge(x = regseason_bubble, y = avg_score_diffs_home_bubble, 
                       by = "Team")

regseason_bubble <- merge(x = regseason_bubble, y = avg_score_diffs_away_bubble, 
                       by = "Team") %>%
  arrange(Date)


#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs_bubble = c()

for (i in c(1:nrow(regseason_bubble))) {
  if (regseason_bubble$Visitor[i] == TRUE) { #if they are visitor
    adj_score_diffs_bubble = c(adj_score_diffs_bubble, 
                            regseason_bubble$score_diff[i] - regseason_bubble$avg_score_diff_away[i])
  }
  else { #if they are home
    adj_score_diffs_bubble = c(adj_score_diffs_bubble, 
                            regseason_bubble$score_diff[i] - regseason_bubble$avg_score_diff_home[i])
  }
}

regseason_bubble <- regseason_bubble %>%
  mutate(adjusted_score_diff = adj_score_diffs_bubble)

dtd_records_bubble <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/record_by_day_2019_20_season.csv")

dtd_records_bubble$Team <- stringr::str_replace(dtd_records_bubble$team, '\\*', '') 

dtd_records_bubble <- select(dtd_records_bubble, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

regseason_bubble <- merge(x= regseason_bubble, y = dtd_records_bubble,
                       by = c("Date", "Team"))

opp_win_records_bubble <- c()
for (i in c(1:nrow(regseason_bubble))) {
  opp <- regseason_bubble$Opponent[i]
  day <- regseason_bubble$Date[i]
  opp_record <- filter(dtd_records_bubble, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records_bubble <- c(opp_win_records_bubble, opp_record)
}

regseason_bubble <- regseason_bubble %>%
  mutate(opp_win_percent = opp_win_records_bubble)

regseason_bubble <- regseason_bubble %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

write_csv(regseason_bubble, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/timesaver/temp_bubble.csv")

regseason_bubble <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/timesaver/temp_bubble.csv")

ratings_bubble <- get_general(
  season = 2019,
  type = "Team",
  measure_type = "Advanced",
  per_mode = "Totals",
  season_type = "Regular+Season",
  season_segment = "",
  game_segment = "",
  date_from = "",
  date_to = "",
  outcome = "",
  period = "0",
  opponent_team_id = "0",
  team_id = "0",
  verbose = TRUE
) %>%  
  select(c("team_name", "off_rating", "def_rating", "net_rating", "pace")) %>%
  rename(Team = team_name)

regseason_bubble <- merge(x = regseason_bubble, y = ratings_bubble,
                       by = "Team")

regseason_bubble <- merge(x = regseason_bubble, y = ratings_bubble,
                       by.x = "Opponent", by.y = "Team")

regseason_bubble <- regseason_bubble %>%
  rename(off_rating = "off_rating.x") %>%
  rename(def_rating = "def_rating.x") %>%
  rename(net_rating = "net_rating.x") %>%
  rename(pace = "pace.x") %>%
  rename(opp_off_rating = "off_rating.y") %>%
  rename(opp_def_rating = "def_rating.y") %>%
  rename(opp_net_rating = "net_rating.y") %>%
  rename(opp_pace = "pace.y") 

regseason_bubble <- regseason_bubble %>%
  mutate(net_rating_diff = net_rating - opp_net_rating) %>%
  mutate(pace_diff = pace - opp_pace)

performance_bubble <- get_general(
  season = 2019,
  type = "Team",
  measure_type = "Base",
  per_mode = "PerGame",
  season_type = "Regular+Season",
  season_segment = "",
  game_segment = "",
  date_from = "",
  date_to = "",
  outcome = "",
  period = "0",
  opponent_team_id = "0",
  team_id = "0",
  verbose = TRUE) %>%
  rename(Team = team_name)


regseason_bubble <- merge(regseason_bubble, performance_bubble,
                       by = "Team")

opp_performance_bubble <- performance_bubble %>%
  select(c(Team,fgm,fg_pct, fg3m, fg3_pct, ftm, ft_pct, reb, ast, tov, stl, blk))

regseason_bubble <- merge(x = regseason_bubble, y = opp_performance_bubble,
                       by.x = "Opponent", by.y = "Team")

regseason_bubble <- regseason_bubble %>%
  rename(fgm = "fgm.x") %>%
  rename(fg_pct = "fg_pct.x") %>%
  rename(fg3m = "fg3m.x") %>%
  rename(fg3_pct = "fg3_pct.x") %>%
  rename(ftm = "ftm.x") %>%
  rename(ft_pct = "ft_pct.x") %>%
  rename(reb = "reb.x") %>%
  rename(ast = "ast.x") %>%
  rename(tov = "tov.x")  %>%
  rename(stl = "stl.x") %>%
  rename(blk = "blk.x")  %>%
  rename(opp_fgm = "fgm.y") %>%
  rename(opp_fg_pct = "fg_pct.y") %>%
  rename(opp_fg3m = "fg3m.y") %>%
  rename(opp_fg3_pct = "fg3_pct.y") %>%
  rename(opp_ftm = "ftm.y") %>%
  rename(opp_ft_pct = "ft_pct.y") %>%
  rename(opp_reb = "reb.y") %>%
  rename(opp_ast = "ast.y") %>%
  rename(opp_tov = "tov.y") %>%
  rename(opp_stl = "stl.y") %>%
  rename(opp_blk = "blk.y")

regseason_bubble <- regseason_bubble %>%
  filter(Date > "2020-07-29")

write_csv(regseason_bubble, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason_bubble.csv")




