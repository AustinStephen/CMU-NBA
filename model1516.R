

# Info about flight distance, time, direction, coordinates
travel1516 <- nba_travel(start_season = 2016,
                         end_season = 2016,
                         return_home = 3,
                         phase = "RS") #airball

# Data about back to back, first or second leg etc. 
density1516 <- nba_density(travel1516) #airball


#Merge travel1516 and density1516 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density1516 <- merge(x = travel1516, y = density1516,
                                by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away"))

regseason1516 <- mutate(travel_and_density1516, Win = (travel_and_density1516$"W/L" == "W")) %>%
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
flight_durations1516 = c()

for (d in regseason1516$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations1516 = c(flight_durations1516, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations1516 = c(flight_durations1516, hours)
  }
  else {
    flight_durations1516 = c(flight_durations1516, 0)
  }
}

regseason1516 <- mutate(regseason1516, flight_duration = flight_durations1516) %>%
  select(- "Flight Time")

team_boxscores1516 <- get_team_boxscore(2015) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus)) 


regseason1516 <- merge(x = regseason1516, y = team_boxscores1516,
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

avg_score_diffs_home1516 <- regseason1516 %>%
  filter(Visitor == FALSE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_home = mean))

avg_score_diffs_away1516 <- regseason1516 %>%
  filter(Visitor == TRUE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_away = mean))

regseason1516 <- merge(x = regseason1516, y = avg_score_diffs_home1516, 
                       by = "Team")

regseason1516 <- merge(x = regseason1516, y = avg_score_diffs_away1516, 
                       by = "Team") %>%
  arrange(Date)


#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs1516 = c()

for (i in c(1:nrow(regseason1516))) {
  if (regseason1516$Visitor[i] == TRUE) { #if they are visitor
    adj_score_diffs1516 = c(adj_score_diffs1516, 
                            regseason1516$score_diff[i] - regseason1516$avg_score_diff_away[i])
  }
  else { #if they are home
    adj_score_diffs1516 = c(adj_score_diffs1516, 
                            regseason1516$score_diff[i] - regseason1516$avg_score_diff_home[i])
  }
}

regseason1516 <- regseason1516 %>%
  mutate(adjusted_score_diff = adj_score_diffs1516)

dtd_records1516 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/record_by_day_2015_16_season.csv")

dtd_records1516$Team <- stringr::str_replace(dtd_records1516$team, '\\*', '') 

dtd_records1516 <- select(dtd_records1516, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

regseason1516 <- merge(x= regseason1516, y = dtd_records1516,
                       by = c("Date", "Team")) 

opp_win_records1516 <- c()
for (i in c(1:nrow(regseason1516))) {
  opp <- regseason1516$Opponent[i]
  day <- regseason1516$Date[i]
  opp_record <- filter(dtd_records1516, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records1516 <- c(opp_win_records1516, opp_record)
}

regseason1516 <- regseason1516 %>%
  mutate(opp_win_percent = opp_win_records1516)

regseason1516 <- regseason1516 %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

write_csv(regseason1516, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/timesaver/temp1516.csv")

regseason1516 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/timesaver/temp1516.csv")

ratings1516 <- get_general(
  season = 2015,
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

regseason1516 <- merge(x = regseason1516, y = ratings1516,
                       by = "Team")

regseason1516 <- merge(x = regseason1516, y = ratings1516,
                       by.x = "Opponent", by.y = "Team")

regseason1516 <- regseason1516 %>%
  rename(off_rating = "off_rating.x") %>%
  rename(def_rating = "def_rating.x") %>%
  rename(net_rating = "net_rating.x") %>%
  rename(pace = "pace.x") %>%
  rename(opp_off_rating = "off_rating.y") %>%
  rename(opp_def_rating = "def_rating.y") %>%
  rename(opp_net_rating = "net_rating.y") %>%
  rename(opp_pace = "pace.y") 

regseason1516 <- regseason1516 %>%
  mutate(net_rating_diff = net_rating - opp_net_rating) %>%
  mutate(pace_diff = pace - opp_pace)

performance1516 <- get_general(
  season = 2015,
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

regseason1516 <- merge(regseason1516, performance1516,
                       by = "Team")

opp_performance1516 <- performance1516 %>%
  select(c(Team, fgm,fg_pct, fg3m, fg3_pct, ftm, ft_pct, reb, ast, tov, stl, blk))

regseason1516 <- merge(x = regseason1516, y = opp_performance1516,
                       by.x = "Opponent", by.y = "Team")

regseason1516 <- regseason1516 %>%
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

write_csv(regseason1516, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1516.csv")



