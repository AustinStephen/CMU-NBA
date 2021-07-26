# Info about flight distance, time, direction, coordinates
travel1415 <- nba_travel(start_season = 2015,
                         end_season = 2015,
                         return_home = 3,
                         phase = "RS") #airball
# Data about back to back, first or second leg etc. 
density1415 <- nba_density(travel1415) #airball


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density1415 <- merge(x = travel1415, y = density1415,
                                by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away"))

regseason1415 <- mutate(travel_and_density1415, Win = (travel_and_density1415$"W/L" == "W")) %>%
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
flight_durations1415 = c()

for (d in regseason1415$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations1415 = c(flight_durations1415, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations1415 = c(flight_durations1415, hours)
  }
  else {
    flight_durations1415 = c(flight_durations1415, 0)
  }
}

regseason1415 <- mutate(regseason1415, flight_duration = flight_durations1415) %>%
  select(- "Flight Time")

team_boxscores1415 <- get_team_boxscore(2014) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus)) 


regseason1415 <- merge(x = regseason1415, y = team_boxscores1415,
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

avg_score_diffs_home1415 <- regseason1415 %>%
  filter(Visitor == FALSE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_home = mean))

avg_score_diffs_away1415 <- regseason1415 %>%
  filter(Visitor == TRUE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_away = mean))

regseason1415 <- merge(x = regseason1415, y = avg_score_diffs_home1415, 
                       by = "Team")

regseason1415 <- merge(x = regseason1415, y = avg_score_diffs_away1415, 
                       by = "Team") %>%
  arrange(Date)


#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs1415 = c()

for (i in c(1:nrow(regseason1415))) {
  if (regseason1415$Visitor[i] == TRUE) { #if they are visitor
    adj_score_diffs1415 = c(adj_score_diffs1415, 
                            regseason1415$score_diff[i] - regseason1415$avg_score_diff_away[i])
  }
  else { #if they are home
    adj_score_diffs1415 = c(adj_score_diffs1415, 
                            regseason1415$score_diff[i] - regseason1415$avg_score_diff_home[i])
  }
}

regseason1415 <- regseason1415 %>%
  mutate(adjusted_score_diff = adj_score_diffs1415)

dtd_records1415 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/record_by_day_2014_15_season.csv")

dtd_records1415$Team <- stringr::str_replace(dtd_records1415$team, '\\*', '') 

dtd_records1415 <- select(dtd_records1415, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

regseason1415 <- merge(x= regseason1415, y = dtd_records1415,
                       by = c("Date", "Team")) 

opp_win_records1415 <- c()
for (i in c(1:nrow(regseason1415))) {
  opp <- regseason1415$Opponent[i]
  day <- regseason1415$Date[i]
  opp_record <- filter(dtd_records1415, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records1415 <- c(opp_win_records1415, opp_record)
}

regseason1415 <- regseason1415 %>%
  mutate(opp_win_percent = opp_win_records1415)

regseason1415 <- regseason1415 %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

write_csv(regseason1415, "/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/timesaver/temp1415.csv")

regseason1415 <- read_csv("/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/timesaver/temp1415.csv")

ratings1415 <- get_general(
  season = 2014,
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

regseason1415 <- merge(x = regseason1415, y = ratings1415,
                       by = "Team")

regseason1415 <- merge(x = regseason1415, y = ratings1415,
                       by.x = "Opponent", by.y = "Team")

regseason1415 <- regseason1415 %>%
  rename(off_rating = "off_rating.x") %>%
  rename(def_rating = "def_rating.x") %>%
  rename(net_rating = "net_rating.x") %>%
  rename(pace = "pace.x") %>%
  rename(opp_off_rating = "off_rating.y") %>%
  rename(opp_def_rating = "def_rating.y") %>%
  rename(opp_net_rating = "net_rating.y") %>%
  rename(opp_pace = "pace.y") 

regseason1415 <- regseason1415 %>%
  mutate(net_rating_diff = net_rating - opp_net_rating) %>%
  mutate(pace_diff = pace - opp_pace)

performance1415 <- get_general(
  season = 2014,
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

regseason1415 <- merge(regseason1415, performance1415,
                       by = "Team")

opp_performance1415 <- performance1415 %>%
  select(c(Team, fgm,fg_pct, fg3m, fg3_pct, ftm, ft_pct, reb, ast, tov, stl, blk))

regseason1415 <- merge(x = regseason1415, y = opp_performance1415,
                       by.x = "Opponent", by.y = "Team")

regseason1415 <- regseason1415 %>%
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

write_csv(regseason1415, "/Users/matthewyep/Desktop/CarnegieMellon/CMU-NBA/matthew_data/regseason1415.csv")


  



