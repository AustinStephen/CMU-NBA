

# Info about flight distance, time, direction, coordinates
travel1213 <- nba_travel(start_season = 2013,
                         end_season = 2013,
                         return_home = 3,
                         phase = "RS") #airball

# Data about back to back, first or second leg etc. 
density1213 <- nba_density(travel1213) #airball


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density1213 <- merge(x = travel1213, y = density1213,
                                by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away")) %>%
  filter(Rest != 15)

regseason1213 <- mutate(travel_and_density1213, Win = (travel_and_density1213$"W/L" == "W")) %>%
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
flight_durations1213 = c()

for (d in regseason1213$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations1213 = c(flight_durations1213, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations1213 = c(flight_durations1213, hours)
  }
  else {
    flight_durations1213 = c(flight_durations1213, 0)
  }
}

regseason1213 <- mutate(regseason1213, flight_duration = flight_durations1213) %>%
  select(- "Flight Time")

team_boxscores1213 <- get_team_boxscore(2012) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus)) 


regseason1213 <- merge(x = regseason1213, y = team_boxscores1213,
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

avg_score_diffs_home1213 <- regseason1213 %>%
  filter(Visitor == FALSE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_home = mean))

avg_score_diffs_away1213 <- regseason1213 %>%
  filter(Visitor == TRUE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_away = mean))

regseason1213 <- merge(x = regseason1213, y = avg_score_diffs_home1213, 
                       by = "Team")

regseason1213 <- merge(x = regseason1213, y = avg_score_diffs_away1213, 
                       by = "Team") %>%
  arrange(Date)


#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs1213 = c()

for (i in c(1:nrow(regseason1213))) {
  if (regseason1213$Visitor[i] == TRUE) { #if they are visitor
    adj_score_diffs1213 = c(adj_score_diffs1213, 
                            regseason1213$score_diff[i] - regseason1213$avg_score_diff_away[i])
  }
  else { #if they are home
    adj_score_diffs1213 = c(adj_score_diffs1213, 
                            regseason1213$score_diff[i] - regseason1213$avg_score_diff_home[i])
  }
}

regseason1213 <- regseason1213 %>%
  mutate(adjusted_score_diff = adj_score_diffs1213)

dtd_records1213 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/record_by_day_2012_13_season.csv")

dtd_records1213$Team <- stringr::str_replace(dtd_records1213$team, '\\*', '') 

dtd_records1213 <- select(dtd_records1213, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

regseason1213 <- merge(x= regseason1213, y = dtd_records1213,
                       by = c("Date", "Team")) %>%
  filter(Rest <= 3)

opp_win_records1213 <- c()
for (i in c(1:nrow(regseason1213))) {
  opp <- regseason1213$Opponent[i]
  day <- regseason1213$Date[i]
  opp_record <- filter(dtd_records1213, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records1213 <- c(opp_win_records1213, opp_record)
}

regseason1213 <- regseason1213 %>%
  mutate(opp_win_percent = opp_win_records1213)

regseason1213 <- regseason1213 %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

ratings1213 <- get_general(
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

regseason1213 <- merge(x = regseason1213, y = ratings1213,
                       by = "Team")

regseason1213 <- merge(x = regseason1213, y = ratings1213,
                       by.x = "Opponent", by.y = "Team")

regseason1213 <- regseason1213 %>%
  rename(off_rating = "off_rating.x") %>%
  rename(def_rating = "def_rating.x") %>%
  rename(net_rating = "net_rating.x") %>%
  rename(pace = "pace.x") %>%
  rename(opp_off_rating = "off_rating.y") %>%
  rename(opp_def_rating = "def_rating.y") %>%
  rename(opp_net_rating = "net_rating.y") %>%
  rename(opp_pace = "pace.y") 

regseason1213 <- regseason1213 %>%
  mutate(net_rating_diff = net_rating - opp_net_rating) %>%
  mutate(pace_diff = pace - opp_pace)


write_csv(regseason1213, "/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1213.csv")





