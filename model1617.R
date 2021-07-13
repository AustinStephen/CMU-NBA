


# Info about flight distance, time, direction, coordinates
travel1617 <- nba_travel(start_season = 2017,
                              end_season = 2017,
                              return_home = 3,
                              phase = "RS") #airball

# Data about back to back, first or second leg etc. 
density1617 <- nba_density(travel1617) #airball

#this data has tipoff time
schedule1617 <- get_schedule(2016) %>%
  filter(season_type == "REG") #NBAr
#main useful variable from schedule1617 is game_time_utc. Havent actually merged schedule1617 with anything yet


#Merge travel1617 and density1617 with one another. 
#Remove first games of the season because there were 15 days of rest prior
travel_and_density1617 <- merge(x = travel1617, y = density1617,
                       by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away")) 

regseason1617 <- mutate(travel_and_density1617, Win = (travel_and_density1617$"W/L" == "W")) %>%
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
flight_durations1617 = c()

for (d in regseason1617$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations1617 = c(flight_durations1617, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations1617 = c(flight_durations1617, hours)
  }
  else {
    flight_durations1617 = c(flight_durations1617, 0)
  }
}

regseason1617 <- mutate(regseason1617, flight_duration = flight_durations1617) %>%
  select(- "Flight Time")
  
team_boxscores1617 <- get_team_boxscore(2016) %>%
  subset(select = season_year:plus_minus) %>%
  mutate(Date = substr(game_date, 0, 10)) %>%
  rename(Team = team_name) %>%
  select(c(Date, game_id,Team, matchup, plus_minus)) #Patrick Chodowski


regseason1617 <- merge(x = regseason1617, y = team_boxscores1617,
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

avg_score_diffs_home1617 <- regseason1617 %>%
  filter(Visitor == FALSE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
              list(avg_score_diff_home = mean))

avg_score_diffs_away1617 <- regseason1617 %>%
  filter(Visitor == TRUE) %>%
  group_by(Team) %>%
  summarise_at(vars(score_diff),
               list(avg_score_diff_away = mean))

regseason1617 <- merge(x = regseason1617, y = avg_score_diffs_home1617, 
                           by = "Team")

regseason1617 <- merge(x = regseason1617, y = avg_score_diffs_away1617, 
                       by = "Team") %>%
  arrange(Date)

#Adjusted_score_diffs accounts for if the team was home or away
adj_score_diffs1617 = c()

for (i in c(1:nrow(regseason1617))) {
  if (regseason1617$Visitor[i] == TRUE) { #if they are visitor
    adj_score_diffs1617 = c(adj_score_diffs1617, 
                        regseason1617$score_diff[i] - regseason1617$avg_score_diff_away[i])
  }
  else { #if they are home
    adj_score_diffs1617 = c(adj_score_diffs1617, 
                        regseason1617$score_diff[i] - regseason1617$avg_score_diff_home[i])
  }
}

regseason1617 <- regseason1617 %>%
  mutate(adjusted_score_diff = adj_score_diffs1617)

dtd_records1617 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/record_by_day_2016_17_season.csv")

dtd_records1617$Team <- stringr::str_replace(dtd_records1617$team, '\\*', '') 



dtd_records1617 <- select(dtd_records1617, c(date, Team, w, l, w_lpercent, ps_g, pa_g)) %>%
  rename(Date = date)

typeof(regseason1617$Date[1])
typeof(dtd_records1617$Date[1])

regseason1617 <- merge(x= regseason1617, y = dtd_records1617,
                       by = c("Date", "Team"))

opp_win_records1617 <- c()
for (i in c(1:nrow(regseason1617))) {
  opp <- regseason1617$Opponent[i]
  day <- regseason1617$Date[i]
  opp_record <- filter(dtd_records1617, 
                       (Team == opp & Date == day))$w_lpercent[1]
  opp_win_records1617 <- c(opp_win_records1617, opp_record)
}

regseason1617 <- regseason1617 %>%
  mutate(opp_win_percent = opp_win_records1617)

regseason1617 <- regseason1617 %>%
  mutate(win_percent_diff = w_lpercent - opp_win_percent)

ratings1617 <- get_general(
  season = 2016,
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

regseason1617 <- merge(x = regseason1617, y = ratings1617,
                       by = "Team")

regseason1617 <- merge(x = regseason1617, y = ratings1617,
                       by.x = "Opponent", by.y = "Team")

regseason1617 <- regseason1617 %>%
  rename(off_rating = "off_rating.x") %>%
  rename(def_rating = "def_rating.x") %>%
  rename(net_rating = "net_rating.x") %>%
  rename(pace = "pace.x") %>%
  rename(opp_off_rating = "off_rating.y") %>%
  rename(opp_def_rating = "def_rating.y") %>%
  rename(opp_net_rating = "net_rating.y") %>%
  rename(opp_pace = "pace.y") 

regseason1617 <- regseason1617 %>%
  mutate(net_rating_diff = net_rating - opp_net_rating) %>%
  mutate(pace_diff = pace - opp_pace)
  

performance1617 <- get_general(
  season = 2013,
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

regseason1617 <- merge(regseason1617, performance1617,
                       by = "Team")

opp_performance1617 <- performance1617 %>%
  select(c(Team, fg_pct, fg3m, fg3_pct, ftm, ft_pct, reb, ast, tov, stl, blk))

regseason1617 <- merge(x = regseason1617, y = opp_performance1617,
                       by.x = "Opponent", by.y = "Team")

regseason1617 <- regseason1617 %>%
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

write_csv(regseason1617, "/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1617.csv")

data1617 <- read_csv("/Users/matthewyep/Desktop/Carnegie Mellon/CMU-NBA/data/regseason1617.csv")


#keep track of cumulative travel 

visitors <- filter(data1617, Visitor == TRUE)

test_rest_visitors <- select(visitors, c(Rest, net_rating_diff)) %>%
  group_by(Rest) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_adj_score_diff = mean))
#interesting to see that teams do well with 1-2 days rest but poor after more than that
# huge dip after 7 days rest. Likely due to small sample size

test_shift_visitors <- select(visitors, c(shift, net_rating_diff)) %>%
  group_by(shift) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_adj_score_diff = mean))
#Nothing convincing here. Might need more seasons of data

test_b2b_visitors <- select(visitors, c(three_in_four, net_rating_diff)) %>%
  group_by(three_in_four) %>%
  summarise_at(vars(net_rating_diff),
               list(avg_adj_score_diff = mean))

visitors_lm <- lm(adjusted_score_diff ~ w_lpercent + opp_win_percent + three_in_four + traveling_west, 
                  data = visitors)

summary(visitors_lm)

# Distance flight_duration  Rest 
# shift   b2b_2nd   three_in_four   Visitor
# traveling_west  traveling_east
# w_lpercent opp_win_percent

#to do: opponent current record

library(ggfortify)
autoplot(visitors_lm) +
  theme_bw()

ggplot(visitors, aes(x = win_percent_diff, y= adjusted_score_diff)) +
  geom_point(alpha = 0.5)


#Traveling west (east coast teams flying west) has a negative influence on adj_score_diff
test_west_visitor <- select(visitors, c(traveling_west, adjusted_score_diff)) %>%
  group_by(traveling_west) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))

#In contrast, traveling east is not that bad for west coast teams
test_east_visitor <- select(visitors, c(traveling_east, adjusted_score_diff)) %>%
  group_by(traveling_east) %>%
  summarise_at(vars(adjusted_score_diff),
               list(avg_adj_score_diff = mean))

#ask ron at OH how we can test the significance of these results
#east vs west is really interesting

#other things to add ot model
#opponent days rest
#opponent record
#opponent avg score diff
#expected outcome T or F







  
