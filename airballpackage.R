library(tidyverse)
library(airball)
library(nbastatR)
library(ggplot2)
library(nbastatR)
library(NBAr)
install.packages("devtools")
library(devtools)
install_github("rtelmore/ballr")
library(ballr)

data <- nba_travel(start_season = 2015, end_season = 2019)
density <- nba_density(df = data)

travel_and_density <- merge(x = data, y = density,
                            by = c("Date", "Team"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  select_at(vars(-starts_with("d."))) %>%
  mutate(Visitor = (Location == "Away")) %>%
  filter(Rest != 15)

data_2015_2019 <- mutate(travel_and_density, Win = (travel_and_density$"W/L" == "W")) %>%
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

flight_durations = c()
for (d in data_2015_2019$"Flight Time") {
  second_to_last = substr(d, nchar(d) - 1, nchar(d)-1)
  if (second_to_last == "e") { #"~55 minutes"
    flight_durations = c(flight_durations, 1) 
  }
  else if (second_to_last == "r") {
    hours = as.double(substr(d, 2, 5))
    flight_durations = c(flight_durations, hours)
  }
  else {
    flight_durations = c(flight_durations, 0)
  }
}

data1_2015_2019 <- mutate(data_2015_2019, flight_duration = flight_durations) %>%
  select(- "Flight Time")

#EDA with this data

data1_2015_2019 %>%
  filter(b2b_2nd == "Yes") %>%
  ggplot(aes(x = Team, y = Distance)) +
  geom_bar(stat = "Identity") +
  facet_wrap(~ Win) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


data1_2015_2019 %>%
  filter(b2b_2nd == "Yes") %>%
  ggplot(aes(x = Team, y = Distance)) +
  geom_bar(stat = "Identity") +
  facet_wrap(~ Win) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


data1_2015_2019 %>%
  filter(b2b_2nd == "Yes" & Win == TRUE) %>%
  group_by(Team) %>%
  mutate(number_of_win = length(Win)) %>%
  ggplot(aes(x = number_of_win)) +
  geom_density() +
  facet_wrap(~ Team)
  theme_bw()
  
data1_2015_2019 %>%
  filter(b2b_2nd == "Yes" & Win == TRUE & Visitor == TRUE) %>%
  group_by(Team) %>%
  #mutate(number_of_win = length(Win)) %>%
  ggplot(aes(x = Distance)) +
  geom_density() +
  facet_wrap(~ Team)
  theme_bw()

data1_2015_2019 %>%
  filter(b2b_2nd == "Yes") %>%
  ggplot(aes(x = Team, y = Distance)) +
  geom_histogram(stat = "Identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# working with ballr
s <-  NBAStandingsByDate("2010-01-31")




