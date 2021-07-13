

# Day by day tracking data ----------------------------------------------

## Pass the function a vector with the start and end years of the season 
## ex. c(2016,2017)

distance_by_day <- function(season){
  
  ## clearing out tmp variables pre run
  accumlate_games <- c()
  west <- c()
  east <- c()
  all_for_day <- c()
  tmp <- c()
  
  ## Setting up season parse
  start <- season[1]
  finish <- season[2]
  
  ## first half of the season binding the results from each day 
  
  for(month in c(10,11,12)){
    
    for(day in 1:31)
    {
      Sys.sleep(1)
      ## setting the date to scrape
      date <- paste(month, day, start, sep = "/")
      print(paste("Getting ",date))
      
      ## getting the date 
      single_day <- get_tracking(season = start, measure_type = c("SpeedDistance"),
                                 type =c("Player"), date_from = date,
                                 date_to = date
      )
      
      ## Checking if there was a record on that given day, else do nothing
      if(length(single_day) != 1)
      {
        single_day <- single_day %>% 
          mutate("date" = as.Date(date, format = "%m/%d/%Y"))
        
        accumlate_games <- rbind(accumlate_games,single_day)
      }
    }
    
  }
  
  ## second half of the season binding the results from each day 
  for(month in 1:6){
    
    for(day in 1:31)
    {
      Sys.sleep(1)
      ## setting the date to scrape
      date <- paste(month, day, finish, sep = "/")
      print(paste("Getting ",date))
      
      ## getting the date 
      single_day <- get_tracking(season = start, measure_type = c("SpeedDistance"),
                                 type =c("Player"), date_from = date,
                                 date_to = date
      )
      
      ## Checking if there was a record on that given day, else do nothing
      if(length(single_day) != 1)
      {
        single_day <- single_day %>% 
          mutate("date" = as.Date(date, format = "%m/%d/%Y"))
        
        accumlate_games <- rbind(accumlate_games,single_day)
      }
    }
  }
    return(accumlate_games)  
  
}

# Generating and writting results to CSV ----------------------------------

## Storing the results of game by game data by season.

## 2019/20 
drives_season_2019_20 <- Drives_by_day()

write.csv(drives_season_2019_20,
          "./data/drives_tracking_data/tracking_drives_season_daily_2019_20.csv",
          row.names = FALSE)

## 2018/19
drives_season_2018_19 <- Drives_by_day()

write.csv(drives_season_2018_19,
          "./data/drives_tracking_data/tracking_drives_season_daily_2018_19.csv",
          row.names = FALSE)

## 2017/18
drives_season_2017_18 <- Drives_by_day()

write.csv(drives_season_2017_18,
          "./data/drives_tracking_data/tracking_drives_season_daily_2017_18.csv",
          row.names = FALSE)

## 2016/17
drives_season_2016_17 <- Drives_by_day()

write.csv(drives_season_2016_17,
          "./data/drives_tracking_data/tracking_drives_season_daily_2016_17.csv",
          row.names = FALSE)

## 2015/16
drives_season_2015_16 <- Drives_by_day()

write.csv(drives_season_2015_16,
          "./data/drives_tracking_data/tracking_drives_season_daily_2015_16.csv",
          row.names = FALSE)

## 2014/15
drives_season_2014_15 <- Drives_by_day()

write.csv(drives_season_2014_15,
          "./data/drives_tracking_data/tracking_drives_season_daily_2014_15.csv",
          row.names = FALSE)



# Reading distance csv in -------------------------------------------------

distance_season_2014_15 <-read_csv("./data/distance_tracking_data/tracking_by_player_season_daily_2014_15.csv")

distance_season_2016_17 <-read_csv("./data/distance_tracking_data/tracking_by_player_season_daily_2016_17.csv")

distance_season_2017_18 <-read_csv("./data/distance_tracking_data/tracking_by_player_season_daily_2017_18.csv")

distance_season_2018_19 <-read_csv("./data/distance_tracking_data/tracking_by_player_season_daily_2018_19.csv")

distance_season_2019_20 <-read_csv("./data/distance_tracking_data/tracking_by_player_season_daily_2019_20.csv")


# merging distance and drive ----------------------------------------------


distance_drive_2014_15 <- merge(drives_season_2014_15, distance_season_2014_15, 
                                by = c("player_id", "date", "player_name", "team_id", "team_abbreviation", 
                                       "gp", "w", "l"))


distance_drive_2016_17 <- merge(drives_season_2016_17, distance_season_2016_17, 
                                by = c("player_id", "date", "player_name", "team_id", "team_abbreviation", 
                                       "gp", "w", "l"))

distance_drive_2017_18 <- merge(drives_season_2017_18, distance_season_2017_18, 
                                by = c("player_id", "date", "player_name", "team_id", "team_abbreviation", 
                                       "gp", "w", "l"))

distance_drive_2018_19 <- merge(drives_season_2018_19, distance_season_2018_19, 
                                by = c("player_id", "date", "player_name", "team_id", "team_abbreviation", 
                                       "gp", "w", "l"))


distance_drive_2019_20 <- merge(drives_season_2019_20, distance_season_2019_20, 
                                by = c("player_id", "date", "player_name", "team_id", "team_abbreviation", 
                                       "gp", "w", "l"))

# creating merge csv ------------------------------------------------------

write.csv(distance_drive_2014_15,
          "./data/distance_drives_tracking_data/tracking_drives_distance_daily_2014_15.csv",
          row.names = FALSE)

write.csv(distance_drive_2016_17,
          "./data/distance_drives_tracking_data/tracking_drives_distance_daily_2016_17.csv",
          row.names = FALSE)

write.csv(distance_drive_2017_18,
          "./data/distance_drives_tracking_data/tracking_drives_distance_daily_2017_18.csv",
          row.names = FALSE)

write.csv(distance_drive_2018_19,
          "./data/distance_drives_tracking_data/tracking_drives_distance_daily_2018_19.csv",
          row.names = FALSE)

write.csv(distance_drive_2019_20,
          "./data/distance_drives_tracking_data/tracking_drives_distance_daily_2019_20.csv",
          row.names = FALSE)

#select_at(vars(-ends_with(".y"))) %>%
# eda ---------------------------------------------------------------------

distance_drive_2014_15 %>%
  ggplot(aes(x = dist_miles, y = drives)) +
  geom_point(alpha = .3) +
  theme_bw()

distance_drive_2016_17 %>%
  ggplot(aes(x = dist_miles, y = drives)) +
  geom_point(alpha = .3) +
  theme_bw()

distance_drive_2017_18 %>%
  ggplot(aes(x = dist_miles, y = drives)) +
  geom_point(alpha = .3) +
  theme_bw()


distance_drive_2014_15 %>%
  filter(team_abbreviation == "LAC") %>%
  ggplot(aes(x = dist_miles, y = drives)) +
  geom_point(alpha = .3) +
  theme_bw()

distance_drive_2014_15 %>%
  filter(team_abbreviation == "LAC") %>%
  filter(player_name == "Chris Paul") %>%
  #filter(date == "1/17/2015" | date =="1/19/2015" | date =="1/22/2015"
         #| date == "1/25/2015" | date == "1/26/2015" | date == "1/28/2015" |
           #date == "1/30/2015") %>%
  ggplot(aes(x = date, y = drives)) +
  geom_point(alpha = .3) +
  theme_bw()

#expected_point_diff = home_season_average - away_season_average
#expected_point_diff = home_season_average - away_season_average
#deviation_from_expected_diff = actual_point_diff - expected_point_diff


# density_distance_drive --------------------------------------------------

library(airball)

#2014-2015
data <- nba_travel(start_season = 2015, end_season = 2015)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(distance_drive_2014_15$date, format = "%m/%d/%Y")

distance_drive_2014_15 <- distance_drive_2014_15 %>%
  mutate("date" = date1)

distance_drive_2014_15 <- merge(distance_drive_2014_15, team_name_id, by = "team_id")

density_drives_2014_15 <- merge(distance_drive_2014_15, density, by = c("date", "Team"))


write.csv(density_drives_2014_15,
          "./data/density_distance_drives_tracking/_density_drives_distance_daily_2014_15.csv",
          row.names = FALSE)

#2016-2017
data <- nba_travel(start_season = 2017, end_season = 2017)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(distance_drive_2016_17$date, format = "%m/%d/%Y")

distance_drive_2016_17 <- distance_drive_2016_17 %>%
  mutate("date" = date1)

distance_drive_2016_17 <- merge(distance_drive_2016_17, team_name_id, by = "team_id")

density_drives_2016_17 <- merge(distance_drive_2016_17, density, by = c("date", "Team"))


write.csv(density_drives_2016_17,
          "./data/density_distance_drives_tracking/_density_drives_distance_daily_2016_17.csv",
          row.names = FALSE)

#2017-2018
data <- nba_travel(start_season = 2018, end_season = 2018)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(distance_drive_2017_18$date, format = "%m/%d/%Y")

distance_drive_2017_18 <- distance_drive_2017_18 %>%
  mutate("date" = date1)

distance_drive_2017_18 <- merge(distance_drive_2017_18, team_name_id, by = "team_id")

density_drives_2017_18 <- merge(distance_drive_2017_18, density, by = c("date", "Team"))


write.csv(density_drives_2017_18,
          "./data/density_distance_drives_tracking/_density_drives_distance_daily_2017_18.csv",
          row.names = FALSE)

#2018-2019
data <- nba_travel(start_season = 2019, end_season = 2019)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(distance_drive_2018_19$date, format = "%m/%d/%Y")

distance_drive_2018_19 <- distance_drive_2018_19 %>%
  mutate("date" = date1)

distance_drive_2018_19 <- merge(distance_drive_2018_19, team_name_id, by = "team_id")

density_drives_2018_19 <- merge(distance_drive_2018_19, density, by = c("date", "Team"))


write.csv(density_drives_2018_19,
          "./data/density_distance_drives_tracking/_density_drives_distance_daily_2018_19.csv",
          row.names = FALSE)

#2019-2020
data <- nba_travel(start_season = 2020, end_season = 2020)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(distance_drive_2019_20$date, format = "%m/%d/%Y")

distance_drive_2019_20 <- distance_drive_2019_20 %>%
  mutate("date" = date1)

distance_drive_2019_20 <- merge(distance_drive_2019_20, team_name_id, by = "team_id")

density_drives_2019_20 <- merge(distance_drive_2019_20, density, by = c("date", "Team"))


write.csv(density_drives_2019_20,
          "./data/density_distance_drives_tracking/_density_drives_distance_daily_2019_20.csv",
          row.names = FALSE)



# eda ---------------------------------------------------------------------

density_drives_2014_15 %>%
  filter(team_abbreviation == "LAC") %>%
  ggplot(aes(x = dist_miles, y = drives)) +
  geom_point(alpha = .3) +
  theme_bw()

b2b <- density_drives_2014_15 %>%
  filter(`B2B-2nd` == "Yes")


density_drives_2014_15 %>%
  #filter(team_abbreviation == "DAL") %>%
  ggplot(aes(x = date, y = drives)) +
  geom_point() +
  geom_point(data=b2b, aes(x = date, y = drives),
             color = "red", alpha = .5) +
  theme_bw()

density_drives_2014_15 %>%
  ggplot(aes(y = drives)) +
  geom_bar(aes(x = `B2B-2nd`), stat = "Identity") +
  theme_bw()

density_drives_2014_15 %>%
  ggplot(aes(y = drives)) +
  geom_bar(aes(x = `B2B-1st`), stat = "Identity") +
  theme_bw()

density_drives_2014_15 %>%
  ggplot(aes(y = drives)) +
  geom_bar(aes(x = `B2B`), stat = "Identity") +
  theme_bw()

density_drives_2014_15 %>%
  ggplot(aes(y = drives)) +
  geom_bar(aes(x = `3in4`), stat = "Identity") +
  theme_bw()

density_drives_2014_15 %>%
  ggplot(aes(y = drives)) +
  geom_bar(aes(x = `4in5`), stat = "Identity") +
  theme_bw()

density_drives_2014_15 %>%
  ggplot(aes(y = drives)) +
  geom_bar(aes(x = `5in7`), stat = "Identity") +
  theme_bw()

mean(b2b$drives, na.rm = TRUE)

b2bNO <- density_drives_2014_15 %>%
  filter(`B2B-2nd` == "No")

mean(b2bNO$drives, na.rm = TRUE)

mean(density_drives_2014_15$drives, na.rm = TRUE)

density_drives_2014_15 %>%
  ggplot(aes(y = mean(drives))) +
           geom_bar(aes(x = `B2B-2nd`), stat = "Identity") +
           theme_bw()

density_drives_2014_15 %>%
  ggplot(aes(y = avgDrives)) +
           geom_bar(aes(x = `B2B-2nd`), stat = "Identity") +
           theme_bw()

density_drives_2014_15 %>%
  group_by(team_abbreviation) %>%
  mean(density_drives_2014_2015$dist_miles, na.rm = TRUE)

aggregate(density_drives_2014_15$dist_miles, list(density_drives_2014_15$team_abbreviation), FUN=mean, na.rm =TRUE) 

mean(distance_drive_2014_15$dist_miles, na.rm =TRUE)

install.packages("foreach")

library(foreach)

team_distance_season_2014_15 <-read_csv("./data/tracking_by_team_season_summaries_2013_to_20.csv")

