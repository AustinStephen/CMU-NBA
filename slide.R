install.packages("fst")
library(fst)
library(tidyverse)
library(dplyr)
library(nbastatR)
library(NBAr)
library(airball)
install.packages("slider")
library(slider)

# Day by day tracking data ----------------------------------------------

## Pass the function a vector with the start and end years of the season 
## ex. c(2016,2017)

Team_drives_by_day <- function(season){
  
  ## clearing out tmp variables pre run
  accumlate_games <- c()
  west <- c()
  east <- c()
  all_for_day <- c()
  tmp <- c()
  
  ## Setting up season parse
  start <- 2019
  finish <- 2020
  
  ## first half of the season binding the results from each day 
  
  for(month in c(10,11,12)){
    
    for(day in 1:31)
    {
      Sys.sleep(1)
      ## setting the date to scrape
      date <- paste(month, day, start, sep = "/")
      print(paste("Getting ",date))
      
      ## getting the date 
      single_day <- get_tracking(season = start, measure_type = c("Drives"),
                                 type =c("Team"), date_from = date,
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
  
  ## second half of the season binding the results from each day 
  for(month in 1:8){
    
    for(day in 1:31)
    {
      Sys.sleep(1)
      ## setting the date to scrape
      date <- paste(month, day, finish, sep = "/")
      print(paste("Getting ",date))
      
      ## getting the date 
      single_day <- get_tracking(season = start, measure_type = c("Drives"),
                                 type =c("Team"), date_from = date,
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
  return(accumlate_games)  
  
}

# Generating and writting results to CSV ----------------------------------

## Storing the results of game by game data by season.

## 2019/20 
team_drives_season_2019_20 <- Team_drives_by_day()

write.csv(team_drives_season_2019_20,
          "./data/distance_tracking_data/tracking_drives_season_daily_2019_20.csv",
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


# team distance -----------------------------------------------------------



team_distance_season_2019_20 <-read_csv("./data/tracking_by_team_season_summaries_2013_to_20.csv")

Team_distance_by_day <- function(season){
  
  ## clearing out tmp variables pre run
  accumlate_games <- c()
  west <- c()
  east <- c()
  all_for_day <- c()
  tmp <- c()
  
  ## Setting up season parse
  start <- 2014
  finish <- 2015
  
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
                                 type =c("Team"), date_from = date,
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
                                 type =c("Team"), date_from = date,
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
  return(accumlate_games)  
  
}

# Generating and writting results to CSV ----------------------------------

## Storing the results of game by game data by season.

## 2019/20 
team_distance_season_2019_20 <- Team_distance_by_day()

write.csv(team_distance_season_2019_20,
          "./data/distance_tracking_data/tracking_team_distance_season_daily_2019_20.csv",
          row.names = FALSE)

## 2018/19
team_distance_season_2018_19 <- Team_distance_by_day()

write.csv(team_distance_season_2018_19,
          "./data/distance_tracking_data/tracking_team_distance_season_daily_2018_19.csv",
          row.names = FALSE)

## 2017/18
team_distance_season_2017_18 <- Team_distance_by_day()

write.csv(team_distance_season_2017_18,
          "./data/distance_tracking_data/tracking_team_distance_season_daily_2017_18.csv",
          row.names = FALSE)


## 2016/17
team_distance_season_2016_17 <- Team_distance_by_day()

write.csv(team_distance_season_2016_17,
          "./data/distance_tracking_data/tracking_team_distance_season_daily_2016_17.csv",
          row.names = FALSE)

## 2015/16
team_distance_season_2015_16 <- Team_distance_by_day()


write.csv(team_distance_season_2015_16,
          "./data/distance_tracking_data/tracking_team_distance_season_daily_2015_16.csv",
          row.names = FALSE)

## 2014/15
team_distance_season_2014_15 <- Team_distance_by_day()


write.csv(team_distance_season_2014_15,
          "./data/distance_tracking_data/tracking_team_distance_season_daily_2014_15.csv",
          row.names = FALSE)

#all seasons
data <- nba_travel(start_season = 2013, end_season = 2020)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(team_distance_season_2013_20$date, format = "%m/%d/%Y")

team_distance_season_2019_20 <- team_distance_season_2019_20 %>%
  mutate("date" = date1)

team_distance_season_2013_20 <- merge(team_distance_season_2013_20, team_name_id, by = "team_id")

team_density_distance_2013_20 <- merge(team_distance_season_2013_20, density, by = c("Team"))


#2019-2020
data <- nba_travel(start_season = 2020, end_season = 2020)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(team_distance_season_2019_20$date, format = "%m/%d/%Y")

team_distance_season_2019_20 <- team_distance_season_2019_20 %>%
  mutate("date" = date1)

team_distance_season_2019_20 <- merge(team_distance_season_2019_20, team_name_id, by = "team_id")

team_density_distance_2019_20 <- merge(team_distance_season_2019_20, density, by = c("date", "Team"))

#2018-2019
data <- nba_travel(start_season = 2019, end_season = 2019)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(team_distance_season_2018_19$date, format = "%m/%d/%Y")

team_distance_season_2018_19 <- team_distance_season_2018_19 %>%
  mutate("date" = date1)

team_distance_season_2018_19 <- merge(team_distance_season_2018_19, team_name_id, by = "team_id")

team_density_distance_2018_19 <- merge(team_distance_season_2018_19, density, by = c("date", "Team"))

#2017-2018
data <- nba_travel(start_season = 2018, end_season = 2018)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(team_distance_season_2017_18$date, format = "%m/%d/%Y")

team_distance_season_2017_18 <- team_distance_season_2017_18 %>%
  mutate("date" = date1)

team_distance_season_2017_18 <- merge(team_distance_season_2017_18, team_name_id, by = "team_id")

team_density_distance_2017_18 <- merge(team_distance_season_2017_18, density, by = c("date", "Team"))

#2016-2017
data <- nba_travel(start_season = 2017, end_season = 2017)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(team_distance_season_2016_17$date, format = "%m/%d/%Y")

team_distance_season_2016_17 <- team_distance_season_2016_17 %>%
  mutate("date" = date1)

team_distance_season_2016_17 <- merge(team_distance_season_2016_17, team_name_id, by = "team_id")

team_density_distance_2016_17 <- merge(team_distance_season_2016_17, density, by = c("date", "Team"))

#2015-2016
data <- nba_travel(start_season = 2016, end_season = 2016)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(team_distance_season_2015_16$date, format = "%m/%d/%Y")

team_distance_season_2015_16 <- team_distance_season_2015_16 %>%
  mutate("date" = date1)

team_distance_season_2015_16 <- merge(team_distance_season_2015_16, team_name_id, by = "team_id")

team_density_distance_2015_16 <- merge(team_distance_season_2015_16, density, by = c("date", "Team"))

#2014-2015
data <- nba_travel(start_season = 2015, end_season = 2015)
density <- nba_density(df = data) %>%
  rename(date = Date)

date1 <- as.Date(team_distance_season_2014_15$date, format = "%m/%d/%Y")

team_distance_season_2014_15 <- team_distance_season_2014_15 %>%
  mutate("date" = date1)

team_distance_season_2014_15 <- merge(team_distance_season_2014_15, team_name_id, by = "team_id")

team_density_distance_2014_15 <- merge(team_distance_season_2014_15, density, by = c("date", "Team"))

# slider ----

slide_index(x, i, ~.x, .before = lubridate::days(1))

days <- as.numeric(lubridate::days(5))

as.Date(lubridate::days(5))

season <- c("2014", "2015")

slide_dist <- slide(distance_season_2014_15$dist_miles, sum, .before = 5)

slide_dist_date <- slide_index(distance_season_2014_15$dist_miles, unique(distance_season_2014_15$player_id), sum, .before = 5)

slide_by_player <- slide_dist[[1]] %>%
  group_by(player_id)

distance_season_2014_15 %>%
  group_by(player_id)
  

# net rating --------------------------------------------------------------

netRating_by_day <- function(season){
  
  ## clearing out tmp variables pre run
  accumlate_games <- c()
  west <- c()
  east <- c()
  all_for_day <- c()
  tmp <- c()
  
  ## Setting up season parse
  start <- 2019
  finish <- 2020
  
  ## first half of the season binding the results from each day 
  
  for(month in c(10,11,12)){
    
    for(day in 1:31)
    {
      Sys.sleep(1)
      ## setting the date to scrape
      date <- paste(month, day, start, sep = "/")
      print(paste("Getting ",date))
      
      ## getting the date 
      single_day <-              
        get_general(season = start, type = c('Player'), measure_type = c("Advanced"), 
                    date_from = date, date_to = date
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
  
  ## second half of the season binding the results from each day 
  for(month in 1:8){
    
    for(day in 1:31)
    {
      Sys.sleep(1)
      ## setting the date to scrape
      date <- paste(month, day, finish, sep = "/")
      print(paste("Getting ",date))
      
      ## getting the date 
      single_day <- get_general(season = start, type = c('Player'), measure_type = c("Advanced"), 
                    date_from = date, date_to = date
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
  return(accumlate_games)  
  
}

## 2014/15
netRating_season_2014_15 <- netRating_by_day()


write.csv(team_distance_season_2014_15,
          "./data/netRating_tracking_data/tracking_netRating_season_daily_2014_15.csv",
          row.names = FALSE)


netRating_season_2015_16 <- netRating_by_day()


write.csv(team_distance_season_2015_16,
          "./data/netRating_tracking_data/tracking_netRating_season_daily_2015_16.csv",
          row.names = FALSE)


netRating_season_2016_17 <- netRating_by_day()


write.csv(team_distance_season_2016_17,
          "./data/netRating_tracking_data/tracking_netRating_season_daily_2016_17.csv",
          row.names = FALSE)

netRating_season_2017_18 <- netRating_by_day()


write.csv(team_distance_season_2017_18,
          "./data/netRating_tracking_data/tracking_netRating_season_daily_2017_18.csv",
          row.names = FALSE)

netRating_season_2018_19 <- netRating_by_day()


write.csv(team_distance_season_2018_19,
          "./data/netRating_tracking_data/tracking_netRating_season_daily_2018_19.csv",
          row.names = FALSE)

netRating_season_2019_20 <- netRating_by_day()


write.csv(team_distance_season_2019_20,
          "./data/netRating_tracking_data/tracking_netRating_season_daily_2019_20.csv",
          row.names = FALSE)

