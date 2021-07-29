## PURPOSE: generate team records by the day
## AUTHOR: Austin
library(ballr)
library(nbastatR)
library(NBAr)
library(tidyverse)
library(dplyr)
devtools::install_github("PatrickChodowski/NBAr",force = TRUE)

## enter season as c(2016,2017) for season that starts in 2016 and ends in 2017

record_2010 <- get_record_by_game(c(2010,2011)) %>% 
  subset(!(team %in% 
           c("Southeast Division","Northwest Division","Atlantic Division",
             "Central Division","Pacific Division","Southwest Division")))

write.csv(record_2010, 
          "./data/record_by_day/record_by_day_2010_11_season.csv",
          row.names = FALSE)

record_2011 <- get_record_by_game(c(2011,2012)) %>% 
  subset(!(team %in% 
             c("Southeast Division","Northwest Division","Atlantic Division",
               "Central Division","Pacific Division","Southwest Division")))

write.csv(record_2011,
          "./data/record_by_day/record_by_day_2011_12_season.csv",
          row.names = FALSE)

record_2012 <- get_record_by_game(c(2012,2013)) %>% 
  subset(!(team %in% 
             c("Southeast Division","Northwest Division","Atlantic Division",
               "Central Division","Pacific Division","Southwest Division")))

write.csv(record_2012, 
          "./data/record_by_day/record_by_day_2012_13_season.csv",
          row.names = FALSE)

record_2013 <- get_record_by_game(c(2013,2014)) %>% 
  subset(!(team %in% 
             c("Southeast Division","Northwest Division","Atlantic Division",
               "Central Division","Pacific Division","Southwest Division")))

write.csv(record_2013,
          "./data/record_by_day/record_by_day_2013_14_season.csv",
          row.names = FALSE)

record_2014 <- get_record_by_game(c(2014,2015)) %>% 
  subset(!(team %in% 
             c("Southeast Division","Northwest Division","Atlantic Division",
               "Central Division","Pacific Division","Southwest Division")))

write.csv(record_2014,
          "./data/record_by_day/record_by_day_2014_15_season.csv",
          row.names = FALSE)

record_2015 <- get_record_by_game(c(2015,2016))

write.csv(record_2015,
          "./data/record_by_day/record_by_day_2015_16_season.csv")

record_2016 <- get_record_by_game(c(2016,2017))

write.csv(record_2016, "./data/record_by_day_2016_17_season.csv")

record_2017 <- get_record_by_game(c(2017,2018))

write.csv(record_2017, "./data/record_by_day/record_by_day_2017_18_season.csv")

record_2018 <- get_record_by_game(c(2018,2019))

write.csv(record_2018, "./data/record_by_day/record_by_day_2018_19_season.csv")
   
record_2019 <- get_record_by_game(c(2019,2020))

write.csv(record_2019, "./data/record_by_day/record_by_day_2019_20_season.csv")       
          

# function definition ------------------------------------------------------

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
    date <- paste(start, month, day, sep = "-")
    print(paste("Getting ",date))
    
    ## getting the date 
    tmp <- NBAStandingsByDate(date)
    east <- tmp[["East"]]
    west <- tmp[["West"]]
    
    ## Checking if there was a record on that given day
    if(length(east) != 0 & length(west) != 0)
    {
      ## adding date of standings and matching col titles 
      west <- west %>% 
        mutate("date" = date) %>%
        rename("team" = western_conference)
        
      east <- east %>% 
        mutate("date" = date) %>%
        rename("team" = eastern_conference)
    
    
      all_for_day <- rbind(east,west)
      accumlate_games <- rbind(accumlate_games,all_for_day)
    }
  }
  
}

## 2017 binding the results from each day 
for(month in 1:8){
  
  for(day in 1:31)
  {
    ## setting the date to scrape
    date <- paste(finish, month, day, sep = "-")
    print(paste("Getting ",date))
    
    ## getting the date 
    tmp <- NBAStandingsByDate(date)
    east <- tmp[["East"]]
    west <- tmp[["West"]]
    
    ## Checking if there was a record on that given day
    if(length(east) != 0 & length(west) != 0)
    {
      ## adding date of standings and matching col titles 
      west <- west %>% 
        mutate(date = date) %>%
        rename("team" = western_conference)
    
      east <- east %>% 
        mutate(date = date) %>%
        rename("team" = eastern_conference)
    
    
      all_for_day <- rbind(east,west)
      accumlate_games <- rbind(accumlate_games,all_for_day)
    }
  }
  
}
return(accumlate_games)
}



