## PURPOSE: generate team records by the day
## AUTHOR: Austin
install.packages("remotes")
remotes::install_github("rtelmore/ballr")
library(ballr)
library(tidyverse)

## enter season as c(2016,2017) for season that starts in 2016 and ends in 2017

record_2016 <- get_record_by_game(c(2016,2017))

write.csv(record_2016, "./data/record_by_day_2016_17_season.csv")

record_2017 <- get_record_by_game(c(2017,2018))

write.csv(record_2017, "./data/record_by_day_2017_18_season.csv")

record_2018 <- get_record_by_game(c(2018,2019))

write.csv(record_2018, "./data/record_by_day_2018_19_season.csv")
          
          

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
for(month in 1:6){
  
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



