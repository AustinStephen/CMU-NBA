library(tidyverse)
devtools::install_github("PatrickChodowski/NBAr")
library(NBAr)

setseason <- c("2016","2017")

get_tracking_by_day1 <- function(season)
{
  
  ## clearing out tmp variables pre run
  accumulate_games <- c()
  tracking <- c()
  
  #west <- c()
  #east <- c()
  #all_for_day <- c()
  #tmp <- c()
  
  ## Setting up season parse
  start <- season[1]
  finish <- season[2]
  
  ## 2016 binding the results from each day from the teams_players_stats call 
  for(month in c(10,11,12)){
    
    for(day in 1:31)
    {
      ## setting the date to scrape
      date <- paste(month, day, start, sep = "-")
      print(paste("Getting ",date))
      
      ## getting the date 
      #tmp <- NBAStandingsByDate(date)
      # east <- tmp[["East"]]
      # west <- tmp[["West"]]
      tracking <-
        get_tracking(start,
                     measure_type = "SpeedDistance", 
                     season_type = "Regular+Season",
                     type = "Player",
                     date_from = date,
                     date_to = date) %>%
      
      ## Checking if there was a record on that given day
      if(length(tracking) != 0)
      {
        ## adding date of standings and matching col titles 
        tracking <- tracking %>% 
          mutate("date" = date)
        
        accumulate_games <- rbind(accumulate_games,tracking)
      }
    }
    
  }
  
  ## 2017 binding the results from each day 
  for(month in 1:6){
    
    for(day in 1:31)
    {
      ### setting the date to scrape
      date <- paste(month, day, finish, sep = "-")
      print(paste("Getting ",date))

      
      ## getting the date 
      #tmp <- NBAStandingsByDate(date)
      #east <- tmp[["East"]]
      #west <- tmp[["West"]]
      tracking <-
        get_tracking(start,
                     measure_type = "SpeedDistance", 
                     season_type = "Regular+Season",
                     type = "Player",
                     date_from = date,
                     date_to = date) 
      
      ## Checking if there was a record on that given day
      if(length(tracking) != 0)
      {
        ## adding date of standings and matching col titles 
        tracking <- tracking %>% 
          mutate("date" = date)
        
        accumulate_games <- rbind(accumulate_games,tracking)
      }
    }
    
  }
  return(accumulate_games)
}

setseason <- c("2016", "2017")
get_tracking_by_day1(setseason)

