## PURPOSE: Pull Reference Dataframes
## AUTHOR: Austin 

library(nbastatR)
library(NBAr)
library(ballr)
library(tidyverse)
library(dplyr)



# Schedule ----------------------------------------------------------------

schedule <- c()
for(year in 2011:2021){

## Getting the schedule 
schedule <- seasons_schedule(seasons = year)

## name and location 
string_title <- paste("data/schedules/","season_",(year-1),"_",(year),".csv",sep="")

## Writing the file 
write.csv(schedule, string_title,row.names = FALSE)
}


# Other -------------------------------------------------------------------

## general team information 
teams_dictonary <- nba_teams()

## dictonary of things reference in other tables 
dictonary_terms <- dictionary_nba_names()

## getting list of all players names and their teams
dictonary_players <- dictionary_bref_players()
