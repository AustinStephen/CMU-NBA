#Time of game
time2015 <- get_schedule(2015)
time2016 <- get_schedule(2016)
time2017 <- get_schedule(2017)
time2018 <- get_schedule(2018)
time2020 <- get_schedule(2020)

tipoffs <- rbind(time2015,time2016)
tipoffs <- rbind(tipoffs,time2017)
tipoffs <- rbind(tipoffs,time2018)
tipoffs <- rbind(tipoffs,time2020)

tipoffs <- tipoffs %>%
  filter(season_type == "REG") %>%
  select(c("season","game_id", "game_date", "game_time_utc", "visitor", "home"))

west <- c("UTA", "SAS", "DAL", "DEN", "OKC", 'MIN', "SAC", "HOU", "LAC", "PHX",
          "LAL", "GSW", "MEM", "NOP", "POR")

tipoffs <- tipoffs %>%
  mutate(visitor_west_coast = visitor %in% west) %>%
  mutate(visitor_east_coast = 1 - visitor_west_coast)

substring_time <- c()
for (t in tipoffs$game_time_utc) {
  temp <- substr(t, 1, 2)
  substring_time <- as.integer(c(substring_time, temp))
}

est <- (substring_time - 5)%%24

tipoffs <-  tipoffs %>%
  mutate(game_time_est = est)

subset_visitors <- together_net_Rating_allWindows %>% 
  select(c(Date, game_id, visitor, Win, score_diff, capped_score_diff, game_net_rating))

subset_visitors <- subset_visitors %>%
  mutate(abrev = substr(visitors$matchup, 1, 3))

tipoffs_and_results <- merge(x = tipoffs, y = subset_visitors,
               by = "game_id") %>%
  select(-c("Date", "abrev"))

               
morning_game_for_west_visiting_east <- tipoffs_and_results %>%
  filter(visitor_west_coast == TRUE) %>%
  filter(game_time_est <= 15)


night_game_for_east_visiting_west <- tipoffs_and_results %>%
  filter(visitor_east_coast == TRUE) %>%
  filter(game_time_est >= 18)


#Probably not enough observations for this histogram to be super useful
ggplot(data = morning_game_for_west_visiting_east, aes(x= capped_score_diff)) +
  geom_histogram( color = "black", fill = "white")
#117 observations

#Interesting to see that there are quite a bit of blowout losses when EST teams play at night
ggplot(data = night_game_for_east_visiting_west, aes(x= capped_score_diff)) +
  geom_histogram( color = "black", fill = "white")

#2254 observations


#We could use these results to show that tipoff time does not have an effect on outcome of the game





