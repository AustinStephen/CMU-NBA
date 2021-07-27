install.packages("fst")
library(fst)
library(tidyverse)
library(dplyr)
library(nbastatR)
library(NBAr)
library(airball)
install.packages("slider")
library(slider)
library(lubridate, warn.conflicts = FALSE)
# slider ----

distance__season_2014_15 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2014_15.csv")

date1 <- as.Date(distance__season_2014_15$date, format = "%m/%d/%Y")


temp_dist_5day_wind_2014_15 <- distance__season_2014_15 %>%
  mutate("date" = date1)

dist_window_2014_15 <- temp_dist_5day_wind_2014_15 %>%
  group_by(player_name) %>%
  mutate(dist_3dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(3), .complete = F)) %>%
  mutate(dist_3dayWindow = dist_3dayWindow - dist_miles) %>%
  mutate(dist_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                        .i = date,
                                        .f = ~sum(.x$dist_miles), # arg .f
                                        .before = days(5), .complete = F)) %>%
  mutate(dist_5dayWindow = dist_5dayWindow - dist_miles) %>%
  mutate(dist_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(7), .complete = F)) %>%
  mutate(dist_7dayWindow = dist_7dayWindow - dist_miles) %>%
  mutate(dist_10dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(10), .complete = F)) %>%
  mutate(dist_10dayWindow = dist_10dayWindow - dist_miles)

write.csv(dist_window_2014_15,
          "./data1/distance_window_data/player_Window_dist_daily_2014_15.csv",
          row.names = FALSE)

check_5dayWind_season_2014_15 <-read_csv("./data1/distance_window_data/player_Window_dist_daily_2014_15.csv")

temp_5dayWind_season_2014_15 <-read_csv("./data1/5_day_window_data/player_5dayWindow_dist_daily_2014_15.csv",
                                         col_types = list(col_double(),col_character(),
                                                          col_double(), col_character(),
                                                          col_double(),col_double(),
                                                          col_double(), col_double(),col_double(),
                                                          col_double(),col_double(), col_double(),
                                                          col_double(), col_double(), col_double(), 
                                                          col_double(),col_character(), col_double()))

distance__season_2016_17 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2016_17.csv")

date1 <- as.Date(distance__season_2016_17$date, format = "%m/%d/%Y")


distance__season_2016_17 <- distance__season_2016_17 %>%
  mutate("date" = date1)

dist_window_2016_17 <- distance__season_2016_17 %>%
  group_by(player_name) %>%
  mutate(dist_3dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(3), .complete = F)) %>%
  mutate(dist_3dayWindow = dist_3dayWindow - dist_miles) %>%
  mutate(dist_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(5), .complete = F)) %>%
  mutate(dist_5dayWindow = dist_5dayWindow - dist_miles) %>%
  mutate(dist_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(7), .complete = F)) %>%
  mutate(dist_7dayWindow = dist_7dayWindow - dist_miles) %>%
  mutate(dist_10dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$dist_miles), # arg .f
                                                    .before = days(10), .complete = F)) %>%
  mutate(dist_10dayWindow = dist_10dayWindow - dist_miles)


write.csv(dist_window_2016_17,
          "./data1/distance_window_data/player_Window_dist_daily_2016_17.csv",
          row.names = FALSE)


distance__season_2017_18 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2017_18.csv") 
                                    #col_types = list(col_double(),col_character()))

date1 <- as.Date(distance__season_2017_18$date, format = "%m/%d/%Y")


distance__season_2017_18 <- distance__season_2017_18 %>%
  mutate("date" = date1)

dist_window_2017_18 <- distance__season_2017_18 %>%
  group_by(player_name) %>%
  mutate(dist_3dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(3), .complete = F)) %>%
  mutate(dist_3dayWindow = dist_3dayWindow - dist_miles) %>%
  mutate(dist_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(5), .complete = F)) %>%
  mutate(dist_5dayWindow = dist_5dayWindow - dist_miles) %>%
  mutate(dist_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(7), .complete = F)) %>%
  mutate(dist_7dayWindow = dist_7dayWindow - dist_miles) %>%
  mutate(dist_10dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$dist_miles), # arg .f
                                                    .before = days(10), .complete = F)) %>%
  mutate(dist_10dayWindow = dist_10dayWindow - dist_miles)


write.csv(dist_window_2017_18,
          "./data1/distance_window_data/player_Window_dist_daily_2017_18.csv",
          row.names = FALSE)


distance__season_2018_19 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2018_19.csv")

date1 <- as.Date(distance__season_2018_19$date, format = "%m/%d/%Y")


distance__season_2018_19 <- distance__season_2018_19 %>%
  mutate("date" = date1)

dist_window_2018_19 <- distance__season_2018_19 %>%
  group_by(player_name) %>%
  mutate(dist_3dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(3), .complete = F)) %>%
  mutate(dist_3dayWindow = dist_3dayWindow - dist_miles) %>%
  mutate(dist_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(5), .complete = F)) %>%
  mutate(dist_5dayWindow = dist_5dayWindow - dist_miles) %>%
  mutate(dist_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(7), .complete = F)) %>%
  mutate(dist_7dayWindow = dist_7dayWindow - dist_miles) %>%
  mutate(dist_10dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$dist_miles), # arg .f
                                                    .before = days(10), .complete = F)) %>%
  mutate(dist_10dayWindow = dist_10dayWindow - dist_miles)


write.csv(dist_window_2018_19,
          "./data1/distance_window_data/player_Window_dist_daily_2018_19.csv",
          row.names = FALSE)



distance__season_2019_20 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2019_20.csv")

date1 <- as.Date(distance__season_2019_20$date, format = "%m/%d/%Y")


distance__season_2019_20 <- distance__season_2019_20 %>%
  mutate("date" = date1)

dist_window_2019_20 <- distance__season_2019_20 %>%
  group_by(player_name) %>%
  mutate(dist_3dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(3), .complete = F)) %>%
  mutate(dist_3dayWindow = dist_3dayWindow - dist_miles) %>%
  mutate(dist_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(5), .complete = F)) %>%
  mutate(dist_5dayWindow = dist_5dayWindow - dist_miles) %>%
  mutate(dist_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$dist_miles), # arg .f
                                                   .before = days(7), .complete = F)) %>%
  mutate(dist_7dayWindow = dist_7dayWindow - dist_miles) %>%
  mutate(dist_10dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$dist_miles), # arg .f
                                                    .before = days(10), .complete = F)) %>%
  mutate(dist_10dayWindow = dist_10dayWindow - dist_miles)

write.csv(dist_window_2019_20,
          "./data1/distance_window_data/player_Window_dist_daily_2019_20.csv",
          row.names = FALSE)


# windows for avg speed ---------------------------------------------------

distance__season_2014_15 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2014_15.csv")

date1 <- as.Date(distance__season_2014_15$date, format = "%m/%d/%Y")

speed_window_2014_15 <- distance__season_2014_15 %>%
  mutate("date" = date1)

speed_window_2014_15 <- speed_window_2014_15 %>%
  group_by(player_name) %>%
  mutate(speed_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$avg_speed), # arg .f
                                                   .before = days(5), .complete = F)) %>%
  mutate(speed_5dayWindow = speed_5dayWindow - avg_speed) %>%
  mutate(speed_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = date,
                                                   .f = ~sum(.x$avg_speed), # arg .f
                                                   .before = days(7), .complete = F)) %>%
  mutate(speed_7dayWindow = speed_7dayWindow - avg_speed) 

write.csv(speed_window_2014_15,
          "./data1/speed_window_data/player_Window_speed_daily_2014_15.csv",
          row.names = FALSE)

check_speed_season_2014_15 <-read_csv("./data1/speed_window_data/player_Window_speed_daily_2014_15.csv")


distance__season_2016_17 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2016_17.csv")

date1 <- as.Date(distance__season_2016_17$date, format = "%m/%d/%Y")


speed_window_2016_17 <- distance__season_2016_17 %>%
  mutate("date" = date1)

speed_window_2016_17 <- speed_window_2016_17 %>%
  group_by(player_name) %>%
  mutate(speed_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(5), .complete = F)) %>%
  mutate(speed_5dayWindow = speed_5dayWindow - avg_speed) %>%
  mutate(speed_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(7), .complete = F)) %>%
  mutate(speed_7dayWindow = speed_7dayWindow - avg_speed) 

write.csv(speed_window_2016_17,
          "./data1/speed_window_data/player_Window_speed_daily_2016_17.csv",
          row.names = FALSE)

check_speed_season_2016_17 <-read_csv("./data1/speed_window_data/player_Window_speed_daily_2016_17.csv")


distance__season_2017_18 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2017_18.csv") 
#col_types = list(col_double(),col_character()))

date1 <- as.Date(distance__season_2017_18$date, format = "%m/%d/%Y")


speed_window_2017_18 <- distance__season_2017_18 %>%
  mutate("date" = date1)

speed_window_2017_18 <- speed_window_2017_18 %>%
  group_by(player_name) %>%
  mutate(speed_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(5), .complete = F)) %>%
  mutate(speed_5dayWindow = speed_5dayWindow - avg_speed) %>%
  mutate(speed_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(7), .complete = F)) %>%
  mutate(speed_7dayWindow = speed_7dayWindow - avg_speed) 

write.csv(dist_window_2017_18,
          "./data1/speed_window_data/player_Window_speed_daily_2017_18.csv",
          row.names = FALSE)


distance__season_2018_19 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2018_19.csv")

date1 <- as.Date(distance__season_2018_19$date, format = "%m/%d/%Y")


speed_window_2018_19 <- distance__season_2018_19 %>%
  mutate("date" = date1)


speed_window_2018_19 <- speed_window_2018_19 %>%
  group_by(player_name) %>%
  mutate(speed_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(5), .complete = F)) %>%
  mutate(speed_5dayWindow = speed_5dayWindow - avg_speed) %>%
  mutate(speed_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(7), .complete = F)) %>%
  mutate(speed_7dayWindow = speed_7dayWindow - avg_speed) 

write.csv(speed_window_2018_19,
          "./data1/speed_window_data/player_Window_speed_daily_2018_19.csv",
          row.names = FALSE)

distance__season_2019_20 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2019_20.csv")

date1 <- as.Date(distance__season_2019_20$date, format = "%m/%d/%Y")


speed_window_2019_20 <- distance__season_2019_20 %>%
  mutate("date" = date1)

speed_window_2019_20 <- speed_window_2019_20 %>%
  group_by(player_name) %>%
  mutate(speed_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(5), .complete = F)) %>%
  mutate(speed_5dayWindow = speed_5dayWindow - avg_speed) %>%
  mutate(speed_7dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = date,
                                                    .f = ~sum(.x$avg_speed), # arg .f
                                                    .before = days(7), .complete = F)) %>%
  mutate(speed_7dayWindow = speed_7dayWindow - avg_speed) 

write.csv(speed_window_2019_20,
          "./data1/speed_window_data/player_Window_speed_daily_2019_20.csv",
          row.names = FALSE)

#Not important----

density_distance_2014_2015 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2014_15.csv")

dens_dist_5day_wind_2014_15 <- density_distance_2014_2015 %>% 
  # ungroup() %>%
  group_by(player_name) %>%
  mutate(`5dayWind_dist` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                        .f = ~sum(.x$dist_miles), # arg .f
                                        .before = 4, .complete = T)) %>%
  mutate(`5dayWind_drives` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                        .f = ~sum(.x$drives), # arg .f
                                        .before = 4, .complete = T))

density_distance_2016_2017 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2016_17.csv")

dens_dist_5day_wind_2016_17 <- density_distance_2016_2017 %>% 
  # ungroup() %>%
  group_by(player_name) %>%
  mutate(`5dayWind_dist` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                             .f = ~sum(.x$dist_miles), # arg .f
                                             .before = 4, .complete = T)) %>%
  mutate(`5dayWind_drives` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                               .f = ~sum(.x$drives), # arg .f
                                               .before = 4, .complete = T))

density_distance_2017_2018 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2017_18.csv")

dens_dist_5day_wind_2017_18 <- density_distance_2017_2018 %>% 
  # ungroup() %>%
  group_by(player_name) %>%
  mutate(`5dayWind_dist` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                             .f = ~sum(.x$dist_miles), # arg .f
                                             .before = 4, .complete = T)) %>%
  mutate(`5dayWind_drives` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                               .f = ~sum(.x$drives), # arg .f
                                               .before = 4, .complete = T))

density_distance_2018_2019 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2018_19.csv")

dens_dist_5day_wind_2018_19 <- density_distance_2018_2019 %>% 
  # ungroup() %>%
  group_by(player_name) %>%
  mutate(`5dayWind_dist` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                             .f = ~sum(.x$dist_miles), # arg .f
                                             .before = 4, .complete = T)) %>%
  mutate(`5dayWind_drives` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                               .f = ~sum(.x$drives), # arg .f
                                               .before = 4, .complete = T))

density_distance_2019_2020 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_201_20.csv")

dens_dist_5day_wind_2019_20 <- density_distance_2019_2020 %>% 
  # ungroup() %>%
  group_by(player_name) %>%
  mutate(`5dayWind_dist` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                             .f = ~sum(.x$dist_miles), # arg .f
                                             .before = 4, .complete = T)) %>%
  mutate(`5dayWind_drives` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                               .f = ~sum(.x$drives), # arg .f
                                               .before = 4, .complete = T))


check_dist_5day_wind_2014_15 <-read_csv("./data1/5_day_window_data/player_5dayWindow_dist_daily_2014_15.csv")


write.csv(dist_5day_wind_2016_17,
          "./data1/5_day_window_data/player_5dayWindow_dist_daily_2016_17.csv",
          row.names = FALSE)

write.csv(dist_5day_wind_2017_18,
          "./data1/5_day_window_data/player_5dayWindow_dist_daily_2017_18.csv",
          row.names = FALSE)

write.csv(dist_5day_wind_2018_19,
          "./data1/5_day_window_data/player_5dayWindow_dist_daily_2018_19.csv",
          row.names = FALSE)

write.csv(dist_5day_wind_2019_20,
          "./data1/5_day_window_data/player_5dayWindow_dist_daily_2019_20.csv",
          row.names = FALSE)

netRating_2014_2025 <-read_csv("./data1/netRating_tracking_data/tracking_netRating_season_daily_2014_15.csv")


#Together ----
together_updated <-read_csv("./data1/together8.csv")

date1 <- as.Date(together_updated$Date)


together_updated <- together_updated %>%
  mutate("Date" = date1)

together_30dayWind_updated_2010 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2010-11") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = Date,
                                             .f = ~sum(.x$Distance), # arg .f
                                             .before = days(29), .complete = F))
opp_30dayWind_2010 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2010-11") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   .i = Date,
                                                   .f = ~sum(.x$Distance), # arg .f
                                                   .before = days(29), .complete = F))

together_30_2010 <- merge(x = together_30dayWind_updated_2010, y = opp_30dayWind_2010,
                  by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2010 <- together_30_2010 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2011 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2011-12") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2011 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2011-12") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2011 <- merge(x = together_30dayWind_updated_2011, y = opp_30dayWind_2011,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2011 <- together_30_2011 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2012 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2012-13") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2012 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2012-13") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2012 <- merge(x = together_30dayWind_updated_2012, y = opp_30dayWind_2012,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2012 <- together_30_2012 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2013 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2013-14") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2013 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2013-14") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2013 <- merge(x = together_30dayWind_updated_2013, y = opp_30dayWind_2013,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2013 <- together_30_2013 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2014 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2014-15") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2014 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2014-15") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2014 <- merge(x = together_30dayWind_updated_2014, y = opp_30dayWind_2014,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2014 <- together_30_2014 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2015 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2015-16") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2015 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2015-16") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2015 <- merge(x = together_30dayWind_updated_2015, y = opp_30dayWind_2015,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2015 <- together_30_2015 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2016 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2016-17") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2016 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2016-17") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2016 <- merge(x = together_30dayWind_updated_2016, y = opp_30dayWind_2016,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2016 <- together_30_2016 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2017 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2017-18") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2017 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2017-18") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2017 <- merge(x = together_30dayWind_updated_2017, y = opp_30dayWind_2017,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2017 <- together_30_2017 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_updated_2018 <- together_updated %>% 
  # ungroup() %>%
  filter(season == "2018-19") %>%
  group_by(Team) %>%
  mutate(dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                    .i = Date,
                                                    .f = ~sum(.x$Distance), # arg .f
                                                    .before = days(29), .complete = F))
opp_30dayWind_2018 <- together_updated %>%
  select("Date", "Team", "Distance", "season") %>% 
  filter(season == "2018-19") %>%
  group_by(Team) %>%
  mutate(opp_dist_30dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        .i = Date,
                                                        .f = ~sum(.x$Distance), # arg .f
                                                        .before = days(29), .complete = F))

together_30_2018 <- merge(x = together_30dayWind_updated_2018, y = opp_30dayWind_2018,
                          by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_30_2018 <- together_30_2018 %>%
  rename(season = season.x, Distance = Distance.x, opp_distance = Distance.y, opp_season = season.y)


together_30dayWind_dist <- rbind(together_30_2010, together_30_2011, 
                                 together_30_2012, together_30_2013, 
                                 together_30_2014, together_30_2015,
                                 together_30_2016, together_30_2017, 
                                 together_30_2018)

check_together <- together_30dayWind_dist %>%
  dplyr::select(Date, Opponent, game_id, Team, Distance, Route, dist_30dayWindow, opp_distance, opp_dist_30dayWindow)

write.csv(together_30dayWind_dist,
          "./data1/together_30dayWind_dist.csv",
          row.names = FALSE)

#visitors <- together %>%
#select(Date, Opponent, game_id, Team, Visitor, Distance, Route) %>%
#  filter(Visitor == TRUE)

#visitors_5dayWindow <- visitors %>% 
  # ungroup() %>%
 # group_by(Team) %>%
#  mutate(dist_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
 #                                                  .i = Date,
  #                                                 .f = ~sum(.x$Distance), # arg .f
   #                                                .before = days(4), .complete = F))

#home <- together %>%
#  select(Date, Opponent, game_id, Team, Visitor, Distance, Route) %>%
#  filter(Visitor == FALSE)

#home_5dayWindow <- home %>% 
  # ungroup() %>%
#  group_by(Team) %>%
 # mutate(Team_dist_5dayWindow = slider::slide_index_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
#                                                   .i = Date,
#                                                   .f = ~sum(.x$Distance), # arg .f
#                                                   .before = days(4), .complete = F))

#merge_window <- merge(home_5dayWindow, visitors_5dayWindow, by = "game_id")

#check2 <- merge_window %>%
#  select(game_id, Date.y, Opponent.y, Team.y, Visitor.y, Distance.y, Route.y, dist_5dayWindow, Distance.x, Team_dist_5dayWindow)

#Team <- unique(check)

#Team <- Team %>%
#  rename(Opponent = Opponent.x, Team = Team.x, 
 #        Visitor = Visitor.x, Distance = Distance.x, Route = Route.x, 
 #        opp_Distance = Distance.y, opp_dist_5dayWindow = dist_5dayWindow)

#Opponent <- unique(check2)

#Opponent <- Opponent %>%
#  rename(Date = Date.y, Opponent = Opponent.y, Team = Team.y, 
 #        Visitor = Visitor.y, Distance = Distance.y, Route = Route.y, Team_dist_5dayWindow = dist_5dayWindow,
 #        opp_Distance = Distance.x, opp_dist_5dayWindow = Team_dist_5dayWindow)

#together_5dayWindow <- rbind(Team, Opponent)

together_5day_Window_updated <- together_5dayWindow_updated %>%
  rename(Distance = Distance.x, opp_distance = Distance.y)

write.csv(together_5day_Window_updated,
          "./data1/together_5dayWindow_update.csv",
          row.names = FALSE)

check_together_csv <-read_csv("./data1/together_5dayWindow_update.csv")

atl_together <- check_together_csv %>%
  dplyr::select(Date, Opponent, game_id, Team, Distance, Route, game_net_rating ,dist_5dayWindow, opp_distance, opp_dist_5dayWindow) %>%
  filter(Opponent == "Atlanta Hawks")

#Game_net ----
options(scipen = 999)

check_together_csv <- read_csv("./data1/together_30dayWind_dist.csv")

check_together_csv <- check_together_csv %>%
  dplyr::select(-opp_season)

together_netRating_updated <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2010-11") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                   #.i = Date,
                                                   .f = ~sum(.x$game_net_rating), # arg .f
                                                   .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                        #.i = Date,
                                                        .f = ~sum(.x$game_net_rating), # arg .f
                                                        .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)





opp_net_Rating_window <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2010-11") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                       #.i = Date,
                                                       .f = ~sum(.x$game_net_rating), # arg .f
                                                       .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2010 <- merge(x = together_netRating_updated, y = opp_net_Rating_window,
                                     by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2010 <- together_netRating_updated_2010 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)

#2011
together_netRating_updated_2011 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2011-12") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)




opp_net_Rating_window_2011 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2011-12") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2011 <- merge(x = together_netRating_updated_2011, y = opp_net_Rating_window_2011,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2011 <- together_netRating_updated_2011 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)

#2012
together_netRating_updated_2012 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2012-13") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)




opp_net_Rating_window_2012 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2012-13") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2012 <- merge(x = together_netRating_updated_2012, y = opp_net_Rating_window_2012,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2012 <- together_netRating_updated_2012 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)


#2013
together_netRating_updated_2013 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2013-14") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)





opp_net_Rating_window_2013 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2013-14") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2013 <- merge(x = together_netRating_updated_2013, y = opp_net_Rating_window_2013,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2013 <- together_netRating_updated_2013 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)


#2014
together_netRating_updated_2014 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2014-15") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)





opp_net_Rating_window_2014 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2014-15") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2014 <- merge(x = together_netRating_updated_2014, y = opp_net_Rating_window_2014,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2014 <- together_netRating_updated_2014 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)

#2015
together_netRating_updated_2015 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2015-16") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)




opp_net_Rating_window_2015 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2015-16") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2015 <- merge(x = together_netRating_updated_2015, y = opp_net_Rating_window_2015,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2015 <- together_netRating_updated_2015 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)


#2016
together_netRating_updated_2016 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2016-17") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)





opp_net_Rating_window_2016 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2016-17") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2016 <- merge(x = together_netRating_updated_2016, y = opp_net_Rating_window_2016,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2016 <- together_netRating_updated_2016 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)


#2017
together_netRating_updated_2017 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2017-18") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)

  



opp_net_Rating_window_2017 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2017-18") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2017 <- merge(x = together_netRating_updated_2017, y = opp_net_Rating_window_2017,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2017 <- together_netRating_updated_2017 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)


#2018
together_netRating_updated_2018 <- check_together_csv %>% 
  # ungroup() %>%
  filter(season == "2018-19") %>%
  group_by(Team) %>%
  mutate(net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (2), .complete = F), digits = 5)) %>%
  mutate(net_rating_2gameWindow = net_rating_2gameWindow - game_net_rating) %>%
  mutate(net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_5gameWindow = net_rating_5gameWindow - game_net_rating) %>%
  mutate(net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                          #.i = Date,
                                                          .f = ~sum(.x$game_net_rating), # arg .f
                                                          .before = (7), .complete = F), digits = 5)) %>%
  mutate(net_rating_7gameWindow = net_rating_7gameWindow - game_net_rating) %>%
  mutate(net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_10gameWindow = net_rating_10gameWindow - game_net_rating) %>%
  mutate(net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_15gameWindow = net_rating_15gameWindow - game_net_rating) %>%
  mutate(net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_20gameWindow = net_rating_20gameWindow - game_net_rating) %>% 
  mutate(net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_25gameWindow = net_rating_25gameWindow - game_net_rating) %>%
  mutate(net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_30gameWindow = net_rating_30gameWindow - game_net_rating) %>%
  mutate(net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (35), .complete = F), digits = 5)) %>%
  mutate(net_rating_35gameWindow = net_rating_35gameWindow - game_net_rating) %>%
  mutate(net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                           #.i = Date,
                                                           .f = ~sum(.x$game_net_rating), # arg .f
                                                           .before = (40), .complete = F), digits = 5)) %>%
  mutate(net_rating_40gameWindow = net_rating_40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center10gameWindow = net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center20gameWindow = net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center30gameWindow = net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center40gameWindow = net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center50gameWindow = net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                 #.i = Date,
                                                                 .f = ~sum(.x$game_net_rating), # arg .f
                                                                 .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(net_rating_Center60gameWindow = net_rating_Center60gameWindow - game_net_rating)





opp_net_Rating_window_2018 <- check_together_csv %>%
  select("Date", "Team", "season", "game_net_rating") %>%
  filter(season == "2018-19") %>%
  group_by(Team) %>%
  mutate(opp_net_rating_2gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (2), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_2gameWindow = opp_net_rating_2gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_5gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_5gameWindow = opp_net_rating_5gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_7gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                              #.i = Date,
                                                              .f = ~sum(.x$game_net_rating), # arg .f
                                                              .before = (7), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_7gameWindow = opp_net_rating_7gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_10gameWindow = opp_net_rating_10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_15gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_15gameWindow = opp_net_rating_15gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_20gameWindow = opp_net_rating_20gameWindow - game_net_rating) %>% 
  mutate(opp_net_rating_25gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_25gameWindow = opp_net_rating_25gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_30gameWindow = opp_net_rating_30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_35gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (35), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_35gameWindow = opp_net_rating_35gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                               #.i = Date,
                                                               .f = ~sum(.x$game_net_rating), # arg .f
                                                               .before = (40), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_40gameWindow = opp_net_rating_40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center10gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (5), .after = (5), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center10gameWindow = opp_net_rating_Center10gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center20gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (10), .after = (10), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center20gameWindow = opp_net_rating_Center20gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center30gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (15), .after = (15), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center30gameWindow = opp_net_rating_Center30gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center40gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (20), .after = (20), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center40gameWindow = opp_net_rating_Center40gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center50gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (25), .after = (25), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center50gameWindow = opp_net_rating_Center50gameWindow - game_net_rating) %>%
  mutate(opp_net_rating_Center60gameWindow = round(slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                                                     #.i = Date,
                                                                     .f = ~sum(.x$game_net_rating), # arg .f
                                                                     .before = (30), .after = (30), .complete = F), digits = 5)) %>%
  mutate(opp_net_rating_Center60gameWindow = opp_net_rating_Center60gameWindow - game_net_rating)



together_netRating_updated_2018 <- merge(x = together_netRating_updated_2018, y = opp_net_Rating_window_2018,
                                         by.x = c("Date", "Opponent"), by.y = c("Date", "Team"))

together_netRating_updated_2018 <- together_netRating_updated_2018 %>%
  rename(season = season.x, opp_game_net_rating = game_net_rating.y, game_net_rating = game_net_rating.x, opp_season = season.y)

together_netRating_allCenters <- rbind(together_netRating_updated_2010, together_netRating_updated_2011, together_netRating_updated_2012, 
                                       together_netRating_updated_2013,together_netRating_updated_2014, together_netRating_updated_2015, 
                                       together_netRating_updated_2016, together_netRating_updated_2017, together_netRating_updated_2018)

atl_together <- together_netRating_allCenters %>%
  dplyr::select(Date, Opponent, game_id, Team, season, Distance, Route, game_net_rating, net_rating_Center20gameWindow, net_rating_Center30gameWindow, net_rating_Center40gameWindow, net_rating_Center50gameWindow, net_rating_Center60gameWindow,
                opp_game_net_rating, opp_net_rating_20gameWindow, opp_net_rating_Center20gameWindow, opp_net_rating_Center30gameWindow, opp_net_rating_Center40gameWindow, opp_net_rating_Center50gameWindow, opp_net_rating_Center60gameWindow) %>%
  filter(Team == "Atlanta Hawks")

write.csv(together_netRating_allCenters,
          "./data1/together_net_Rating_allCenters_fixed.csv",
          row.names = FALSE)

check_togetherNetCenters_csv <-read_csv("./data1/together_net_Rating_allCenters_fixed.csv")

together_netRating_allCenters %>%
  

# slider eda --------------------------------------------------------------

dist_5day_wind_2014_15 %>%
  filter(na.rm = TRUE) %>%
  group_by(player_name) %>%
  ggplot(aes(x = `5dayWind`)) +
  geom_density() +
  facet_wrap(~ team_abbreviation) +
  geom_rug(alpha = .3) +
  theme_bw()

dist_5day_wind_2019_20 %>%
  filter(na.rm = TRUE) %>%
  group_by(player_name) %>%
  ggplot(aes(x = `5dayWind`)) +
  geom_density() +
  facet_wrap(~ team_abbreviation) +
  geom_rug(alpha = .3) +
  theme_bw()

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

distance__season_2014_15 <-read_csv("./data1/distance_tracking_data/tracking_by_player_season_daily_2014_15.csv")

dist_5day_wind_2014_15 <- distance__season_2014_15 %>% 
  # ungroup() %>%
  group_by(player_name) %>%
  mutate(`5dayWind` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                        .f = ~sum(.x$dist_miles), # arg .f
                                        .before = 4, .complete = T))


slide_index(x, i, ~.x, .before = lubridate::days(1))

days <- as.numeric(lubridate::days(5))

as.Date(lubridate::days(5))

season <- c("2014", "2015")

slide_dist <- slide(distance__season_2014_15$dist_miles, sum, .before = 5)

slide_dist_date <- slide_index(distance__season_2014_15$dist_miles, distance__season_2014_15$date, sum, .before = 5)

slide_by_player <- slide_dist[[1]] %>%
  group_by(player_id)

slide_by_player <- distance__season_2014_15 %>%
  group_by(player_name) %>%
  pull(dist_miles) %>%
  slide(sum, .before = 5)

dist_5day_wind_2014_15 <- distance__season_2014_15 %>% 
  # ungroup() %>%
  group_by(player_name) %>%
  mutate(`5dayWind` = slider::slide_dbl(.x = cur_data(), # use cur_data() instead of .; arg .x
                                   .f = ~sum(.x$dist_miles), # arg .f
                                   .before = 4, .complete = T))

# net rating --------------------------------------------------------------

netRating_by_day <- function(season){
  
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


write.csv(netRating_season_2014_15,
          "./data1/netRating_tracking_data/tracking_netRating_season_daily_2014_15.csv",
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


lm(score_diff ~ wind_5 + wind_10_hollow_5 + wind_15_hollow + wind_20_hollow +
     wind_25_hollow + wind_30_hollow + wind_35_hollow + wind_40_hollow,
   data = together_net_Rating_allWindows)
