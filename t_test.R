library(fst)
library(tidyverse)
library(dplyr)
library(nbastatR)
library(NBAr)
library(airball)
install.packages("slider")
library(slider)

#Purpose: difference of means between 2gameB2b and other

distance__season_2014_15 <- read_csv("./data1/density_distance_drives_tracking/_density_drives_daily_2014_15.csv")


# not as useful -----------------------------------------------------------
countIncrementalSlides: false
slideNumberFormat: "%current%"


density_distance_2014_2015 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2014_15.csv")
density_distance_2016_2017 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2016_17.csv")
density_distance_2017_2018 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2017_18.csv")
density_distance_2018_2019 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2018_19.csv")
density_distance_2019_2020 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2019_20.csv")

mean(density_distance_2014_2015$min.x)

other_2014 <- density_distance_2014_2015 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > .15)

b2b_2014 <- density_distance_2014_2015 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > .15)

# Difference of means: Wins
mean(other_2014$w)
sd(other_2014$w)
count(other_2014)

mean(b2b_2014$w)
sd(b2b_2014$w)
count(b2b_2014)

### results
#### difference: 0.0470653
#### 95% CI:  0.03138237 to 0.06274822
#### t-stat: 5.8828
#### p value: 0.000000004187

t.test(other_2014$w, b2b_2014$w)


# Difference of means: dist_miles
mean(other_2014$dist_miles)
sd(other_2014$dist_miles) 
count(other_2014)

mean(b2b_2014$dist_miles) 
sd(b2b_2014$dist_miles) 
count(b2b_2014) 

### results
#### difference: .025 
#### standard error: .011
#### 95% CI: .0038 to .0459
#### t-stat: 2.319
#### p value:  0.01881

t.test(other_2014$dist_miles, b2b_2014$dist_miles)


# Difference of means: avg_speed
mean(other_2014$avg_speed) # 4.184568
sd(other_2014$avg_speed) # 0.4928722
count(other_2014) # 19089

mean(b2b_2014$avg_speed) # 4.16994
sd(b2b_2014$avg_speed) # 0.4802833
count(b2b_2014) #  5797

### results
#### difference: -.015
#### standard error: .007
#### 95% CI: 0.0004230504 0.0288338534
#### t-stat: 2.0186
#### p value:  0.04356

t.test(other_2014$avg_speed, b2b_2014$avg_speed)


# Difference of means: dist_miles_off
mean(other_2014$dist_miles_off, na.rm = TRUE) # 0.8552649
sd(other_2014$dist_miles_off, na.rm = TRUE) # 0.3889012
count(other_2014, na.rm = TRUE) # 19089

mean(b2b_2014$dist_miles_off, na.rm = TRUE) # 0.8727627
sd(b2b_2014$dist_miles_off, na.rm = TRUE) # 0.3818327
count(b2b_2014) #  5797

### results
#### difference:  -0.0174978
#### standard error: .006
#### 95% CI:  -0.028772899 to -0.006222707
#### t-stat: -3.042
#### p value:   0.002356

t.test(other_2014$dist_miles_off, b2b_2014$dist_miles_off)


# Difference of means: dist_miles_def
mean(other_2014$dist_miles_def, na.rm = TRUE) # 0.7441286
sd(other_2014$dist_miles_def, na.rm = TRUE) # 0.3371302
count(other_2014, na.rm = TRUE) # 19089

mean(b2b_2014$dist_miles_def, na.rm = TRUE) # 0.7517035
sd(b2b_2014$dist_miles_def, na.rm = TRUE) # 0.3267745
count(b2b_2014) #  5797

### results
#### difference:  -0.0075749
#### standard error: .005
#### 95% CI:   -0.017254676 to 0.002104882
#### t-stat: -1.534
#### p value:   0.1251

t.test(other_2014$dist_miles_def, b2b_2014$dist_miles_def)


# Difference of means: avg_speed_off
mean(other_2014$avg_speed_off) # 4.473546
sd(other_2014$avg_speed_off) # 0.760661
count(other_2014) # 19089

mean(b2b_2014$avg_speed_off) # 4.452063
sd(b2b_2014$avg_speed_off) # 0.7119186
count(b2b_2014) #  5797

### results
#### difference: - 0.021483
#### standard error: .011
#### 95% CI: 0.000213129 to 0.042752641
#### t-stat: 1.9798
#### p value:  0.04775

t.test(other_2014$avg_speed_off, b2b_2014$avg_speed_off)


# Difference of means: avg_speed_def
mean(other_2014$avg_speed_def) # 3.876013
sd(other_2014$avg_speed_def) # 0.720265
count(other_2014) # 19089

mean(b2b_2014$avg_speed_def) # 3.874397
sd(b2b_2014$avg_speed_def) # 0.665046
count(b2b_2014) #  5797

### results
#### difference: 0.001616
#### standard error: .011
#### 95% CI:  -0.01832362 to 0.02155519
#### t-stat: 0.15884
#### p value:  0.8738

t.test(other_2014$avg_speed_def, b2b_2014$avg_speed_def)

# Difference of means: drives
mean(other_2014$drives) # 3.329195
sd(other_2014$drives) # 4.289584
count(other_2014) # 19089

mean(b2b_2014$drives) # 3.507849
sd(b2b_2014$drives) # 4.46416
count(b2b_2014) #  5797

### results
#### difference: -0.178654
#### standard error: .065
#### 95% CI:  -0.3087054 -0.0486027
#### t-stat: -2.6928
#### p value:   0.007098

t.test(other_2014$drives, b2b_2014$drives)

# Difference of means: drives pts
mean(other_2014$drive_pts_pct) 
sd(other_2014$drive_pts_pct) 
count(other_2014) 

mean(b2b_2014$drive_pts_pct) 
sd(b2b_2014$drives_pts_pct) 
count(b2b_2014) 

### results
#### difference:  0.0017305
#### 95% CI:   -0.01420476 to 0.01766573
#### t-stat: 0.21287
#### p value:   0.8314

t.test(other_2014$drive_pts_pct, b2b_2014$drive_pts_pct)

# Difference of means: drives passes
mean(other_2014$drive_passes_pct) 
sd(other_2014$drive_passes_pct) 
count(other_2014) 

mean(b2b_2014$drive_passes_pct) 
sd(b2b_2014$drives_passes_pct) 
count(b2b_2014) 

### results
#### difference: -0.0087627
#### 95% CI:   -0.0172300664 to -0.0002952638
#### t-stat: -2.0286
#### p value:   0.04253

t.test(other_2014$drive_passes_pct, b2b_2014$drive_passes_pct)

# Difference of means: drives tov
mean(other_2014$drive_passes_pct) 
sd(other_2014$drive_passes_pct) 
count(other_2014) 

mean(b2b_2014$drive_passes_pct) 
sd(b2b_2014$drives_passes_pct) 
count(b2b_2014) 

### results
#### difference:  0.00109085
#### 95% CI:   -0.003541246 to 0.005722947
#### t-stat: .46162
#### p value: .6444

t.test(other_2014$drive_tov_pct, b2b_2014$drive_tov_pct)

#Significant


# 2016-2017 ----
density_distance_2016_2017 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2016_17.csv")


other_2016 <- density_distance_2016_2017 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > .15)

b2b_2016 <- density_distance_2016_2017 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > .15)

# Difference of means: Wins
mean(other_2016$w) 
sd(other_2016$w)
count(other_2016) 

mean(b2b_2016$w) 
sd(b2b_2016$w) 
count(b2b_2016)

### results
#### difference: 0.0990972
#### standard error: .008
#### 95% CI: 0.08378768 to 0.11440678
#### t-stat: 12.689
#### p value: < 0.00000000000000022

t.test(other_2016$w, b2b_2016$w)


# Difference of means: dist_miles
mean(other_2016$dist_miles) 
sd(other_2016$dist_miles) 
count(other_2016) 

mean(b2b_2016$dist_miles) 
sd(b2b_2016$dist_miles)
count(b2b_2016) 

### results
#### difference:  0.006453
#### 95% CI: -0.01543932 to 0.02834455
#### t-stat: 0.57779
#### p value:  0.5634

t.test(other_2016$dist_miles, b2b_2016$dist_miles)


# Difference of means: avg_speed
mean(other_2016$avg_speed) 
sd(other_2016$avg_speed) 
count(other_2016) 

mean(b2b_2016$avg_speed) 
sd(b2b_2016$avg_speed) 
count(b2b_2016) 

### results
#### difference: 0.012398
#### 95% CI: -0.001179073 to 0.025975544
#### t-stat: 1.79
#### p value:  0.07349

t.test(other_2016$avg_speed, b2b_2016$avg_speed)


# Difference of means: dist_miles_off
mean(other_2016$dist_miles_off, na.rm = TRUE) 
sd(other_2016$dist_miles_off, na.rm = TRUE) 
count(other_2016, na.rm = TRUE) 

mean(b2b_2016$dist_miles_off, na.rm = TRUE) 
sd(b2b_2016$dist_miles_off, na.rm = TRUE)
count(b2b_2016) 

### results
#### difference:  -0.0011984
#### 95% CI:  -0.01313047 - 0.01073379
#### t-stat: -0.19687
#### p value:   0.8439

t.test(other_2016$dist_miles_off, b2b_2016$dist_miles_off)


# Difference of means: dist_miles_def
mean(other_2016$dist_miles_def, na.rm = TRUE) 
sd(other_2016$dist_miles_def, na.rm = TRUE) 
count(other_2016, na.rm = TRUE) 

mean(b2b_2016$dist_miles_def, na.rm = TRUE) 
sd(b2b_2016$dist_miles_def, na.rm = TRUE) 
count(b2b_2016)

### results
#### difference:  0.0078398
#### 95% CI:   -0.002318003  to 0.017997573
#### t-stat: 1.59129
#### p value:   0.1303

t.test(other_2016$dist_miles_def, b2b_2016$dist_miles_def)


# Difference of means: avg_speed_off
mean(other_2016$avg_speed_off) 
sd(other_2016$avg_speed_off) 
count(other_2016) 

mean(b2b_2016$avg_speed_off)
sd(b2b_2016$avg_speed_off) 
count(b2b_2016)

### results
#### difference: 0.02097
#### 95% CI:  -0.00166439 to 0.04360358
#### t-stat: 1.8161
#### p value:   0.06939

t.test(other_2016$avg_speed_off, b2b_2016$avg_speed_off)


# Difference of means: avg_speed_def
mean(other_2016$avg_speed_def) 
sd(other_2016$avg_speed_def)
count(other_2016) 

mean(b2b_2016$avg_speed_def) 
sd(b2b_2016$avg_speed_def) 
count(b2b_2016) 

### results
#### difference: 0.006599
#### 95% CI:   -0.01522227 to 0.02842150
#### t-stat: 0.59285
#### p value:  0.5533

t.test(other_2016$avg_speed_def, b2b_2016$avg_speed_def)

# Difference of means: drives
mean(other_2016$drives) 
sd(other_2016$drives) 
count(other_2016)

mean(b2b_2016$drives) 
sd(b2b_2016$drives) 
count(b2b_2016) 

### results
#### difference: -0.009952
#### 95% CI:   -0.1486399  0.1287344
#### t-stat: -0.14068
#### p value:  0.8881

t.test(other_2016$drives, b2b_2016$drives)

# Difference of means:  pts drives
mean(other_2016$drive_pts_pct) 
sd(other_2016$drive_pts_pct) 
count(other_2016)

mean(b2b_2016$drive_pts_pct) 
sd(b2b_2016$drive_pts_pct) 
count(b2b_2016) 

### results
#### difference: 0.0120241
#### 95% CI:   -0.004884035  0.028932411
#### t-stat: 1.394
#### p value:  0.1633

t.test(other_2016$drive_pts_pct, b2b_2016$drive_pts_pct)

# Difference of means:  pass drives
mean(other_2016$drive_passes_pct) 
sd(other_2016$drive_passes_pct) 
count(other_2016)

mean(b2b_2016$drive_passes_pct) 
sd(b2b_2016$drive_passes_pct) 
count(b2b_2016) 

### results
#### difference: -0.00061
#### 95% CI:   -0.009576093 to 0.008356127
#### t-stat: -0.13336
#### p value:  0.8939

t.test(other_2016$drive_passes_pct, b2b_2016$drive_passes_pct)

# Difference of means:  tov drives
mean(other_2016$drive_passes_pct) 
sd(other_2016$drive_passes_pct) 
count(other_2016)

mean(b2b_2016$drive_passes_pct) 
sd(b2b_2016$drive_passes_pct) 
count(b2b_2016) 

### results
#### difference: -0.00225405
#### 95% CI:   -0.007092616 to 0.002584506
#### t-stat: -0.9132
#### p value: 0.3612

t.test(other_2016$drive_tov_pct, b2b_2016$drive_tov_pct)


#Significant
t.test(other_2016$w, b2b_2016$w)

t.test(other_2016$l, b2b_2016$l)


##not sig
t.test(other_2016$min.x, b2b_2016$min.x)

t.test(other_2016$drive_fga, b2b_2016$drive_fga)

t.test(other_2016$drives, b2b_2016$drives)

t.test(other_2016$dist_miles, b2b_2016$dist_miles)

t.test(other_2016$dist_miles_off, b2b_2016$dist_miles_off)


#Not significant
t.test(other_2016$dist_miles_def, b2b_2016$dist_miles_def)

t.test(other_2016$avg_speed, b2b_2016$avg_speed)

t.test(other_2016$avg_speed_def, b2b_2016$avg_speed_def)

t.test(other_2016$avg_speed_off, b2b_2016$avg_speed_off)

t.test(other_2016$drive_pts, b2b_2016$drive_pts)

t.test(other_2016$drive_fgm, b2b_2016$drive_fgm)


#2017-2018
density_distance_2017_2018 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2017_18.csv")


other_2017 <- density_distance_2017_2018 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > .15)

b2b_2017 <- density_distance_2017_2018 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > .15)

# Difference of means: Wins
mean(other_2017$w) 
sd(other_2017$w)
count(other_2017) 

mean(b2b_2017$w) 
sd(b2b_2017$w) 
count(b2b_2017)

### results
#### difference: 0.0724824
#### 95% CI: 0.05635958 to 0.08860506
#### t-stat: 8.813
#### p value:  < 0.00000000000000022

t.test(other_2017$w, b2b_2017$w)


# Difference of means: dist_miles
mean(other_2017$dist_miles) 
sd(other_2017$dist_miles) 
count(other_2017) 

mean(b2b_2017$dist_miles) 
sd(b2b_2017$dist_miles)
count(b2b_2017) 

### results
#### difference:   0.01471
#### 95% CI: -0.008262476 to 0.037681797
#### t-stat: 1.2552
#### p value:  0.2094

t.test(other_2017$dist_miles, b2b_2017$dist_miles)


# Difference of means: avg_speed
mean(other_2017$avg_speed) 
sd(other_2017$avg_speed) 
count(other_2017) 

mean(b2b_2017$avg_speed) 
sd(b2b_2017$avg_speed) 
count(b2b_2017) 

### results
#### difference: 0.001077
#### 95% CI: -0.01454146 to 0.01669537
#### t-stat: 0.13517
#### p value:  0.8925

t.test(other_2017$avg_speed, b2b_2017$avg_speed)


# Difference of means: dist_miles_off
mean(other_2017$dist_miles_off, na.rm = TRUE) 
sd(other_2017$dist_miles_off, na.rm = TRUE) 
count(other_2017, na.rm = TRUE) 

mean(b2b_2017$dist_miles_off, na.rm = TRUE) 
sd(b2b_2017$dist_miles_off, na.rm = TRUE)
count(b2b_2017) 

### results
#### difference:  0.0044839
#### 95% CI:  -0.008002476  0.016970168
#### t-stat: 0.70395
#### p value:  0.4815

t.test(other_2017$dist_miles_off, b2b_2017$dist_miles_off)


# Difference of means: dist_miles_def
mean(other_2017$dist_miles_def, na.rm = TRUE) 
sd(other_2017$dist_miles_def, na.rm = TRUE) 
count(other_2017, na.rm = TRUE) 

mean(b2b_2017$dist_miles_def, na.rm = TRUE) 
sd(b2b_2017$dist_miles_def, na.rm = TRUE) 
count(b2b_2017)

### results
#### difference:  0.0105469
#### 95% CI:  -0.0001550702  0.0212489348
#### t-stat: 1.9319
#### p value:   0.05341

t.test(other_2017$dist_miles_def, b2b_2017$dist_miles_def)


# Difference of means: avg_speed_off
mean(other_2017$avg_speed_off) 
sd(other_2017$avg_speed_off) 
count(other_2016) 

mean(b2b_2017$avg_speed_off)
sd(b2b_2017$avg_speed_off) 
count(b2b_2017)

### results
#### difference: 0.00182
#### 95% CI:  -0.02149684  0.02513625
#### t-stat: 0.15299
#### p value:   0.8784

t.test(other_2017$avg_speed_off, b2b_2017$avg_speed_off)


# Difference of means: avg_speed_def
mean(other_2017$avg_speed_def) 
sd(other_2017$avg_speed_def)
count(other_2017) 

mean(b2b_2017$avg_speed_def) 
sd(b2b_2017$avg_speed_def) 
count(b2b_2017) 

### results
#### difference: -0.022094
#### 95% CI:  -0.0448221238 to 0.0006332462
#### t-stat: -1.9057
#### p value:   0.05673

t.test(other_2017$avg_speed_def, b2b_2017$avg_speed_def)

# Difference of means: drives
mean(other_2017$drives) 
sd(other_2017$drives) 
count(other_2017)

mean(b2b_2017$drives) 
sd(b2b_2017$drives) 
count(b2b_2017) 

### results
#### difference: -0.015693
#### 95% CI:  -0.1625306 to 0.1311447
#### t-stat:  -0.20951
#### p value:  0.8341

t.test(other_2017$drives, b2b_2017$drives)

# Difference of means:  pts drives
mean(other_2017$drive_pts_pct) 
sd(other_2017$drive_pts_pct) 
count(other_2017)

mean(b2b_2017$drive_pts_pct) 
sd(b2b_2017$drive_pts_pct) 
count(b2b_2017) 

### results
#### difference: -0.0024749
#### 95% CI:   -0.02024233 to 0.01529248
#### t-stat: -0.27307
#### p value:  0.7848

t.test(other_2017$drive_pts_pct, b2b_2017$drive_pts_pct)

# Difference of means:  pass drives
mean(other_2017$drive_passes_pct) 
sd(other_2017$drive_passes_pct) 
count(other_2017)

mean(b2b_2017$drive_passes_pct) 
sd(b2b_2017$drive_passes_pct) 
count(b2b_2017) 

### results
#### difference: 0.0019073
#### 95% CI:   -0.00694759  to 0.01076227
#### t-stat: 0.42225
#### p value:  0.0.6729

t.test(other_2017$drive_passes_pct, b2b_2017$drive_passes_pct)

# Difference of means:  tov drives
mean(other_2017$drive_passes_pct) 
sd(other_2017$drive_passes_pct) 
count(other_2017)

mean(b2b_2017$drive_passes_pct) 
sd(b2b_2017$drive_passes_pct) 
count(b2b_2017) 

### results
#### difference: -0.004084
#### 95% CI:   -0.0090403321 to 0.0008723399
#### t-stat: -1.6153
#### p value: 0.1063

t.test(other_2017$drive_tov_pct, b2b_2017$drive_tov_pct)




#Significant
t.test(other_2017$w, b2b_2017$w)

t.test(other_2017$l, b2b_2017$l)

t.test(other_2017$drive_tov, b2b_2017$drive_tov)


#Not sig
t.test(other_2017$min.x, b2b_2017$min.x)

t.test(other_2017$drive_fga, b2b_2017$drive_fga)

t.test(other_2017$drives, b2b_2017$drives)

t.test(other_2017$dist_miles, b2b_2017$dist_miles)

t.test(other_2017$dist_miles_off, b2b_2017$dist_miles_off)


#Not significant
t.test(other_2017$dist_miles_def, b2b_2017$dist_miles_def)

t.test(other_2017$avg_speed, b2b_2017$avg_speed)

t.test(other_2017$avg_speed_def, b2b_2017$avg_speed_def)

t.test(other_2017$avg_speed_off, b2b_2017$avg_speed_off)

t.test(other_2017$drive_pts, b2b_2017$drive_pts)

t.test(other_2017$drive_fgm, b2b_2017$drive_fgm)


#2018-2019
density_distance_2018_2019 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2018_19.csv")


other_2018 <- density_distance_2018_2019 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > .15)

b2b_2018 <- density_distance_2018_2019 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > .15)

# Difference of means: Wins

### results
#### difference: 0.0795501
#### 95% CI:  0.06274648 to 0.09635360
#### t-stat: 9.2807
#### p value:  < 0.00000000000000022

t.test(other_2018$w, b2b_2018$w)


# Difference of means: dist_miles

### results
#### difference:   -0.004582
#### 95% CI:  -0.02935900 to 0.02019476
#### t-stat: -0.36254
#### p value:  0.717

t.test(other_2018$dist_miles, b2b_2018$dist_miles)


# Difference of means: avg_speed

### results
#### difference: 0.005874
#### 95% CI: -0.008300174 to 0.020048844
#### t-stat: 0.81244
#### p value:  0.4166

t.test(other_2018$avg_speed, b2b_2018$avg_speed)


# Difference of means: dist_miles_off

### results
#### difference:  -0.0058477
#### 95% CI:  -0.019475528 to 0.007780239
#### t-stat: -0.84119
#### p value:  0.4003

t.test(other_2018$dist_miles_off, b2b_2018$dist_miles_off)


# Difference of means: dist_miles_def

### results
#### difference:  0.0012063
#### 95% CI:   -0.01018912 to 0.01260183
#### t-stat: 0.20753
#### p value:   0.8356

t.test(other_2018$dist_miles_def, b2b_2018$dist_miles_def)


# Difference of means: avg_speed_off

### results
#### difference: -0.013747
#### 95% CI:  -0.035320933 to 0.007827046
#### t-stat: -1.2491
#### p value: 0.2117

t.test(other_2018$avg_speed_off, b2b_2018$avg_speed_off)


# Difference of means: avg_speed_def

### results
#### difference: 0.012454
#### 95% CI:  -0.008262207 to 0.033169160
#### t-stat: 1.1785
#### p value:   0.2386

t.test(other_2018$avg_speed_def, b2b_2018$avg_speed_def)

# Difference of means: drives

### results
#### difference: -0.074584
#### 95% CI:  -0.23869811 to  0.08953072
#### t-stat:  -0.89092
#### p value:  0.373

t.test(other_2018$drives, b2b_2018$drives)

# Difference of means:  pts drives

### results
#### difference: -0.0142751
#### 95% CI:    -0.033046052 to 0.004495734
#### t-stat: -1.4909
#### p value:  0.1361

t.test(other_2018$drive_pts_pct, b2b_2018$drive_pts_pct)

# Difference of means:  pass drives

### results
#### difference: -0.0024423
#### 95% CI:   -0.012850704 to 0.007966113
#### t-stat: -0.46
#### p value:   0.6455

t.test(other_2018$drive_passes_pct, b2b_2018$drive_passes_pct)

# Difference of means:  tov drives

### results
#### difference: -0.00116541
#### 95% CI:   -0.006399435 to 0.004068615
#### t-stat: -0.4365
#### p value: 0.6625

t.test(other_2018$drive_tov_pct, b2b_2018$drive_tov_pct)




#Significant
t.test(other_2018$w, b2b_2018$w)

t.test(other_2018$l, b2b_2018$l)

t.test(other_2018$drive_ft_pct, b2b_2018$drive_ft_pct)

t.test(other_2018$avg_speed_off, b2b_2017$avg_speed_off)

#Not sig
t.test(other_2018$min.x, b2b_2018$min.x)

t.test(other_2018$drive_fga, b2b_2018$drive_fga)

t.test(other_2018$drives, b2b_2018$drives)

t.test(other_2018$dist_miles, b2b_2018$dist_miles)

t.test(other_2018$dist_miles_off, b2b_2018$dist_miles_off)


#Not significant
t.test(other_2018$dist_miles_def, b2b_2018$dist_miles_def)

t.test(other_2018$avg_speed, b2b_2018$avg_speed)

t.test(other_2018$avg_speed_def, b2b_2018$avg_speed_def)

t.test(other_2018$drive_pts, b2b_2018$drive_pts)

t.test(other_2018$drive_fgm, b2b_2018$drive_fgm)


#2019-2020
density_distance_2019_2020 <-read_csv("./data1/density_distance_drives_tracking/_density_drives_distance_daily_2019_20.csv")


other_2019 <- density_distance_2019_2020 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > .15)

b2b_2019 <- density_distance_2019_2020 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > .15)

# Difference of means: Wins

### results
#### difference: 0.0306209
#### 95% CI:   0.01091205 to 0.05032968
#### t-stat: 3.046
#### p value:  0.002334

t.test(other_2019$w, b2b_2019$w)


# Difference of means: dist_miles

### results
#### difference:  -0.002638
#### 95% CI:   -0.03173763 to 0.02646264
#### t-stat: -0.1777
#### p value:  0.859

t.test(other_2019$dist_miles, b2b_2019$dist_miles)


# Difference of means: avg_speed

### results
#### difference:  0.023112
#### 95% CI:  0.007139764 to 0.039082999
#### t-stat:  2.837
#### p value:  0.004577

t.test(other_2019$avg_speed, b2b_2019$avg_speed)


# Difference of means: dist_miles_off

### results
#### difference:  -0.0074989
#### 95% CI: -0.023591184  to 0.008593355
#### t-stat: -0.91362
#### p value:   0.361

t.test(other_2019$dist_miles_off, b2b_2019$dist_miles_off)


# Difference of means: dist_miles_def

### results
#### difference:   0.0053719
#### 95% CI:   -0.007907032 to 0.018650816
#### t-stat:  0.79313
#### p value:   0.4277

t.test(other_2019$dist_miles_def, b2b_2019$dist_miles_def)


# Difference of means: avg_speed_off

### results
#### difference: 0.042446
#### 95% CI:  0.01741449 to 0.06747794
#### t-stat: 3.3245
#### p value: 0.0008937

t.test(other_2019$avg_speed_off, b2b_2019$avg_speed_off)


# Difference of means: avg_speed_def

### results
#### difference:  0.002945

#### 95% CI:  -0.02143285 to 0.02732342
#### t-stat: 0.23687
#### p value:   0.8128

t.test(other_2019$avg_speed_def, b2b_2019$avg_speed_def)

# Difference of means: drives

### results
#### difference: -0.087006
#### 95% CI:   -0.2927766 to 0.1187646
#### t-stat:  -0.82898
#### p value:  0.4072

t.test(other_2019$drives, b2b_2019$drives)

# Difference of means:  pts drives

### results
#### difference:  0.0124287
#### 95% CI:   -0.008538416 to 0.033395867
#### t-stat:  1.1622
#### p value: 0.2452

t.test(other_2019$drive_pts_pct, b2b_2019$drive_pts_pct)

# Difference of means:  pass drives

### results
#### difference: -0.0132657
#### 95% CI:  -0.025479802 to -0.001051583
#### t-stat: -2.1294
#### p value:   0.03329

t.test(other_2019$drive_passes_pct, b2b_2019$drive_passes_pct)

# Difference of means:  tov drives

### results
#### difference: -0.0011094
#### 95% CI:    -0.007219367  0.005000557
#### t-stat: -0.35599
#### p value: 0.7219

t.test(other_2019$drive_tov_pct, b2b_2019$drive_tov_pct)


#Significant
t.test(other_2019$w, b2b_2019$w)

t.test(other_2019$l, b2b_2019$l)

t.test(other_2019$drive_passes_pct, b2b_2019$drive_passes_pct)

t.test(other_2019$avg_speed, b2b_2019$avg_speed)

t.test(other_2019$avg_speed_off, b2b_2019$avg_speed_off)

#Not sig
t.test(other_2019$min.x, b2b_2019$min.x)

t.test(other_2019$drive_fga, b2b_2019$drive_fga)

t.test(other_2019$drives, b2b_2019$drives)

t.test(other_2019$dist_miles, b2b_2019$dist_miles)

t.test(other_2019$dist_miles_off, b2b_2019$dist_miles_off)


#Not significant
t.test(other_2019$dist_miles_def, b2b_2019$dist_miles_def)

t.test(other_2019$avg_speed_def, b2b_2019$avg_speed_def)

t.test(other_2019$avg_speed_off, b2b_2019$avg_speed_off)

t.test(other_2019$drive_pts, b2b_2019$drive_pts)

t.test(other_2019$drive_fgm, b2b_2019$drive_fgm)


# together and teams ------------------------------------------------------



all_other <- rbind(other_2014, other_2016, other_2017, other_2018, other_2019) 

all_b2b <- rbind(b2b_2014, b2b_2016, b2b_2017, b2b_2018, b2b_2019)

other_2014 <- density_distance_2014_2015 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10)

b2b_2014 <- density_distance_2014_2015 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10)


other_2016 <- density_distance_2016_2017 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10)

b2b_2016 <- density_distance_2016_2017 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10)


other_2017 <- density_distance_2017_2018 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10)

b2b_2017 <- density_distance_2017_2018 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10)


other_2018 <- density_distance_2018_2019 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10)

b2b_2018 <- density_distance_2018_2019 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10)


other_2019 <- density_distance_2019_2020 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10)

b2b_2019 <- density_distance_2019_2020 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10)


# Win difference over years
t.test(other_2014$w, b2b_2014$w)

t.test(other_2016$w, b2b_2016$w)

t.test(other_2017$w, b2b_2017$w)

t.test(other_2018$w, b2b_2018$w)

t.test(other_2019$w, b2b_2019$w)

t.test(all_other$w, all_b2b$w)


# dist_miles over years

## t.test(other_2014$dist_miles, b2b_2014$dist_miles)

## t.test(other_2016$dist_miles, b2b_2016$dist_miles)

t.test(other_2017$dist_miles, b2b_2017$dist_miles)

t.test(other_2018$dist_miles, b2b_2018$dist_miles)

## t.test(other_2019$dist_miles, b2b_2019$dist_miles)

t.test(all_other$dist_miles, all_b2b$dist_miles)


# dist_miles_def over years (non of the dist_miles_off were significant)

## t.test(other_2014$dist_miles_def, b2b_2014$dist_miles_def)

t.test(other_2016$dist_miles_def, b2b_2016$dist_miles_def)

t.test(other_2017$dist_miles_def, b2b_2017$dist_miles_def)

t.test(other_2018$dist_miles_def, b2b_2018$dist_miles_def)

## t.test(other_2019$dist_miles_def, b2b_2019$dist_miles_def)

t.test(all_other$dist_miles_def, all_b2b$dist_miles_def)

t.test(all_other$dist_miles_off, all_b2b$dist_miles_off)

# avg_speed over years

t.test(other_2014$avg_speed, b2b_2014$avg_speed)

t.test(other_2016$avg_speed, b2b_2016$avg_speed)

## t.test(other_2017$avg_speed, b2b_2017$avg_speed)

## t.test(other_2018$avg_speed, b2b_2018$avg_speed)

t.test(other_2019$avg_speed, b2b_2019$avg_speed)

t.test(all_other$avg_speed, all_b2b$avg_speed)

# avg_speed_off over years

t.test(other_2014$avg_speed_off, b2b_2014$avg_speed_off)

t.test(other_2016$avg_speed_off, b2b_2016$avg_speed_off)

t.test(other_2017$avg_speed_off, b2b_2017$avg_speed_off)

# t.test(other_2018$avg_speed_off, b2b_2018$avg_speed_off)

t.test(other_2019$avg_speed_off, b2b_2019$avg_speed_off)

t.test(all_other$avg_speed_off, all_b2b$avg_speed_off)


# avg_speed_def over years

# t.test(other_2014$avg_speed_def, b2b_2014$avg_speed_def)

t.test(other_2016$avg_speed_def, b2b_2016$avg_speed_def)

# t.test(other_2017$avg_speed_def, b2b_2017$avg_speed_def)

# t.test(other_2018$avg_speed_def, b2b_2018$avg_speed_def)

t.test(other_2019$avg_speed_def, b2b_2019$avg_speed_def)

#t.test(all_other$avg_speed_def, all_b2b$avg_speed_def)


# drives pass pct over years (drove tov and passes wasnt significant)

# t.test(other_2014$drive_passes_pct, b2b_2014$drive_passes_pct)

# t.test(other_2016$drive_passes_pct, b2b_2016$drive_passes_pct)

# t.test(other_2017$drive_passes_pct, b2b_2017$drive_passes_pct)

# t.test(other_2018$drive_passes_pct, b2b_2018$drive_passes_pct)

t.test(other_2019$drive_passes_pct, b2b_2019$drive_passes_pct)

# t.test(all_other$drive_passes_pct, all_b2b$drive_passes_pct)

t.test(all_other$drive_pts_pct, all_b2b$drive_pts_pct)

together_final <- read.csv("./data1/final_together.csv")

team_b2b_vis <- together_final %>%
  filter(b2b_2nd == TRUE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))

team_other_vis <- together_final %>%
  filter(b2b_2nd == FALSE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))





team_3in4_vis <- together_final %>%
  filter(three_in_four == TRUE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))

team_other3in4_vis <- together_final %>%
  filter(three_in_four == FALSE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))


team_b2b_3in4_vis <- together_final %>%
  filter(b2b_2nd == TRUE) %>%
  filter(three_in_four == TRUE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))

team_otherb2b_3in4_vis <- together_final %>%
  filter(b2b_2nd == FALSE) %>%
  filter(three_in_four == FALSE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))



team_3hoursforward_vis <- together_final %>%
  filter(travel_3_hours_forward == TRUE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))

team_other_3hoursforward_vis <- together_final %>%
  filter(travel_3_hours_forward == FALSE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))

team_3hoursback_vis <- together_final %>%
  filter(travel_3_hours_back == TRUE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))

team_other_3hoursback_vis <- together_final %>%
  filter(travel_3_hours_back == FALSE) %>%
  mutate(Win = ifelse(Win == TRUE, 1, 0))


t_b2b<- t.test(team_otherb2b_vis$game_net_rating, team_b2b_vis$game_net_rating)

t_3in4 <-t.test(team_other3in4_vis$game_net_rating, team_3in4_vis$game_net_rating)

t_b2b_3in4 <- t.test(team_otherb2b_3in4_vis$game_net_rating, team_b2b_3in4_vis$game_net_rating)

t_3hrsForward <- t.test(team_other_3hoursforward_vis$game_net_rating, team_3hoursforward_vis$game_net_rating)

t_3hrsBack <- t.test(team_other_3hoursback_vis$game_net_rating, team_3hoursback_vis$game_net_rating)




Test <- c("2nd_B2B", "Three_in_Four", "both", "3hrsBack")

Test <- ordered(Test, levels = c("2nd_B2B", "Three_in_Four", "both", "3hrsBack"))

Mean_of_X <- c(t_b2b$estimate[1], t_3in4$estimate[1], t_b2b_3in4$estimate[1], t_3hrsBack$estimate[1])

Mean_of_Y <- c(t_b2b$estimate[2], t_3in4$estimate[2], t_b2b_3in4$estimate[2], t_3hrsBack$estimate[2])

P_Value <- c(t_b2b$p.value, t_3in4$p.value, t_b2b_3in4$p.value, t_3hrsBack$p.value)

standard_error <- c(sd(together_final$game_net_rating) / sqrt(length(together_final$game_net_rating)),
                    sd(together_final$game_net_rating) / sqrt(length(together_final$game_net_rating)),
                    sd(together_final$game_net_rating) / sqrt(length(together_final$game_net_rating)),
                    sd(together_final$game_net_rating) / sqrt(length(together_final$game_net_rating)))
       
                                      
team_t_test_info <- ordered(Test, levels = c("2nd_B2B", "Three_in_Four", "b2b_and3in4", "3hrsBack"))
team_t_test_info <- data.frame(Test, Mean_of_X, Mean_of_Y, P_Value, standard_error)

write_csv(team_t_test_info,
          "./data1/team_t_test_info.csv")

ggplot(data = team_t_test_info) +
  geom_point(aes(x = Test, y = Mean_of_X), color = "brown", size = 2) +
  geom_point(aes(x = Test, y = Mean_of_Y), color = "blue", size = 2) +
  labs(color = "Legend") +
  #geom_text_repel(aes(label=p), size = 2.5) + 
  geom_errorbar(aes(x = Test, ymin = Mean_of_X - 2*standard_error, ymax = Mean_of_X + 2*standard_error), color = "brown", width = 0.2) +
  geom_errorbar(aes(x = Test, ymin = Mean_of_Y - 2*standard_error, ymax = Mean_of_Y + 2*standard_error), color = "blue", width = 0.2) +
  xlab("Season") +
  ylab("Game Net Rating Mean") +
  labs(title = "Difference of Mean between fatigue related metrics") +
  theme(panel.background = element_rect(fill = "burlywood"),
        panel.grid.major=element_blank())  +
  theme(legend.position = "bottom")


sd(together_final$game_net_rating) / sqrt(length(together_final$game_net_rating))

colnames(together_final)

other_2014 <- density_distance_2014_2015 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10) %>%
  na.omit()

b2b_2014 <- density_distance_2014_2015 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10) %>%
  na.omit()
  

other_2016 <- density_distance_2016_2017 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10) %>%
  na.omit()

b2b_2016 <- density_distance_2016_2017 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10) %>%
  na.omit()

other_2017 <- density_distance_2017_2018 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10) %>%
  na.omit()
  

b2b_2017 <- density_distance_2017_2018 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10) %>%
  na.omit()


other_2018 <- density_distance_2018_2019 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10) %>%
  na.omit()

b2b_2018 <- density_distance_2018_2019 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10) %>%
  na.omit()


other_2019 <- density_distance_2019_2020 %>%
  filter(`B2B-2nd` == "No") %>%
  filter(min.x > 10) %>%
  na.omit()

b2b_2019 <- density_distance_2019_2020 %>%
  filter(`B2B-2nd` == "Yes") %>%
  filter(min.x > 10) %>%
  na.omit()

all_other <- rbind(other_2014, other_2016, other_2017, other_2018, other_2019) 

all_b2b <- rbind(b2b_2014, b2b_2016, b2b_2017, b2b_2018, b2b_2019)


t_dist_miles <- t.test(all_other$dist_miles, all_b2b$dist_miles)
t_dist_miles_def <- t.test(all_other$dist_miles_def, all_b2b$dist_miles_def)
t_dist_miles_off <- t.test(all_other$dist_miles_off, all_b2b$dist_miles_off)
t_avg_speed <- t.test(all_other$avg_speed, all_b2b$avg_speed)
t_avg_speed_off <- t.test(all_other$avg_speed_off, all_b2b$avg_speed_off)

player_t_test_info <- tibble(Test = c("dist_miles", "dist_miles_def", 
                                    "dist_miles_off", "avg_speed", 
                                    "avg_speed_off"), 
                           "Mean_of_X" = c(t_dist_miles$estimate[1], t_dist_miles_def$estimate[1],
                                           t_dist_miles_off$estimate[1], t_avg_speed$estimate[1],
                                           t_avg_speed_off$estimate[1]),
                           "Mean_of_Y" = c(t_dist_miles$estimate[2], t_dist_miles_def$estimate[2],
                                           t_dist_miles_off$estimate[2], t_avg_speed$estimate[2],
                                           t_avg_speed_off$estimate[2]),
                           "P_Value" = c(t_dist_miles$p.value, t_dist_miles_def$p.value, 
                                         t_dist_miles_off$p.value, t_avg_speed$p.value, t_avg_speed_off$p.value))
player_info <- tibble(dist_miles = c(t_dist_miles$estimate[1], t_dist_miles$estimate[2]))

density_distance_2014_2015 <- density_distance_2014_2015 %>%
  na.omit() %>%
  filter(min.x > 10)

density_distance_2016_2017 <- density_distance_2016_2017 %>%
  na.omit()

density_distance_2017_2018 <- density_distance_2017_2018 %>%
  na.omit()

density_distance_2018_2019 <- density_distance_2018_2019 %>%
  na.omit()

density_distance_2019_2020 <- density_distance_2019_2020 %>%
  na.omit()
  
other_2014_3in4 <- density_distance_2014_2015 %>%
  filter(`3in4` == "No") %>%
  filter(min.x > 10)

b2b_2014_3in4 <- density_distance_2014_2015 %>%
  filter(`3in4` == "Yes") %>%
  filter(min.x > 10)

other_2016_3in4 <- density_distance_2016_2017 %>%
  filter(`3in4` == "No") %>%
  filter(min.x > 10)

b2b_2016_3in4 <- density_distance_2016_2017 %>%
  filter(`3in4` == "Yes") %>%
  filter(min.x > 10)

other_2017_3in4 <- density_distance_2017_2018 %>%
  filter(`3in4` == "No") %>%
  filter(min.x > 10)

b2b_2017_3in4 <- density_distance_2017_2018 %>%
  filter(`3in4` == "Yes") %>%
  filter(min.x > 10)


other_2018_3in4 <- density_distance_2018_2019 %>%
  filter(`3in4` == "No") %>%
  filter(min.x > 10)

b2b_2018_3in4 <- density_distance_2018_2019 %>%
  filter(`3in4` == "Yes") %>%
  filter(min.x > 10)


other_2019_3in4 <- density_distance_2019_2020 %>%
  filter(`3in4` == "No") %>%
  filter(min.x > 10)

b2b_2019_3in4 <- density_distance_2019_2020 %>%
  filter(`3in4` == "Yes") %>%
  filter(min.x > 10)



all_other_3in4 <- rbind(other_2014_3in4, other_2016_3in4, other_2017_3in4, other_2018_3in4, other_2019_3in4) 

all_b2b_3in4 <- rbind(b2b_2014_3in4, b2b_2016_3in4, b2b_2017_3in4, b2b_2018_3in4, b2b_2019_3in4)

t_dist_miles_3in4 <- t.test(all_other_3in4$dist_miles, all_b2b_3in4$dist_miles)
t_dist_miles_def_3in4 <- t.test(all_other_3in4$dist_miles_def, all_b2b_3in4$dist_miles_def)
t_dist_miles_off_3in4 <- t.test(all_other_3in4$dist_miles_off, all_b2b_3in4$dist_miles_off)
t_avg_speed_3in4 <- t.test(all_other_3in4$avg_speed, all_b2b_3in4$avg_speed)
t_avg_speed_off_3in4 <- t.test(all_other_3in4$avg_speed_off, all_b2b_3in4$avg_speed_off)


player_t_test_info_3in4 <- tibble(Test = c("dist_miles", "dist_miles_def", 
                                      "dist_miles_off", "avg_speed", 
                                      "avg_speed_off"), 
                             "Mean_of_X" = c(t_dist_miles_3in4$estimate[1], t_dist_miles_def_3in4$estimate[1],
                                             t_dist_miles_off_3in4$estimate[1], t_avg_speed_3in4$estimate[1],
                                             t_avg_speed_off_3in4$estimate[1]),
                             "Mean_of_Y" = c(t_dist_miles_3in4$estimate[2], t_dist_miles_def_3in4$estimate[2],
                                             t_dist_miles_off_3in4$estimate[2], t_avg_speed_3in4$estimate[2],
                                             t_avg_speed_off_3in4$estimate[2]),
                             "P_Value" = c(t_dist_miles_3in4$p.value, t_dist_miles_def_3in4$p.value, 
                                           t_dist_miles_off_3in4$p.value, t_avg_speed_3in4$p.value, t_avg_speed_off_3in4$p.value))
player_t_test_info %>%
  ggplot(aes(x = Test, y = c(Mean_of_X - Mean_of_Y))) +
  geom_bar(stat = "Identity") +
  theme_bw()





t_dist_miles_2014 <- t.test(other_2014$dist_miles, b2b_2014$dist_miles)
t_dist_miles_2016 <- t.test(other_2016$dist_miles, b2b_2016$dist_miles)
t_dist_miles_2017 <- t.test(other_2017$dist_miles, b2b_2017$dist_miles)
t_dist_miles_2018 <- t.test(other_2018$dist_miles, b2b_2018$dist_miles)
t_dist_miles_2019 <- t.test(other_2019$dist_miles, b2b_2019$dist_miles)

dist_table <- tibble(season = c("2014-15", "2016-17", "2017-18", "2018-19", "2019-20"),
       other_mean = c(t_dist_miles_2014$estimate[1], t_dist_miles_2016$estimate[1],
                      t_dist_miles_2017$estimate[1], t_dist_miles_2018$estimate[1],
                      t_dist_miles_2019$estimate[1]),
       b2b_mean = c(t_dist_miles_2014$estimate[2], t_dist_miles_2016$estimate[2],
                    t_dist_miles_2017$estimate[2], t_dist_miles_2018$estimate[2],
                    t_dist_miles_2019$estimate[2]),
       standard_error = c(sd(density_distance_2014_2015$dist_miles) / sqrt(length(density_distance_2014_2015$dist_miles)),
                          sd(density_distance_2016_2017$dist_miles) / sqrt(length(density_distance_2016_2017$dist_miles)),
                          sd(density_distance_2017_2018$dist_miles) / sqrt(length(density_distance_2017_2018$dist_miles)),
                          sd(density_distance_2018_2019$dist_miles) / sqrt(length(density_distance_2018_2019$dist_miles)),
                          sd(density_distance_2019_2020$dist_miles) / sqrt(length(density_distance_2019_2020$dist_miles))))

write_csv(dist_table,
          "./data1/distance_t_test_table.csv")

t_speed_2014 <- t.test(other_2014$avg_speed, b2b_2014$avg_speed)
t_speed_2016 <- t.test(other_2016$avg_speed, b2b_2016$avg_speed)
t_speed_2017 <- t.test(other_2017$avg_speed, b2b_2017$avg_speed)
t_speed_2018 <- t.test(other_2018$avg_speed, b2b_2018$avg_speed)
t_speed_2019 <- t.test(other_2019$avg_speed, b2b_2019$avg_speed)

speed_table <- tibble(season = c("2014-15", "2016-17", "2017-18", "2018-19", "2019-20"),
                     other_mean = c(t_speed_2014$estimate[1], t_speed_2016$estimate[1],
                                    t_speed_2017$estimate[1], t_speed_2018$estimate[1],
                                    t_speed_2019$estimate[1]),
                     b2b_mean = c(t_speed_2014$estimate[2], t_speed_2016$estimate[2],
                                  t_speed_2017$estimate[2], t_speed_2018$estimate[2],
                                  t_speed_2019$estimate[2]),
                     standard_error = c(sd(density_distance_2014_2015$avg_speed) / sqrt(length(density_distance_2014_2015$avg_speed)), 
                                        sd(density_distance_2016_2017$avg_speed) / sqrt(length(density_distance_2016_2017$avg_speed)), 
                                        sd(density_distance_2017_2018$avg_speed)/ sqrt(length(density_distance_2017_2018$avg_speed)),
                                        sd(density_distance_2018_2019$avg_speed) / sqrt(length(density_distance_2018_2019$avg_speed)),
                                        sd(density_distance_2019_2020$avg_speed) / sqrt(length(density_distance_2019_2020$avg_speed))))

write_csv(speed_table,
          "./data1/speed_t_test_table.csv")

sd(distance__season_2014_15$dist_miles) / sqrt(length(distance__season_2014_15$dist_miles))
sd(distance__season_2016_17$dist_miles) / sqrt(length(distance__season_2016_17$dist_miles))
sd(distance__season_2017_18$dist_miles) / sqrt(length(distance__season_2017_18$dist_miles))
sd(distance__season_2018_19$dist_miles) / sqrt(length(distance__season_2018_19$dist_miles))
sd(distance__season_2019_20$dist_miles) / sqrt(length(distance__season_2019_20$dist_miles))

sd(distance__season_2014_15$avg_speed) / sqrt(length(distance__season_2014_15$avg_speed))
sd(distance__season_2016_17$avg_speed) / sqrt(length(distance__season_2016_17$avg_speed))
sd(distance__season_2017_18$avg_speed) / sqrt(length(distance__season_2017_18$avg_speed))
sd(distance__season_2018_19$avg_speed) / sqrt(length(distance__season_2018_19$avg_speed))
sd(distance__season_2019_20$avg_speed) / sqrt(length(distance__season_2019_20$avg_speed))



ggplot(data = speed_table) +
  geom_point(aes(x = season, y = other_mean, color = "brown"), size = 2) +
  geom_point(aes(x = season, y = b2b_mean, color = "blue"), size = 2) +
  labs(color = "Legend") +
  #geom_text_repel(aes(label=p), size = 2.5) + 
  geom_errorbar(aes(x = season, ymin = other_mean - 2*standard_error, ymax = other_mean + 2*standard_error), color = "brown", width = 0.2) +
  geom_errorbar(aes(x = season, ymin = b2b_mean - 2*standard_error, ymax = b2b_mean + 2*standard_error), color = "blue", width = 0.2) +
  xlab("Season") +
  ylab("Mean speed") +
  labs(title = "Difference of Means between 2nd game of a back to back and all other games") +
  theme(panel.background = element_rect(fill = "burlywood"),
        panel.grid.major=element_blank())  +
  theme(legend.position = "bottom") 
 


legend("topright", inset=c(-0.2,0), legend=c("b2b","other"), title="Group", fill = c("brown", "blue"), text.width = c(3,5))
  
ggplot(data = dist_table) +
  geom_point(aes(x = season, y = other_mean, color = "brown"), size = 2) +
  geom_point(aes(x = season, y = b2b_mean, color = "blue"), size = 2) +
  labs(color = "Legend") +
  #geom_text_repel(aes(label=p), size = 2.5) + 
  geom_errorbar(aes(x = season, ymin = other_mean - 2*standard_error, ymax = other_mean + 2*standard_error), color = "brown", width = 0.2) +
  geom_errorbar(aes(x = season, ymin = b2b_mean - 2*standard_error, ymax = b2b_mean + 2*standard_error), color = "blue", width = 0.2) +
  xlab("Season") +
  ylab("Mean Distance") +
  labs(title = "Difference of Means between 2nd game of a back to back and all other games") +
  theme(panel.background = element_rect(fill = "burlywood"),
        panel.grid.major=element_blank())  +
  theme(legend.position = "bottom")
 


 
speed_table_sd <- tibble(season = c("2014-15", "2016-17", "2017-18", "2018-19", "2019-20"),
                      other_mean = c(t_speed_2014$estimate[1], t_speed_2016$estimate[1],
                                     t_speed_2017$estimate[1], t_speed_2018$estimate[1],
                                     t_speed_2019$estimate[1]),
                      b2b_mean = c(t_speed_2014$estimate[2], t_speed_2016$estimate[2],
                                   t_speed_2017$estimate[2], t_speed_2018$estimate[2],
                                   t_speed_2019$estimate[2]),
                      standard_error = c(sd(density_distance_2014_2015$avg_speed), 
                                         sd(density_distance_2016_2017$avg_speed), 
                                         sd(density_distance_2017_2018$avg_speed),
                                         sd(density_distance_2018_2019$avg_speed),
                                         sd(density_distance_2019_2020$avg_speed)))

ggplot(data = speed_table_sd) +
  geom_point(aes(x = season, y = other_mean, color = "brown"), size = 2) +
  geom_point(aes(x = season, y = b2b_mean, color = "blue"), size = 2) +
  labs(color = "Legend") +
  #geom_text_repel(aes(label=p), size = 2.5) + 
  geom_errorbar(aes(x = season, ymin = other_mean - 2*standard_error, ymax = other_mean + 2*standard_error), color = "brown", width = 0.2) +
  geom_errorbar(aes(x = season, ymin = b2b_mean - 2*standard_error, ymax = b2b_mean + 2*standard_error), color = "blue", width = 0.2) +
  xlab("Season") +
  ylab("Mean speed") +
  labs(title = "Difference of Means between 2nd game of a back to back and all other games") +
  theme(panel.background = element_rect(fill = "burlywood"),
        panel.grid.major=element_blank())  +
  theme(legend.position = "bottom") 


dist_table_sd <- tibble(season = c("2014-15", "2016-17", "2017-18", "2018-19", "2019-20"),
                     other_mean = c(t_dist_miles_2014$estimate[1], t_dist_miles_2016$estimate[1],
                                    t_dist_miles_2017$estimate[1], t_dist_miles_2018$estimate[1],
                                    t_dist_miles_2019$estimate[1]),
                     b2b_mean = c(t_dist_miles_2014$estimate[2], t_dist_miles_2016$estimate[2],
                                  t_dist_miles_2017$estimate[2], t_dist_miles_2018$estimate[2],
                                  t_dist_miles_2019$estimate[2]),
                     standard_error = c(sd(density_distance_2014_2015$dist_miles),
                                        sd(density_distance_2016_2017$dist_miles),
                                        sd(density_distance_2017_2018$dist_miles),
                                        sd(density_distance_2018_2019$dist_miles),
                                        sd(density_distance_2019_2020$dist_miles)))


ggplot(data = dist_table_sd) +
  geom_point(aes(x = season, y = other_mean, color = "brown"), size = 2) +
  geom_point(aes(x = season, y = b2b_mean, color = "blue"), size = 2) +
  labs(color = "Legend") +
  #geom_text_repel(aes(label=p), size = 2.5) + 
  geom_errorbar(aes(x = season, ymin = other_mean - 2*standard_error, ymax = other_mean + 2*standard_error), color = "brown", width = 0.2) +
  geom_errorbar(aes(x = season, ymin = b2b_mean - 2*standard_error, ymax = b2b_mean + 2*standard_error), color = "blue", width = 0.2) +
  xlab("Season") +
  ylab("Mean Distance") +
  labs(title = "Difference of Means between 2nd game of a back to back and all other games") +
  theme(panel.background = element_rect(fill = "burlywood"),
        panel.grid.major=element_blank())  +
  theme(legend.position = "bottom")


#geom_point() +
#stat_summary(fun = mean, geom = "point", 
#            color = "red") +
#stat_summary(aes(x = season, y = avg_speed), fun.data = mean_se, geom = "errorbar", 
#             color = "red")

