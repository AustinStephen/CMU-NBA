## Author: Austin Stephen 
## Date: 8/18/2021
## Purpose: Generating figures for the "Structural Fatigue" section 
#       of the paper.
library(tidyverse)
library(dplyr)
library(gt)

# Stage 2 -----------------------------------------------------------------


metric <- c( "time zone -3","time zone -2", "time zone -1", 
            "time zone +1", "time zone +2","time zone +3", 
            "flight distance differnence", "flight duration", 
            "second game of a back to back", "three games in four days",
            "rest difference")

coeff <- c(-1.740, .504, -.052, 0.094, 0.870, -1.115, 6.707, 0.189, -.384,
           -1.29, .431)
  
p_val <- c(".108", ".431", ".867", ".756", ".162", ".916", ".687", ".243",
           "0.164", "1.69e-07" ,"4.13e-07")
   
r_squared <- c(.00014, -3.51e-5, -8.98e-05,-8.35e-05, 8.87e-05, -9.13e-05,
               -7.74e-05, 3.3e-05, 4.007e-05, 0.0040, .00410 )

table<- data.frame(metric, coeff, p_val, r_squared)

table <- table %>% rename("p-value" = p_val,
                          "adjusted R^2" = r_squared)

save <- table %>%
  gt(rowname_col = "metric") %>%
  tab_header(
    title = md("**Structural Fatigue Sources**")
  ) %>% 
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% 
  cols_align(align = "left",
             columns = c(2,4)) %>% 
  tab_style(
    style = list(
      cell_fill(color = "grey")
    ),
    locations = cells_body(
      rows = c(2,4,6,8,10))
  )%>%
  tab_source_note(
    source_note = "Each factor was tested in its own model to avoid collinearity"
  )


library(webshot)
gtsave(save,"structural_fatigue.png",path= "./Paper/")
