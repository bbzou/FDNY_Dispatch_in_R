library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(shiny)

## This R file is for graphs & Shiny presentations

# read file
df = fread('Fire Incident Dispatch Data_Cleaned.csv')


# df$Classification_Grp = case_when(
#     df$Classification_Grp == "NonMedical Emergencies" ~ 'NME',
#     df$Classification_Grp == "Medical Emergencies" ~ 'ME',
#     df$Classification_Grp == "NonMedical MFAs" ~ 'NMM',
#     df$Classification_Grp == "Medical MFAs" ~ 'MM',
#     df$Classification_Grp == "NonStructural Fires" ~ 'NSF',
#     df$Classification_Grp == "Structural Fires" ~ 'SF')

# summary tables

# num of incident
df %>%
    filter(If_Valid == 'Y') %>%
    group_by(Year, DayWeek) %>%
    summarize(Count = n()) %>%
    arrange(Count)




