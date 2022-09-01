
################################################################################
# preamble 
# always run this part
################################################################################

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)

Sys.setlocale("LC_ALL", "English")

################################################################################

source("code/prepping_2019.R")

source("code/prepping_2020.R")

source("code/prepping_2021.R")

source("code/harmonizing_19_20_21.R")


################################################################################
# making tables to validate the data
################################################################################

# table 3.1 of delhi police report 

victim_table_yearwise_road_user <- 
  victim_19_21 %>% 
  group_by(Year_of_crash, Road_user_type) %>% 
  summarize(n())


# table 7, 8 of TRIPC status report 2021
victim_table_yearwise_striking <-
  victim_19_21 %>% 
  group_by(Year_of_crash, Other_vehicle_type) %>% 
  summarize(n())

victim_table_yearwise_vic_veh <-
  victim_19_21 %>% 
  group_by(Year_of_crash, Victim_vehicle_type) %>% 
  summarize(n())


################################################################################
# Reading 2013-16 and 2016-2018 data sets ("silver" quality)
################################################################################


crash_13_16 <- read_xlsx('data/older/2013-16.xlsx',sheet ="For GIS")

crash_16_18 <- read_excel("data/older/Fatal accident Data 2016-18 (1).xlsx")
