################################################################################
# preamble
################################################################################

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)

################################################################################

Sys.setlocale("LC_ALL", "English")

#setwd('C:/Users/goelr/Work/Datasets/Delhi crash database/2021 Extracted accident form Yatin')
setwd('C:/Users/asha2/Documents/GitHub/delhi_road_deaths')


################################################################################
# Reading in data
################################################################################

crash1 <- read_csv("data/2021/1_Crash_Table_1.csv")
crash2 <- read_csv("data/2021/1_Crash_Table_2.csv")
crash3 <- read_csv("data/2021/1_Crash_Table_3.csv")
crash4 <- read_csv("data/2021/1_Crash_Table_4.csv")

ps_names1 <- read_csv("data/2021/Usys1_2_PSDetail_1.csv")
ps_names2 <- read_csv("data/2021/Usys1_2_PSDetail_2.csv")
ps_names3 <- read_csv("data/2021/Usys1_2_PSDetail_3.csv")
ps_names4 <- read_csv("data/2021/Usys1_2_PSDetail_4.csv")


crash1$PS_Name <- NA
crash2$PS_Name <- NA
crash3$PS_Name <- NA
crash4$PS_Name <- NA


for (i in 1:nrow(crash1)){
  crash1$PS_Name[i] <- ps_names1$PS_Name[which(ps_names1$PS_ID==crash1$Police_Station[i])]
}

for (i in 1:nrow(crash2)){
  crash2$PS_Name[i] <- ps_names2$PS_Name[which(ps_names2$PS_ID==crash2$Police_Station[i])]
}

for (i in 1:nrow(crash3)){
  crash3$PS_Name[i] <- ps_names3$PS_Name[which(ps_names3$PS_ID==crash3$Police_Station[i])]
}

for (i in 1:nrow(crash4)){
  crash4$PS_Name[i] <- ps_names4$PS_Name[which(ps_names4$PS_ID==crash4$Police_Station[i])]
}

vehicle1 <- read_csv("data/2021/2_Vehicle_Table_1.csv")
vehicle2 <- read_csv("data/2021/2_Vehicle_Table_2.csv")
vehicle3 <- read_csv("data/2021/2_Vehicle_Table_3.csv")
vehicle4 <- read_csv("data/2021/2_Vehicle_Table_4.csv")

victim1 <- read_csv("data/2021/3_Victim_Table_1.csv")
victim2 <- read_csv("data/2021/3_Victim_Table_2.csv")
victim3 <- read_csv("data/2021/3_Victim_Table_3.csv")
victim4 <- read_csv("data/2021/3_Victim_Table_4.csv")

# Reading file that has police station names and districts 
pstation <- read.csv("data/2021/PS_lookup_181.csv")

################################################################################
# Assigning crash_ID to vehicle and victim files
################################################################################

crash1$Crash_ID_new<- paste0(crash1$Crash_ID, crash1$FIR_No, crash1$PS_Name)
crash_lookup<- subset(crash1, select=c("Crash_ID", "PS_Name", "Crash_ID_new"))
vehicle1<- vehicle1 %>% left_join(crash_lookup, by="Crash_ID")
victim1<- victim1 %>% left_join(crash_lookup, by="Crash_ID")

crash2$Crash_ID_new<- paste0(crash2$Crash_ID, crash2$FIR_No, crash2$PS_Name)
crash_lookup<- subset(crash2, select=c("Crash_ID", "PS_Name", "Crash_ID_new"))
vehicle2<- vehicle2 %>% left_join(crash_lookup, by="Crash_ID")
victim2<- victim2 %>% left_join(crash_lookup, by="Crash_ID")

crash3$Crash_ID_new<- paste0(crash3$Crash_ID, crash3$FIR_No, crash3$PS_Name)
crash_lookup<- subset(crash3, select=c("Crash_ID", "PS_Name", "Crash_ID_new"))
vehicle3<- vehicle3 %>% left_join(crash_lookup, by="Crash_ID")
victim3<- victim3 %>% left_join(crash_lookup, by="Crash_ID")

crash4$Crash_ID_new<- paste0(crash4$Crash_ID, crash4$FIR_No, crash4$PS_Name)
crash_lookup<- subset(crash4, select=c("Crash_ID", "PS_Name", "Crash_ID_new"))
vehicle4<- vehicle4 %>% left_join(crash_lookup, by="Crash_ID")
victim4<- victim4 %>% left_join(crash_lookup, by="Crash_ID")

crash_21<- rbind(crash1, crash2,crash3,crash4)
vehicle_21<- rbind(vehicle1, vehicle2,vehicle3,vehicle4)
victim_21<- rbind(victim1, victim2,victim3,victim4)

rm(crash1, crash2, crash3, crash4)
rm(vehicle1, vehicle2, vehicle3, vehicle4)
rm(victim1, victim2, victim3, victim4)

crash_21$FIR_No <- substr(crash_21$FIR_No, 1, 4)

################################################################################
# Assigning crash_ID to those vehicle where it is not available
################################################################################

x<-unique(vehicle_21$Vehicle_ID[which(is.na(vehicle_21$Crash_ID))])
victim_21 <- victim_21[-c(which(victim_21$Vehicle_ID %in% x)),]

################################################################################
# Reassigning Victim IDs to be unique
################################################################################

for (i in 1:nrow(victim_21)){
  victim_21$Victim_ID[i] <- 8565+i
}

################################################################################
# Removing unnecessary columns
################################################################################

crash_21 <- crash_21 %>% select(where(~!all(is.na(.))))

################################################################################
# Joining crash file with police station look up file for district, state and police station names
################################################################################

crash_21<- crash_21 %>% left_join(pstation, by="PS_Name")

################################################################################
# Adding crash details to vehicle file
################################################################################

vehicle_21 <- vehicle_21 %>% left_join(crash_21, by="Crash_ID_new")

vehicle_21$Vehicle_ID_new<- paste0(vehicle_21$Crash_ID_new,vehicle_21$Vehicle_ID)

################################################################################
# Adding crash details to victim file
################################################################################


victim_21$Vehicle_ID_new<- paste0(victim_21$Crash_ID_new,victim_21$Vehicle_ID)

#victim<- victim[,-which(colnames(victim) %in% c("Vehicle_ID", "Crash_ID", "Crash_ID_new"))]

victim_21 <- victim_21 %>% left_join(vehicle_21, by="Vehicle_ID_new")

victim_21$Impacting_VehOrObject[ which ( victim_21$Road_User=="Pedestrian" & 
                                       victim_21$Impacting_VehOrObject=="Pedestrian") ]<- 
  victim_21$Vehicle_Type[ which ( victim_21$Road_User=="Pedestrian" & 
                                 victim_21$Impacting_VehOrObject=="Pedestrian")]

victim_21$Vehicle_Type[which(victim_21$Road_User=="Pedestrian" )]<- "Pedestrian"


################################################################################
## adding in geocodes from crash file with geocodes
################################################################################

crash_gps_21<- read_csv('data/2021/2021 FIR Delhi geocoded.csv')

crash_gps_21<- subset(crash_gps_21, select=c("FIR_No", "PS_Name", "Latitude", "Longitude"))

# stripping spaces and punctuations from names and making them capital


crash_21$PS_Name<- str_to_upper(crash_21$PS_Name)

crash_21$PS_Name <- str_replace_all(crash_21$PS_Name, "[[:punct:]]", "")

crash_21$PS_Name <- str_replace_all(crash_21$PS_Name, " ", "")



# crash file with geocodes

crash_gps_21$PS_Name<- str_to_upper(crash_gps_21$PS_Name)

crash_gps_21$PS_Name <- str_replace_all(crash_gps_21$PS_Name, "[[:punct:]]", " ")

crash_gps_21$PS_Name <- str_replace_all(crash_gps_21$PS_Name, " ", "")

crash_gps_21$FIR_No<- as.character(crash_gps_21$FIR_No)

crash_gps_21$FIR_No<- str_pad(crash_gps_21$FIR_No, 4, pad = "0")

#victim file

victim_21$PS_Name<- str_to_upper(victim_21$PS_Name)

victim_21$PS_Name <- str_replace_all(victim_21$PS_Name, "[[:punct:]]", "")

victim_21$PS_Name <- str_replace_all(victim_21$PS_Name, " ", "")


# correcting some misspelled names

ps_name_change <- read_csv("data/2021/ps_name_change.csv")

for (i in 1:nrow(crash_21)){
  
  if (crash_21$PS_Name[i] %in% ps_name_change$wrong){
   
    crash_21$PS_Name[i] <- ps_name_change$correct[which(ps_name_change$wrong==crash_21$PS_Name[i])]
  
  }

}

for (i in 1:nrow(victim_21)){
  
  if (victim_21$PS_Name[i] %in% ps_name_change$wrong){
    
    victim_21$PS_Name[i] <- ps_name_change$correct[which(ps_name_change$wrong==victim_21$PS_Name[i])]
    
  }
  
}

for (i in 1:nrow(crash_gps_21)){
  
  if (crash_gps_21$PS_Name[i] %in% ps_name_change$wrong){
    
    crash_gps_21$PS_Name[i] <- ps_name_change$correct[which(ps_name_change$wrong==crash_gps_21$PS_Name[i])]
    
  }
  
}
  
# testing the unmatched ones - to be commented in final code

crash_21_iitd <- data.frame(crash_21$FIR_No, crash_21$PS_Name)
crash_21_bbrg <- data.frame(crash_gps_21$FIR_No, crash_gps_21$PS_Name)
colnames(crash_21_iitd) <- c("FIR_No", "PS_Name")
colnames(crash_21_bbrg) <- c("FIR_No", "PS_Name")
iitd_bbrg_21 <- setdiff(crash_21_iitd, crash_21_bbrg)
bbrg_iitd_21 <- setdiff(crash_21_bbrg, crash_21_iitd)
unique(iitd_bbrg_21$PS_Name)
unique(bbrg_iitd_21$PS_Name)


# combining geocodes into victim file

victim_21 <- victim_21 %>% left_join(crash_gps_21, by=c("FIR_No","PS_Name"))
crash_21 <- crash_21 %>% left_join(crash_gps_21, by=c("FIR_No","PS_Name"))




################################################################################
# Simplifying/replacing values
################################################################################

##simplifying vehicle type names and Impacting Vehicle or Object type names:


victim_21$Victim_Vehicle_Type <- victim_21$Vehicle_Type

##simplifying vehicle type names
victim_21$Victim_Vehicle_Type[which(victim_21$Vehicle_Type %in% c("Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim_21$Victim_Vehicle_Type[which(victim_21$Vehicle_Type %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim_21$Victim_Vehicle_Type[which(victim_21$Vehicle_Type %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim_21$Victim_Vehicle_Type[which(victim_21$Vehicle_Type %in% c("Mini Bus"))]<-"Bus"
victim_21$Victim_Vehicle_Type[which(victim_21$Vehicle_Type %in% c("Bicycle - Manual"))]<-"Bicycle"
victim_21$Victim_Vehicle_Type[which(victim_21$Vehicle_Type %in% c("Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Others"
victim_21$Victim_Vehicle_Type[which(victim_21$Vehicle_Type %in% c("Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private"))]<-"M3W"


victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("Wall", "Tree","Pole", "Guard Rail", "Median", "Other Road Infrastructure", "Shoulder"))]<-"Fixed object"
victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Other"
victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private"))]<-"M3W"
victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("Mini Bus"))]<-"Bus"
victim_21$Impacting_VehOrObject[which(victim_21$Impacting_VehOrObject %in% c("Ambulance", "Construction Vehicle","Animal Drawn Vehicle","Cycle Rickshaw - Manual", "Others", "Cycle Rickshaw"))]<-"Other"





################################################################################
# Only people who died - 2021
################################################################################

victim_f_21 <- 
  victim_21[which(victim_21$Injury_Category %in% 
                    c("Fatal Injury with Offsite Death",
                      "Fatal Injury with Onsite Death")),]


# getting rid of useless columns
victim_f_21 <- victim_f_21 %>% select(where(~!all(is.na(.))))



################################################################################
# Reading in 2019 crash data
################################################################################


crash1 <- read_csv("data/2019/1_Crash_Form_1.csv")
crash2 <- read_csv("data/2019/1_Crash_Form_2.csv")

ps_names1 <- read_csv("data/2019/Usys1_2_PSDetail_1.csv")
ps_names2 <- read_csv("data/2019/Usys1_2_PSDetail_2.csv")


crash1$PS_Name <- NA
crash2$PS_Name <- NA


# some NA values in crash1 are creating a problem - remove them because they are useless without the details anyway. 

crash1 <- crash1 %>% drop_na(Police_Station) %>% drop_na(FIR_No)
crash2 <- crash2 %>% drop_na(Police_Station) %>% drop_na(FIR_No)

for (i in 1:nrow(crash1)){
  crash1$PS_Name[i] <- ps_names1$PS_Name[which(ps_names1$PS_ID==crash1$Police_Station[i])]
}

for (i in 1:nrow(crash2)){
  crash2$PS_Name[i] <- ps_names2$PS_Name[which(ps_names2$PS_ID==crash2$Police_Station[i])]
}

vehicle1 <- read_csv("data/2019/2_Vehicle_Form_1.csv")
vehicle2 <- read_csv("data/2019/2_Vehicle_Form_2.csv")

victim1 <- read_csv("data/2019/3_Person_Form_1.csv")
victim2 <- read_csv("data/2019/3_Person_Form_2.csv")

################################################################################
# Assigning crash_ID to vehicle and victim files
################################################################################

crash1$Crash_ID_new<- paste0(crash1$Crash_ID, crash1$FIR_No, crash1$PS_Name)
crash_lookup<- subset(crash1, select=c("Crash_ID", "PS_Name", "Crash_ID_new"))
vehicle1<- vehicle1 %>% left_join(crash_lookup, by="Crash_ID")
victim1<- victim1 %>% left_join(crash_lookup, by="Crash_ID")

crash2$Crash_ID_new<- paste0(crash2$Crash_ID, crash2$FIR_No, crash2$PS_Name)
crash_lookup<- subset(crash2, select=c("Crash_ID", "PS_Name", "Crash_ID_new"))
vehicle2<- vehicle2 %>% left_join(crash_lookup, by="Crash_ID")
victim2<- victim2 %>% left_join(crash_lookup, by="Crash_ID")

crash_19<- rbind(crash1, crash2)
vehicle_19<- rbind(vehicle1, vehicle2)
victim_19<- rbind(victim1, victim2)

rm(crash1, crash2)
rm(vehicle1, vehicle2)
rm(victim1, victim2)

crash_19$FIR_No <- substr(crash_19$FIR_No, 1, 4)

################################################################################
# Assigning crash_ID to those vehicle where it is not available
################################################################################

x<-unique(vehicle_19$Vehicle_ID[which(is.na(vehicle_19$Crash_ID))])
victim_19 <- victim_19[-c(which(victim_19$Vehicle_ID %in% x)),]

################################################################################
# Reassigning Victim IDs to be unique
################################################################################

for (i in 1:nrow(victim_19)){
  victim_19$Victim_ID[i] <- 5000+i
}

################################################################################
# Removing unnecessary columns
################################################################################

crash_19 <- crash_19 %>% select(where(~!all(is.na(.))))
victim_19 <- victim_19 %>% select(where(~!all(is.na(.))))
vehicle_19 <- vehicle_19 %>% select(where(~!all(is.na(.))))

################################################################################
# Adding crash details to vehicle file
################################################################################

vehicle_19 <- vehicle_19 %>% left_join(crash_19, by="Crash_ID_new")

vehicle_19$Vehicle_ID_new<- paste0(vehicle_19$Crash_ID_new,vehicle_19$Vehicle_ID)


################################################################################
# Joining crash file with police station look up file for district, state and police station names
################################################################################

crash_19 <- crash_19 %>% left_join(pstation, by="PS_Name")


################################################################################
# Adding crash details to victim file
################################################################################


victim_19$Vehicle_ID_new<- paste0(victim_19$Crash_ID_new,victim_19$Vehicle_ID)

#victim<- victim[,-which(colnames(victim) %in% c("Vehicle_ID", "Crash_ID", "Crash_ID_new"))]

victim_19 <- victim_19 %>% left_join(vehicle_19, by="Vehicle_ID_new")

victim_19$Impacting_VehOrObject[ which ( victim_19$Road_User=="Pedestrian" & 
                                           victim_19$Impacting_VehOrObject=="Pedestrian") ]<- 
  victim_19$Vehicle_Type[ which ( victim_19$Road_User=="Pedestrian" & 
                                    victim_19$Impacting_VehOrObject=="Pedestrian")]

victim_19$Vehicle_Type[which(victim_19$Road_User=="Pedestrian" )]<- "Pedestrian"





################################################################################
# Simplifying/replacing values
################################################################################

##simplifying vehicle type names and Impacting Vehicle or Object type names:


victim_19$Victim_Vehicle_Type <- victim_19$Vehicle_Type

##simplifying vehicle type names
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Tractor with Trailer", "Truck (Generic)","Agricultural Tractor", "Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Mini Bus", "Bus (DTC Delhi)", "Bus (Cluster)"))]<-"Bus"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Bicycle - Manual"))]<-"Bicycle"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Cycle Rickshaw" , "Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Others"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Three Wheeler - Passenger", "E-rickshaw","Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private"))]<-"M3W"

victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Truck (Generic)", "Agricultural Tractor", "Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Wall", "Tree","Pole", "Guard Rail", "Median", "Other Road Infrastructure", "Shoulder", "Barrier", "Curb" ))]<-"Fixed object"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Animal", "Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Other"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private"))]<-"M3W"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Mini Bus","Bus (DTC Delhi)", "Bus (Cluster)"))]<-"Bus"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("E-rickshaw", "Electric Cycle Rickshaw", "Ambulance", "Construction Vehicle","Animal Drawn Vehicle","Cycle Rickshaw - Manual", "Others", "Cycle Rickshaw"))]<-"Other"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Bicycle - Manual"))]<-"Bicycle"



################################################################################
# Correlating with Delhi police data - 2019
#################################################################################

# cycles_2019 <- read_csv("data/2019/2019_cycle_fatality_crashes.csv")
# 
# cycles_2019$`POLICE STATION` <- str_to_upper(cycles_2019$`POLICE STATION`)
# 
# cycles_2019$`POLICE STATION` <- str_replace_all(cycles_2019$`POLICE STATION` , "[[:punct:]]", "")
# 
# cycles_2019$`POLICE STATION` <- str_replace_all(cycles_2019$`POLICE STATION`, " ", "")
# 
# cycle_vics_19 <- data.frame()
# 
# for (i in 1:nrow(cycles_2019)){
#   cycle_vics_19 <- 
#     rbind( cycle_vics_19, 
#            victim_19[ which ( as.numeric(victim_19$FIR_No)==cycles_2019$FIRNO[i]
#                               ),
#                       ]
#            )
# }
# cycle_vics_19 <- 
#   cycle_vics_19 %>% 
#   filter(`No_Of_ Fatalities`>0)%>% 
#   select(FIR_No, PS_Name, Collision_Type, Note, Victim_Vehicle_Type, Impacting_VehOrObject)
# 
# #----------------------------------------------------------------------------#
# # Finding: many cases have cycles in the impacting object column rather than 
# # the victim vehicle column. 
# # - One victim : To be exchanged
# # - Two or more victims : Only one to have bicycle in the victim vehicle

victim_19 <- victim_19 %>% drop_na(PS_Name) %>% drop_na(FIR_No)

ps_lookup_dp <- read_csv("data/ps_lookup_2016_2020_w_2019.csv")

ps_lookup_dp <- ps_lookup_dp %>% select(`POLICE STATION`, PS_Name)

DP_crash_level_data <- read_csv("data/2016_2020_crash_level_data.csv")

x <- unique(DP_crash_level_data$`U/S`)

x <-x[c(3,5,7)]

DP_crash_level_data <- 
  DP_crash_level_data %>% 
  filter(`U/S` %in% x) %>% 
  filter(YEAR==2019)

colnames(DP_crash_level_data) <- c("SL NO.", "FIR_No", "PS_Name" , "U/S", 
                                   "Date_Of_Crash_DP", "OFFENDING VEHICLE_DP",  
                                   "VICTIMS_DP", "PLACE OF OCCURANCE_DP", 
                                   "ROAD NAME_DP", "YEAR_DP"  )

for (i in 1:nrow(DP_crash_level_data)){
  DP_crash_level_data$PS_Name[i] <- 
    ps_lookup_dp$PS_Name[which(ps_lookup_dp$`POLICE STATION`== 
                                 DP_crash_level_data$PS_Name[i])]
}

victim_19$FIR_No <- as.double(victim_19$FIR_No)

month_short <- substr(month.name,1,3)

for(i in 1:nrow(victim_19)){
  victim_19$Date_Of_Crash[i] <- paste0(substr(victim_19$Date_Of_Crash[i],1,2),
                                 "-",
                                 month_short[as.numeric(substr(victim_19$Date_Of_Crash[i], 4,5))],
                                 "-",
                                 "19")
}

victim_19 <- victim_19 %>% left_join(DP_crash_level_data, by=c("FIR_No", "PS_Name") )

view_19 <- 
  victim_19 %>% 
  filter(Injury_Category %in% c("Fatal Injury with Offsite Death",
                                "Fatal Injury with Onsite Death")) %>% 
  select(Victim_ID, FIR_No, PS_Name, Date_Of_Crash, 
         Victim_Vehicle_Type, VICTIMS_DP, 
         Impacting_VehOrObject, `OFFENDING VEHICLE_DP`,
         Note)




################################################################################
# Changing format of PS names - 2019
################################################################################

victim_19$PS_Name<- str_to_upper(victim_19$PS_Name)

victim_19$PS_Name <- str_replace_all(victim_19$PS_Name, "[[:punct:]]", "")

victim_19$PS_Name <- str_replace_all(victim_19$PS_Name, " ", "")


################################################################################
# Only people who died - 2019
################################################################################

victim_f_19 <- 
  victim_19[which(victim_19$Injury_Category %in% 
                    c("Fatal Injury with Offsite Death",
                      "Fatal Injury with Onsite Death")),]

# getting rid of useless columns
victim_f_19 <- victim_f_19 %>% select(where(~!all(is.na(.))))

################################################################################
# Reading in 2020, 2013-16 and 2016-2018 data sets ("silver" quality)
################################################################################


# crash_20 <- read_xlsx('data/2020/FIR Fatal crash data 2020.xlsx',sheet ="Sheet1")

# changing to csv file in excel fixed some formatting issues
crash_20 <- read_csv("data/2020/FIR Fatal crash data 2020.csv")

crash_20 <- crash_20[which(!is.na(crash_20$`S. no.`)),]

crash_13_16 <- read_xlsx('data/older/2013-16.xlsx',sheet ="For GIS")

crash_16_18 <- read_excel("data/older/Fatal accident Data 2016-18 (1).xlsx")



################################################################################
# Making victim Table out of crash tables - 2020
################################################################################



victim_20 <- data.frame(matrix(nrow= sum(crash_20$`Total killed`), ncol = length(colnames(crash_20))))

colnames(victim_20) <- colnames (crash_20)

# Assigning a unique victim id for each victim
#victim_20$Victim_ID <- c(7001:8197)


# Assigning each crash to the victim 
i_victim <- 1

victim_20$Victim_ID <- NA

for (i_crash in 1:nrow(crash_20)){
  

  
  victims <- crash_20$`Total killed`[i_crash] 
  
  while(victims > 0){
    
    victim_20[i_victim,] <- crash_20[i_crash,]
    
    # giving a unique victim ID starting from 7001
    victim_20$Victim_ID[i_victim] <- 7000 + i_victim    
    
    if (victims == 2){
      victim_20$`Age (killed 1)`[i_victim] <- victim_20$`Age (killed 2)`[i_victim]
      victim_20$`Gender (killed 1)`[i_victim] <- victim_20$`Gender (killed 2)`[i_victim]
      victim_20$`Road user type (killed 1)`[i_victim] <- victim_20$`Road user type (killed 2)...20`[i_victim]
    }
    
    if (victims == 3){
      victim_20$`Age (killed 1)`[i_victim] <- 
        victim_20$`Age (killed 3)`[i_victim] 
      
      victim_20$`Gender (killed 1)`[i_victim] <- 
        victim_20$`Gender (killed 3)`[i_victim]
      
      victim_20$`Road user type (killed 2)...23`[i_victim] <- 
        victim_20$`Road user type (killed 2)...23`[i_victim]
    }
    
    i_victim <- i_victim + 1

    victims <- victims - 1

  }

}

################################################################################
# The crashes with multiple victims were checked manually. 
# Here we load that file and enter the values into the victim file 
# using victim ID. 
################################################################################



victims_20_checked <- read_csv("data/2020/2020_multiple_death_FIRs.csv")

i <- 1

while (i <= nrow(victims_20_checked)){
  
  
  # add victim if not already there
  if (victims_20_checked$Victim_no[i]==0){
    Victim_ID <- 7000 + i_victim 
    i_victim <- i_victim+1
    
    # index same as the previously used (as it must have already been entered)
    index_new <- which(victim_20$Victim_ID==victims_20_checked$Victim_no[i-1])
    
    victim_20 <- rbind(victim_20, victim_20[index_new,])
    
    victim_20$Victim_ID[nrow(victim_20)] <- i_victim
  
  }
  
  index <- which(victim_20$Victim_ID==victims_20_checked$Victim_no[i])
  
  # remove the victim if it is nonfatal or non existent
  if (is.na(victims_20_checked$Injury_Category[i])|
      victims_20_checked$Injury_Category[i]=="Non-Fatal"){
    
    victim_20 <- victim_20[-c(which(victim_20$Victim_ID==victims_20_checked$Victim_no[i])),]
  
    i <- i+1
    
    next
  }
  
  
  
  # add other details:
  
  # Victim_vehicle_type
  victim_20$`Victim user type (as per Delhi Traffic Police)`[index] <-
    victims_20_checked$Victim_Vehicle_Type[i]
  
  # Age
  victim_20$`Age (killed 1)`[index] <- victims_20_checked$Age[i]
  
  # Sex
  victim_20$`Gender (killed 1)`[index] <- victims_20_checked$Sex[i]
  
  # victim vehicle type
  victim_20$`Victim user type (as per Delhi Traffic Police)`[index] <- 
    victims_20_checked$Vehicle_Occupant_Type[i]
  
  # impacting veh or object
  victim_20$`Responsible road user`[index] <- victims_20_checked$Impacting_VehOrObject[i]
  
  
  i <- i+1
  

  
}

################################################################################
#******************************************************************************#
#           Harmonizing the 2020 dataset with 2021 and 2019                    #
#******************************************************************************#
################################################################################

# headings for the harmonized dataset
headings_19_21 <- read_csv("data/headings_20_21.csv")

victim_19_21 <- data.frame(matrix(nrow = nrow(victim_f_19)+nrow(victim_20)+nrow(victim_f_21), ncol = length(colnames(headings_19_21))))

colnames(victim_19_21) <- colnames(headings_19_21)

month_short <- substr(month.name,1,3)

################################################################################
# filling in victim data for 2019 
################################################################################

for (i in 1:nrow(victim_f_19)){
  
  # Victim ID
  ##############################################################################
  victim_19_21$Victim_ID[i] <- victim_f_19$Victim_ID[i]
  
  # Crash ID # TODO
  ##############################################################################
  victim_19_21$Crash_ID[i] <- victim_f_19$Crash_ID.x[i]
  
  # Age
  ##############################################################################
  victim_19_21$Age[i] <- victim_f_19$Text44[i]
  
  
  # All the zero values should be NA
  
  if (is.na(victim_19_21$Age[i])){
    victim_19_21$Age[i] <- 0
  }
  
  if (victim_19_21$Age[i]== 0){
    victim_19_21$Age[i]= NA
  }
  
  
  # Sex
  ##############################################################################
  
  victim_19_21$Sex[i] <- victim_f_19$Sex[i]
  
  
  # Injury category - only fatal entries taken
  ##############################################################################
    
  victim_19_21$Injury_category[i] <- "Death" #victim_f_21$Injury_Category[i]
  
  
  # Date of crash
  ##############################################################################
  
  victim_19_21$Date_of_Crash[i] <- 
    paste0(substr(victim_f_19$Date_Of_Crash[i],1,2),
           "-",
           month_short[as.numeric(substr(victim_f_19$Date_Of_Crash[i], 4,5))],
           "-",
           "19")
  
  victim_19_21$Day_of_crash[i] <- day(victim_f_19$Date_Of_Crash[i])
  
  victim_19_21$Month_of_crash[i] <- month(victim_f_19$Date_Of_Crash[i])
  
  victim_19_21$Year_of_crash[i] <- 2019
  
  
  # Time of crash
  ##############################################################################
  
  victim_19_21$Time_of_Crash[i] <- 
    paste0(hour(victim_f_19$Time_Of_Crash[i]),
           ":",
           minute(victim_f_19$Time_Of_Crash[i])
           )
  
  victim_19_21$Hour_of_crash[i] <- hour(victim_f_19$Time_Of_Crash[i])
  
  victim_19_21$Minutes_of_crash[i] <- minute(victim_f_19$Time_Of_Crash[i])
  
  
  # Victim vehicle type
  ##############################################################################
  
  victim_19_21$Victim_vehicle_type[i] <- victim_f_19$Victim_Vehicle_Type[i]
  
  
  # Road user type
  ##############################################################################
  
  victim_19_21$Road_user_type[i] <- victim_f_19$Road_User[i]
  
  if (!is.na(victim_f_19$Road_User[i])){
    
    if (victim_f_19$Road_User[i] == "Disembarked Vehicle Occupant"){
      victim_19_21$Road_user_type[i] <- "Pedestrian"
      victim_19_21$Victim_vehicle_type[i] <- "Pedestrian"
    }
    
    if (victim_f_19$Road_User[i] == "Vehicle Driver"){
      victim_19_21$Road_user_type[i] <- "Driver"
    }
    
    if (victim_f_19$Road_User[i] == "Vehicle Passenger"){
      victim_19_21$Road_user_type[i] <- "Passenger"
    }
    
    if (victim_f_19$Road_User[i] == "Non Road User Pedestrian"){
      victim_19_21$Road_user_type[i] <- "Pedestrian"
    }
    
    if (victim_f_19$Road_User[i] == "Non Road User Vehicle Occupant"){
      victim_19_21$Road_user_type[i] <- "Unknown"
    }
    
    if (victim_f_19$Road_User[i] == "Other"){
      victim_19_21$Road_user_type[i] <- "Unknown"
    }
  }
  
  
  # Striking vehicle type
  ##############################################################################
  
  victim_19_21$Other_vehicle_type[i] <- victim_f_19$Impacting_VehOrObject[i]
  
  # PS and FIR
  ##############################################################################
  
  victim_19_21$PS_Name[i] <- victim_f_19$PS_Name[i]
  
  victim_19_21$FIR_Number[i] <- victim_f_19$FIR_No[i]
  
  # Location of crash
  ##############################################################################
  
  victim_19_21$Place_of_occurence[i] <- paste(victim_f_19$Road_1[i],
                                              ", ",
                                              victim_f_19$Road_2[i],
                                              ", ", 
                                              victim_f_19$Road_3[i])
  
  victim_19_21$Latitude[i] <- victim_f_19$GPS_Lat[i]
  
  victim_19_21$Longitude[i] <- victim_f_19$GPS_Long[i]
  
  ########################### End of loop ######################################
}




################################################################################
# filling in victim data for 2020
################################################################################


n_row_start <- nrow(victim_f_19)

for (i in 1:nrow(victim_20)){
  
  # Victim_ID is unique for 2020 dataset - from 7000 to 8197
  ##############################################################################
  victim_19_21$Victim_ID[n_row_start+i] <- victim_20$Victim_ID[i]
  
  # Crash ID (unique)
  ##############################################################################
  victim_19_21$Crash_ID[n_row_start+i] <- 1000 + victim_20$`S. no.`[i]
  
  # Age 
  ##############################################################################
  victim_19_21$Age[n_row_start+i] <- victim_20$`Age (killed 1)`[i]
  
  victim_19_21$Age[n_row_start+i] <- switch (victim_19_21$Age[n_row_start+i],
                                 "25-30" = as.integer(27),
                                 "30-35" = as.integer(33),
                                 "35-40" = as.integer(37),
                                 "25*"   = as.integer(25),
                                 "1.5"   = 1,
                                 "N/A"   = NA,
                                 "0"     = NA,
                                 "Unknown" = NA,
                                 as.integer(victim_19_21$Age[n_row_start+i])
                                 ) 
  
  # Sex
  ##############################################################################
  victim_19_21$Sex[n_row_start+i] <- victim_20$`Gender (killed 1)`[i]
  
  # Injury category always death
  ##############################################################################
  victim_19_21$Injury_category[n_row_start+i] <- "Death"
  
  # Date of crash
  ##############################################################################
  
  victim_19_21$Date_of_Crash[n_row_start+i] <- victim_20$`Date of crash`[i]
  
  if(!is.na(victim_20$`Date of crash`[i])){
    
    victim_19_21$Day_of_crash[n_row_start+i] <- as.numeric(substr(victim_20$`Date of crash`[i],1,2))
  
    victim_19_21$Month_of_crash[n_row_start+i] <- as.numeric(which(month_short==substr(victim_20$`Date of crash`[i],4,6)))
  
  }
  
  victim_19_21$Year_of_crash[n_row_start+i] <- 2020
  
  # Time of crash
  ##############################################################################
  
  victim_19_21$Time_of_Crash[n_row_start+i] <- victim_20$`Time of crash (24 hr format)`[i]
  
  if(!is.na(victim_20$`Time of crash (24 hr format)`[i]) 
     & 
     victim_20$`Time of crash (24 hr format)`[i]!="Unknown"){
    
    victim_19_21$Hour_of_crash[n_row_start+i] <- 
      as.numeric(substr(victim_20$`Time of crash (24 hr format)`[i],1,2))
  
    victim_19_21$Minutes_of_crash[n_row_start+i] <- 
      as.numeric(substr(victim_20$`Time of crash (24 hr format)`[i],4,5))
  }
  
  if(!is.na(victim_20$`Time of crash (24 hr format)`[i]) 
     &
     victim_20$`Time of crash (24 hr format)`[i]=="Unknown"){
    
    victim_19_21$Time_of_Crash[n_row_start+i] <- NA
  
  }
  
  if (!is.na(victim_20$`Time of crash (24 hr format)`[i]) 
      &
      (victim_20$`Time of crash (24 hr format)`[i]==0.2|
       victim_20$`Time of crash (24 hr format)`[i]==0.27)){
    victim_19_21$Time_of_Crash[n_row_start+i] <- NA
  }
  
  
  # Road user type
  ##############################################################################
  victim_19_21$Road_user_type[n_row_start+i] <- victim_20$`Road user type (killed 1)`[i]
  
  
  if (victim_20$`Victim user type (as per Delhi Traffic Police)`[i]=="Cyclists"){
    victim_19_21$Road_user_type[n_row_start+i] <- "Cyclist"
  }
  
  # Victim vehicle type
  ##############################################################################
  victim_19_21$Victim_vehicle_type[n_row_start+i] <- victim_20$`Victim user type (as per Delhi Traffic Police)`[i]
  
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="HDC"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="TNG"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Cycle rickshaw"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Ambulance"){
    victim_19_21$Victim_vehicle_type[n_row_start+i] = "Others"
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="PED"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="POV"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Cyclists"){
    victim_19_21$Victim_vehicle_type[n_row_start+i] = "Bicycle"
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="Self"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="MIL"){
    victim_19_21$Victim_vehicle_type[n_row_start+i] = victim_20$`Responsible road user`[i]
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="TSR"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Auto-rickshaw- TSR"){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="M3W"
  }
  
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="TAX"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Car/ Jeep/ Van/ Taxi"){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="Car"
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="Pedestrians"){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="Pedestrian"
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="TMP"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="HTV"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Tempo/ Tractor"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Heavy vehicles"){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="Truck/Tractor"
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="TWW"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="Motorised two wheeler"){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="MTW"
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="PAS"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="DTC"){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="Bus"
  }
  
  if (victim_19_21$Victim_vehicle_type[n_row_start+i]=="ERC"|
      victim_19_21$Victim_vehicle_type[n_row_start+i]=="TRC"){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="Others"
  }
  
  if (!is.na(victim_19_21$Road_user_type[n_row_start+i])){
    if (victim_19_21$Road_user_type[n_row_start+i]=="Pedestrian"){
      victim_19_21$Victim_vehicle_type[n_row_start+i]="Pedestrian"
    }
  }
  
  
  # Correction for pedestrians
  
  if (victim_20$`Pedestrian killed`[i]==1){
    victim_19_21$Victim_vehicle_type[n_row_start+i]="Pedestrian"
  }

  # Slight correction for MTWs
  
  if (victim_20$`MTW killed`[i]==1 & 
      victim_20$`Victim user type (as per Delhi Traffic Police)`[i]=="Self" & 
      victim_20$`Responsible road user`[i] =="UNKNOWN"){
    
    victim_19_21$Victim_vehicle_type[n_row_start+i] = "MTW"
  
  }
  
  
  if (victim_20$`Victim user type (as per Delhi Traffic Police)`[i]=="Cyclists"){
    victim_19_21$Victim_vehicle_type[n_row_start+i] = "Bicycle"
  }
  
  # Impacting vehicle
  ##############################################################################
  victim_19_21$Other_vehicle_type[n_row_start+i] <- victim_20$`Responsible road user`[i]
  
  if (victim_19_21$Other_vehicle_type[n_row_start+i] == "Motorised two wheeler"){
    victim_19_21$Other_vehicle_type[n_row_start+i] = "MTW"
  }
  
  if (victim_19_21$Other_vehicle_type[n_row_start+i] == "Heavy vehicles" |
      victim_19_21$Other_vehicle_type[n_row_start+i] == "Heavy Vehicles"|
      victim_19_21$Other_vehicle_type[n_row_start+i] == "Tempo/ Tractor"){
    victim_19_21$Other_vehicle_type[n_row_start+i] = "Truck/Tractor"
  }
  
  if (victim_19_21$Other_vehicle_type[n_row_start+i] == "Car/ Jeep/ Van/ Taxi"){
    victim_19_21$Other_vehicle_type[n_row_start+i] = "Car"
  }
  
  if (victim_19_21$Other_vehicle_type[n_row_start+i] == "Auto-rickshaw- TSR"){
    victim_19_21$Other_vehicle_type[n_row_start+i] = "M3W"
  }
  
  if (victim_19_21$Other_vehicle_type[n_row_start+i] == "UNKNOWN"){
    victim_19_21$Other_vehicle_type[n_row_start+i] = "Unknown"
  }
  
  if (victim_19_21$Other_vehicle_type[n_row_start+i] == "Cycle rickshaw"){
    victim_19_21$Other_vehicle_type[n_row_start+i] = "Other"
  }  
  

  # Police station (in the same format at 2021) and FIR
  ##############################################################################
  
  victim_20$`Police station`[i]<- str_to_upper(victim_20$`Police station`[i])
  
  victim_20$`Police station`[i] <- str_replace_all(victim_20$`Police station`[i], "[[:punct:]]", "")
  
  victim_20$`Police station`[i] <- str_replace_all(victim_20$`Police station`[i], " ", "")
  
  victim_19_21$PS_Name[n_row_start+i] <- victim_20$`Police station`[i]
  
  victim_19_21$FIR_Number[n_row_start+i] <- victim_20$`FIR No.`[i]
  
  # Location of crash
  ##############################################################################
  
  victim_19_21$Place_of_occurence[n_row_start+i] <- paste(victim_20$`Place of occurrence`[i], victim_20$`Road name`[i])
  
  victim_19_21$Latitude[n_row_start+i] <- victim_20$`Geocordinates (Latitude)`[i]
  
  victim_19_21$Longitude[n_row_start+i] <- victim_20$`Geocordinates (longitude)`[i]
  
  ########################### End of loop ######################################
}


################################################################################
# filling in victim data for 2021
################################################################################

n_row_start <- n_row_start + nrow(victim_20)

for (i in 1:nrow(victim_f_21)){
  
  # Victim ID
  ##############################################################################
  
  victim_19_21$Victim_ID[n_row_start+i] <- victim_f_21$Victim_ID[i]
  
  # Crash ID - TODO
  ##############################################################################
  
  victim_19_21$Crash_ID[n_row_start+i] <- victim_f_21$Crash_ID.x[i]
  
  # Age
  ##############################################################################
  
  victim_19_21$Age[n_row_start+i] <- victim_f_21$Text44[i]
  
  
  # All the zero values should be NA
  
  if (is.na(victim_19_21$Age[n_row_start+i])){
    victim_19_21$Age[n_row_start+i] <- 0
  }
  
  if (victim_19_21$Age[n_row_start+i]== 0){
    victim_19_21$Age[n_row_start+i]= NA
  }
  
  # Sex
  ##############################################################################
  
  victim_19_21$Sex[n_row_start+i] <- victim_f_21$Sex[i]
  
  # Injury category always death 
  ##############################################################################

  victim_19_21$Injury_category[n_row_start+i] <- "Death" #victim_f_21$Injury_Category[i]
  
  # Date of crash
  ##############################################################################
  
  # one missing value
  if(i == 611){
    victim_f_21$Date_Of_Crash[i] = "16-09-2021"
  }
  
  victim_19_21$Date_of_Crash[n_row_start+i] <- 
    paste0(day(victim_f_21$Date_Of_Crash[i]),
           "-",
           month(victim_f_21$Date_Of_Crash[i], label = TRUE, abbr = TRUE),
           "-",
           year(victim_f_21$Date_Of_Crash[i]))
  
  victim_19_21$Day_of_crash[n_row_start+i] <- day(victim_f_21$Date_Of_Crash[i])
  
  victim_19_21$Month_of_crash[n_row_start+i] <- month(victim_f_21$Date_Of_Crash[i])
  
  victim_19_21$Year_of_crash[n_row_start+i] <- 2021
  
  # Time of crash
  ##############################################################################
  
  victim_19_21$Time_of_Crash[n_row_start+i] <- 
    
    paste0(hour(victim_f_21$Time_Of_Crash[i]),
           ":",
           minute(victim_f_21$Time_Of_Crash[i])
           )
  
  victim_19_21$Hour_of_crash[i] <- hour(victim_f_21$Time_Of_Crash[i])
  
  victim_19_21$Minutes_of_crash[i] <- minute(victim_f_19$Time_Of_Crash[i])
  
  
  # Road user type
  ##############################################################################
  
  victim_19_21$Road_user_type[n_row_start+i] <- victim_f_21$Road_User[i]
  
  if (!is.na(victim_f_21$Road_User[i])){
  
    if (victim_f_21$Road_User[i] == "Disembarked Vehicle Occupant"){
      victim_19_21$Road_user_type[n_row_start+i] <- "Pedestrian"
      victim_19_21$Victim_vehicle_type[n_row_start+i] <- "Pedestrian"
    }
    
    if (victim_f_21$Road_User[i] == "Vehicle Driver"){
      victim_19_21$Road_user_type[n_row_start+i] <- "Driver"
    }
    
    if (victim_f_21$Road_User[i] == "Vehicle Passenger"){
      victim_19_21$Road_user_type[n_row_start+i] <- "Passenger"
    }
    
    if (victim_f_21$Road_User[i] == "Non Road User Pedestrian"){
      victim_19_21$Road_user_type[n_row_start+i] <- "Pedestrian"
    }
    
    if (victim_f_21$Road_User[i] == "Non Road User Vehicle Occupant"){
      victim_19_21$Road_user_type[n_row_start+i] <- "Unknown"
    }
    
    if (victim_f_21$Road_User[i] == "Other"){
      victim_19_21$Road_user_type[n_row_start+i] <- "Unknown"
    }
  }
  
  # Victim vehicle type
  ##############################################################################
  
  victim_19_21$Victim_vehicle_type[n_row_start+i] <- victim_f_21$Victim_Vehicle_Type[i]
  
  
  # Striking vehicle type
  ##############################################################################
  
  victim_19_21$Other_vehicle_type[n_row_start+i] <- 
    victim_f_21$Impacting_VehOrObject[i]
  
  # PS and FIR
  ##############################################################################
  
  victim_19_21$PS_Name[n_row_start+i] <- victim_f_21$PS_Name[i]
  
  victim_19_21$FIR_Number[n_row_start+i] <- victim_f_21$FIR_No[i]
  
  # Location of crash
  ##############################################################################
  
  victim_19_21$Place_of_occurence[n_row_start+i] <- 
    paste(victim_f_21$Road_1[i],
          ", ",
          victim_f_21$Road_2[i],
          ", ", 
          victim_f_21$Road_3[i])
  
  victim_19_21$Latitude[n_row_start+i] <- victim_f_21$Latitude[i]
  
  victim_19_21$Longitude[n_row_start+i] <- victim_f_21$Longitude [i]
  
  ########################### End of loop ######################################
}



write_csv(victim_19_21, "data/victim_19_21.csv")

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

# ################################################################################
# # Imputing the missing values of Age and Sex
# ################################################################################
# 
# 
# library(mice)
# 
# #victim_imputed_age <-mice(victim_19_21, m = 10, )
# 
# 
# ################################################################################
# # Tables needed
# ################################################################################
# 
# 
# View(  victim_f_21 %>% 
#          
#          # remove NA values, considering them to be the unknown cases
#          filter(!is.na(Victim_Vehicle_Type)) %>% 
#          
#          filter(Victim_Vehicle_Type!="Unknown") %>% 
#          
#          group_by(Victim_Vehicle_Type) %>% 
#          
#          summarise(n = n()) %>%
#          
#          mutate(percentage = round(n*100 / sum(n)))
#        
# )
# 
# View(  victim_f_21 %>% 
#          filter(Impacting_VehOrObject!="Unknown") %>% 
#          group_by(Impacting_VehOrObject) %>% 
#          summarise(n = n()) %>%
#          mutate(percentage = round(n*100 / sum(n)))
# )
# 
# 
# View(victim_f_21 %>% 
#        filter(Victim_Vehicle_Type!="Unknown") %>% 
#        filter(Impacting_VehOrObject!="Unknown") %>%
#        group_by(Victim_Vehicle_Type, Impacting_VehOrObject) %>% 
#        summarise(n = n()) %>% 
#        mutate(percentage = round(n*100/sum(n))) %>% 
#        select(-c("n")) %>% 
#        spread(Impacting_VehOrObject, percentage) %>% 
#        replace(is.na(.), 0)
# )
# 
# projected_population_21 <- read_csv("data/2021/2021_projection_DL_population.csv")
# 
# victim_f_21$pop <- 0
# victim_f_21$Age_grp <- NA
# 
# for (i in 1:nrow(projected_population_21)){
#   
#   victim_f_21$Age_grp[ which(victim_f_21$Text44>=projected_population_21$age_from[i] & victim_f_21$Text44<=projected_population_21$age_to[i])] <- 
#     paste(projected_population_21$age_from[i], "-", projected_population_21$age_to[i])
#   
#   victim_f_21$pop[ which(victim_f_21$Text44>=projected_population_21$age_from[i] & victim_f_21$Text44<=projected_population_21$age_to[i] & victim_f_21$Sex=="Male")] <- 
#     projected_population_21$Male[i]
#   
#   victim_f_21$pop[ which(victim_f_21$Text44>=projected_population_21$age_from[i] & victim_f_21$Text44<=projected_population_21$age_to[i] & victim_f_21$Sex=="Female")] <-
#     projected_population_21$Female[i]
# }
# 
# 
# 
# 
# victim_f_21 %>% 
#   filter(Text44!=0) %>% 
#   filter(Sex=="Male"|Sex=="Female") %>% 
#   group_by(Age_grp, Sex) %>% 
#   summarize(count = round( n()*100000/mean(pop),1)) %>% 
#   mutate(Age_grp =  factor(Age_grp, levels = paste(projected_population_21$age_from, "-", projected_population_21$age_to))) %>%
#   arrange(Age_grp) %>% 
#   ggplot(aes(x=Age_grp, y=count, fill=Sex))+
#   geom_col(position="dodge")+
#   geom_text(aes(label= count), position = position_dodge(width = 0.9),  vjust=-0.3, size = 3)+
#   ylab("Reported deaths per 100,000 people")+
#   xlab("Age Group")+
#   theme_bw()+
#   theme(
#     panel.border = element_blank(),  
#     axis.line = element_line(colour = "white"), 
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# victim_f_21$Age_grp2 <- NA
# 
# ages <- c(0,18,25,35,45,60,100)
# 
# for (i in 1:6){
#   victim_f_21$Age_grp2[ which( victim_f_21$Text44>ages[i] & victim_f_21$Text44<=ages[i+1])] <- 
#     paste(ages[i],"-",ages[i+1])
# }
# 
# 
# victim_f_21 %>% 
#   filter(Text44!=0) %>% 
#   filter(Sex=="Male"|Sex=="Female") %>% 
#   group_by(Age_grp2, Sex) %>% 
#   summarize(count = round( n()*100000/mean(pop),1)) %>% 
#   ggplot(aes(x=Age_grp2, y=count, fill=Sex))+
#   geom_col(position="dodge")+
#   geom_text(aes(label= count), position = position_dodge(width = 0.9),  vjust=-0.3, size = 3)+
#   ylab("Reported deaths per 100,000 people")+
#   xlab("Age Group")+
#   theme_bw()+
#   theme(
#     panel.border = element_blank(),  
#     axis.line = element_line(colour = "white"), 
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



