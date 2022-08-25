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

crash_gps_21<- subset(crash_gps_21, select=c("FIR No.", "Police Station", "Latitude", "Longitude"))

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

victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Agricultural Tractor", "Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Wall", "Tree","Pole", "Guard Rail", "Median", "Other Road Infrastructure", "Shoulder"))]<-"Fixed object"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Other"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private"))]<-"M3W"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Mini Bus","Bus (DTC Delhi)", "Bus (Cluster)"))]<-"Bus"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Ambulance", "Construction Vehicle","Animal Drawn Vehicle","Cycle Rickshaw - Manual", "Others", "Cycle Rickshaw"))]<-"Other"


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


# crash_20 <- read_xlsx('data/older/FIR Fatal crash data 2020.xlsx',sheet ="Sheet1")

# changing to csv file in excel fixed some formatting issues
crash_20 <- read_csv("data/older/FIR Fatal crash data 2020.csv")

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
  

  
  victims <- crash_20$`Total killed`[i_crash] # + crash_20$`Total injured`[i_crash] # we only care about the dead people

  
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
      victim_20$`Age (killed 1)`[i_victim] <- victim_20$`Age (killed 3)`[i_victim] 
      victim_20$`Gender (killed 1)`[i_victim] <- victim_20$`Gender (killed 3)`[i_victim]
      victim_20$`Road user type (killed 2)...23`[i_victim] <- victim_20$`Road user type (killed 2)...23`[i_victim]
    }
    
    i_victim <- i_victim + 1

    victims <- victims - 1

  }

}

################################################################################
# Harmonizing the 2020 dataset with 2021 and 2019
################################################################################

# headings for the harmonized dataset
headings_19_21 <- read_excel("data/headings_20_21.xlsx")

victim_19_21 <- data.frame(matrix(nrow = nrow(victim_f_19)+nrow(victim_20)+nrow(victim_f_21), ncol = length(colnames(headings_20_21))))

colnames(victim_19_21) <- colnames(headings_19_21)


################################################################################
# filling in victim data for 2019 - work in progress from here on!!!!!
################################################################################

for (i in 1:nrow(victim_f_19)){
  
  victim_19_21$Victim_ID[i] <- victim_f_19$Victim_ID[i]
  
  victim_19_21$Crash_ID[i] <- victim_f_19$Crash_ID.x[i]
  
  victim_19_21$Age[i] <- victim_f_19$Text44[i]
  
  
  # All the zero values should be NA
  
  if (is.na(victim_20_21$Age[nrow(victim_20)+i])){
    victim_20_21$Age[nrow(victim_20)+i] <- 0
  }
  
  if (victim_20_21$Age[nrow(victim_20)+i]== 0){
    victim_20_21$Age[nrow(victim_20)+i]= NA
  }
  
  victim_20_21$Sex[nrow(victim_20)+i] <- victim_f_21$Sex[i]
  
  victim_20_21$Injury_category[nrow(victim_20)+i] <- "Death" #victim_f_21$Injury_Category[i]
  
  victim_20_21$Date_of_Crash[nrow(victim_20)+i] <- paste0(day(victim_f_21$Date_Of_Crash[i]),"-",month(victim_f_21$Date_Of_Crash[i], label = TRUE, abbr = TRUE),"-",year(victim_f_21$Date_Of_Crash[i]))
  
  victim_20_21$Time_of_Crash[nrow(victim_20)+i] <- victim_f_21$Time_Of_Crash[i]
  
  victim_20_21$Road_user_type[nrow(victim_20)+i] <- victim_f_21$Road_User[i]
  
  if (!is.na(victim_f_21$Road_User[i])){
    
    if (victim_f_21$Road_User[i] == "Disembarked Vehicle Occupant"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Pedestrian"
    }
    
    if (victim_f_21$Road_User[i] == "Vehicle Driver"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Driver"
    }
    
    if (victim_f_21$Road_User[i] == "Vehicle Passenger"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Passenger"
    }
    
    if (victim_f_21$Road_User[i] == "Non Road User Pedestrian"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Pedestrian"
    }
    
    if (victim_f_21$Road_User[i] == "Non Road User Vehicle Occupant"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Unknown"
    }
    
    if (victim_f_21$Road_User[i] == "Other"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Unknown"
    }
  }
  victim_20_21$Victim_vehicle_type[nrow(victim_20)+i] <- victim_f_21$Victim_Vehicle_Type[i]
  
  victim_20_21$Other_vehicle_type[nrow(victim_20)+i] <- victim_f_21$Impacting_VehOrObject[i]
  
  victim_20_21$PS_Name[nrow(victim_20)+i] <- victim_f_21$PS_Name[i]
  
  victim_20_21$FIR_Number[nrow(victim_20)+i] <- victim_f_21$FIR_No[i]
  
  victim_20_21$Place_of_occurence[nrow(victim_20)+i] <- paste(victim_f_21$Road_1[i],", ",victim_f_21$Road_2[i],", ", victim_f_21$Road_3[i])
  
  victim_20_21$Latitude[nrow(victim_20)+i] <- victim_f_21$Latitude[i]
  
  victim_20_21$Longitude[nrow(victim_20)+i] <- victim_f_21$Longitude [i]
}





# filling in victim data for 2020

for (i in 1:nrow(victim_20)){
  
  # Victim_ID is unique for 2020 dataset - from 7000 to 8197
  victim_20_21$Victim_ID[i] <- victim_20$Victim_ID[i]
  
  # Unique crash ID (unique to 2020)
  victim_20_21$Crash_ID[i] <- 1000 + victim_20$`S. no.`[i]
  
  # Age is sometimes specified as age group. 
  # 
  victim_20_21$Age[i] <- victim_20$`Age (killed 1)`[i]
  
  victim_20_21$Age[i] <- switch (victim_20_21$Age[i],
                                 "25-30" = as.integer(27),
                                 "30-35" = as.integer(33),
                                 "35-40" = as.integer(37),
                                 "25*"   = as.integer(25),
                                 "1.5"   = 1,
                                 "N/A"   = NA,
                                 "0"     = NA,
                                 as.integer(victim_20_21$Age[i])
                                 ) 
  
  
    
  
  # 
  victim_20_21$Sex[i] <- victim_20$`Gender (killed 1)`[i]
  
  # change later when injured also included
  victim_20_21$Injury_category[i] <- "Death"
  
  # 
  victim_20_21$Date_of_Crash[i] <- victim_20$`Date of crash`[i]
  
  # 
  victim_20_21$Time_of_Crash[i] <- victim_20$`Time of crash (24 hr format)`[i]
  
  # 
  victim_20_21$Road_user_type[i] <- victim_20$`Road user type (killed 1)`[i]
  
  # Victim vehicle type
  ##############################################################################
  victim_20_21$Victim_vehicle_type[i] <- victim_20$`Victim user type (as per Delhi Traffic Police)`[i]
  
  
  if (victim_20_21$Victim_vehicle_type[i]=="HDC"|
      victim_20_21$Victim_vehicle_type[i]=="TNG"|
      victim_20_21$Victim_vehicle_type[i]=="Cycle rickshaw"|
      victim_20_21$Victim_vehicle_type[i]=="Ambulance"){
    victim_20_21$Victim_vehicle_type[i] = "Others"
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="PED"|
      victim_20_21$Victim_vehicle_type[i]=="POV"|
      victim_20_21$Victim_vehicle_type[i]=="Cyclists"){
    victim_20_21$Victim_vehicle_type[i] = "Bicycle"
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="Self"|
      victim_20_21$Victim_vehicle_type[i]=="MIL"){
    victim_20_21$Victim_vehicle_type[i] = victim_20$`Responsible road user`[i]
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="TSR"){
    victim_20_21$Victim_vehicle_type[i]="M3W"
  }
  
  
  if (victim_20_21$Victim_vehicle_type[i]=="TAX"){
    victim_20_21$Victim_vehicle_type[i]="Car"
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="Pedestrians"){
    victim_20_21$Victim_vehicle_type[i]="Pedestrian"
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="TMP"|
      victim_20_21$Victim_vehicle_type[i]=="HTV"|
      victim_20_21$Victim_vehicle_type[i]=="Tempo/ Tractor"){
    victim_20_21$Victim_vehicle_type[i]="Truck/Tractor"
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="TWW"){
    victim_20_21$Victim_vehicle_type[i]="MTW"
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="PAS|DTC"){
    victim_20_21$Victim_vehicle_type[i]="Bus"
  }
  
  if (victim_20_21$Victim_vehicle_type[i]=="ERC"|
      victim_20_21$Victim_vehicle_type[i]=="TRC"){
    victim_20_21$Victim_vehicle_type[i]="Others"
  }
  
  if (!is.na(victim_20_21$Road_user_type[i])){
    if (victim_20_21$Road_user_type[i]=="Pedestrian"){
      victim_20_21$Victim_vehicle_type[i]="Pedestrian"
    }
  }
  
  # Impacting vehicle
  ##############################################################################
  victim_20_21$Other_vehicle_type[i] <- victim_20$`Responsible road user`[i]
  
  if (victim_20_21$Other_vehicle_type[i] == "Motorised two wheeler"){
    victim_20_21$Other_vehicle_type[i] = "MTW"
  }
  
  if (victim_20_21$Other_vehicle_type[i] == "Heavy vehicles" |
      victim_20_21$Other_vehicle_type[i] == "Heavy Vehicles"|
      victim_20_21$Other_vehicle_type[i] == "Tempo/ Tractor"){
    victim_20_21$Other_vehicle_type[i] = "Truck/Tractor"
  }
  
  if (victim_20_21$Other_vehicle_type[i] == "Car/ Jeep/ Van/ Taxi"){
    victim_20_21$Other_vehicle_type[i] = "Car"
  }
  
  if (victim_20_21$Other_vehicle_type[i] == "Auto-rickshaw- TSR"){
    victim_20_21$Other_vehicle_type[i] = "M3W"
  }
  
  if (victim_20_21$Other_vehicle_type[i] == "UNKNOWN"){
    victim_20_21$Other_vehicle_type[i] = "Unknown"
  }
  
  if (victim_20_21$Other_vehicle_type[i] == "Cycle rickshaw"){
    victim_20_21$Other_vehicle_type[i] = "Other"
  }  
  

  # Police station in the same format at 2021 ----------------------------------
  
  victim_20$`Police station`[i]<- str_to_upper(victim_20$`Police station`[i])
  
  victim_20$`Police station`[i] <- str_replace_all(victim_20$`Police station`[i], "[[:punct:]]", "")
  
  victim_20$`Police station`[i] <- str_replace_all(victim_20$`Police station`[i], " ", "")
  
  victim_20_21$PS_Name[i] <- victim_20$`Police station`[i]
  
  #-----------------------------------------------------------------------------
  
  victim_20_21$FIR_Number[i] <- victim_20$`FIR No.`[i]
  
  victim_20_21$Place_of_occurence[i] <- paste(victim_20$`Place of occurrence`[i], victim_20$`Road name`[i])
  
  victim_20_21$Latitude[i] <- victim_20$`Geocordinates (Latitude)`[i]
  
  victim_20_21$Longitude[i] <- victim_20$`Geocordinates (longitude)`[i]
}



# filling in victim data for 2021

for (i in 1:nrow(victim_f_21)){
  
  victim_20_21$Victim_ID[nrow(victim_20)+i] <- victim_f_21$Victim_ID[i]
  
  victim_20_21$Crash_ID[nrow(victim_20)+i] <- victim_f_21$Crash_ID.x[i]
  
  victim_20_21$Age[nrow(victim_20)+i] <- victim_f_21$Text44[i]
  
  
  # All the zero values should be NA
  
  if (is.na(victim_20_21$Age[nrow(victim_20)+i])){
    victim_20_21$Age[nrow(victim_20)+i] <- 0
  }
  
  if (victim_20_21$Age[nrow(victim_20)+i]== 0){
    victim_20_21$Age[nrow(victim_20)+i]= NA
  }
  
  victim_20_21$Sex[nrow(victim_20)+i] <- victim_f_21$Sex[i]

  victim_20_21$Injury_category[nrow(victim_20)+i] <- "Death" #victim_f_21$Injury_Category[i]
  
  victim_20_21$Date_of_Crash[nrow(victim_20)+i] <- paste0(day(victim_f_21$Date_Of_Crash[i]),"-",month(victim_f_21$Date_Of_Crash[i], label = TRUE, abbr = TRUE),"-",year(victim_f_21$Date_Of_Crash[i]))
  
  victim_20_21$Time_of_Crash[nrow(victim_20)+i] <- victim_f_21$Time_Of_Crash[i]
  
  victim_20_21$Road_user_type[nrow(victim_20)+i] <- victim_f_21$Road_User[i]
  
  if (!is.na(victim_f_21$Road_User[i])){
  
    if (victim_f_21$Road_User[i] == "Disembarked Vehicle Occupant"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Pedestrian"
    }
    
    if (victim_f_21$Road_User[i] == "Vehicle Driver"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Driver"
    }
    
    if (victim_f_21$Road_User[i] == "Vehicle Passenger"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Passenger"
    }
    
    if (victim_f_21$Road_User[i] == "Non Road User Pedestrian"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Pedestrian"
    }
    
    if (victim_f_21$Road_User[i] == "Non Road User Vehicle Occupant"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Unknown"
    }
    
    if (victim_f_21$Road_User[i] == "Other"){
      victim_20_21$Road_user_type[nrow(victim_20)+i] <- "Unknown"
    }
  }
  victim_20_21$Victim_vehicle_type[nrow(victim_20)+i] <- victim_f_21$Victim_Vehicle_Type[i]
  
  victim_20_21$Other_vehicle_type[nrow(victim_20)+i] <- victim_f_21$Impacting_VehOrObject[i]
  
  victim_20_21$PS_Name[nrow(victim_20)+i] <- victim_f_21$PS_Name[i]
  
  victim_20_21$FIR_Number[nrow(victim_20)+i] <- victim_f_21$FIR_No[i]
  
  victim_20_21$Place_of_occurence[nrow(victim_20)+i] <- paste(victim_f_21$Road_1[i],", ",victim_f_21$Road_2[i],", ", victim_f_21$Road_3[i])
  
  victim_20_21$Latitude[nrow(victim_20)+i] <- victim_f_21$Latitude[i]
  
  victim_20_21$Longitude[nrow(victim_20)+i] <- victim_f_21$Longitude [i]
}



write_csv(victim_20_21, "data/victim_20_21.csv")


################################################################################
# Imputing the missing values of Age and Sex
################################################################################


library(mice)

#victim_imputed_age <-mice(victim_20_21, m = 10, )


################################################################################
# Tables needed
################################################################################


View(  victim_f_21 %>% 
         
         # remove NA values, considering them to be the unknown cases
         filter(!is.na(Victim_Vehicle_Type)) %>% 
         
         filter(Victim_Vehicle_Type!="Unknown") %>% 
         
         group_by(Victim_Vehicle_Type) %>% 
         
         summarise(n = n()) %>%
         
         mutate(percentage = round(n*100 / sum(n)))
       
)

View(  victim_f_21 %>% 
         filter(Impacting_VehOrObject!="Unknown") %>% 
         group_by(Impacting_VehOrObject) %>% 
         summarise(n = n()) %>%
         mutate(percentage = round(n*100 / sum(n)))
)


View(victim_f_21 %>% 
       filter(Victim_Vehicle_Type!="Unknown") %>% 
       filter(Impacting_VehOrObject!="Unknown") %>%
       group_by(Victim_Vehicle_Type, Impacting_VehOrObject) %>% 
       summarise(n = n()) %>% 
       mutate(percentage = round(n*100/sum(n))) %>% 
       select(-c("n")) %>% 
       spread(Impacting_VehOrObject, percentage) %>% 
       replace(is.na(.), 0)
)

projected_population_21 <- read_csv("data/2021/2021_projection_DL_population.csv")

victim_f_21$pop <- 0
victim_f_21$Age_grp <- NA

for (i in 1:nrow(projected_population_21)){
  
  victim_f_21$Age_grp[ which(victim_f_21$Text44>=projected_population_21$age_from[i] & victim_f_21$Text44<=projected_population_21$age_to[i])] <- 
    paste(projected_population_21$age_from[i], "-", projected_population_21$age_to[i])
  
  victim_f_21$pop[ which(victim_f_21$Text44>=projected_population_21$age_from[i] & victim_f_21$Text44<=projected_population_21$age_to[i] & victim_f_21$Sex=="Male")] <- 
    projected_population_21$Male[i]
  
  victim_f_21$pop[ which(victim_f_21$Text44>=projected_population_21$age_from[i] & victim_f_21$Text44<=projected_population_21$age_to[i] & victim_f_21$Sex=="Female")] <-
    projected_population_21$Female[i]
}




victim_f_21 %>% 
  filter(Text44!=0) %>% 
  filter(Sex=="Male"|Sex=="Female") %>% 
  group_by(Age_grp, Sex) %>% 
  summarize(count = round( n()*100000/mean(pop),1)) %>% 
  mutate(Age_grp =  factor(Age_grp, levels = paste(projected_population_21$age_from, "-", projected_population_21$age_to))) %>%
  arrange(Age_grp) %>% 
  ggplot(aes(x=Age_grp, y=count, fill=Sex))+
  geom_col(position="dodge")+
  geom_text(aes(label= count), position = position_dodge(width = 0.9),  vjust=-0.3, size = 3)+
  ylab("Reported deaths per 100,000 people")+
  xlab("Age Group")+
  theme_bw()+
  theme(
    panel.border = element_blank(),  
    axis.line = element_line(colour = "white"), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

victim_f_21$Age_grp2 <- NA

ages <- c(0,18,25,35,45,60,100)

for (i in 1:6){
  victim_f_21$Age_grp2[ which( victim_f_21$Text44>ages[i] & victim_f_21$Text44<=ages[i+1])] <- 
    paste(ages[i],"-",ages[i+1])
}


victim_f_21 %>% 
  filter(Text44!=0) %>% 
  filter(Sex=="Male"|Sex=="Female") %>% 
  group_by(Age_grp2, Sex) %>% 
  summarize(count = round( n()*100000/mean(pop),1)) %>% 
  ggplot(aes(x=Age_grp2, y=count, fill=Sex))+
  geom_col(position="dodge")+
  geom_text(aes(label= count), position = position_dodge(width = 0.9),  vjust=-0.3, size = 3)+
  ylab("Reported deaths per 100,000 people")+
  xlab("Age Group")+
  theme_bw()+
  theme(
    panel.border = element_blank(),  
    axis.line = element_line(colour = "white"), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



