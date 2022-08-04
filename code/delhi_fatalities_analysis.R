################################################################################
# preamble
################################################################################

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library("data.table")
################################################################################


#setwd('C:/Users/goelr/Work/Datasets/Delhi crash database/2021 Exctracted accident form Yatin')
#setwd('C:/Users/asha2/Documents/GitHub/delhi_road_deaths')


################################################################################
# Reading in data
################################################################################

crash1<- read_xlsx('data/2021/1_Crash_Table_1.xlsx',sheet ="1_Crash_Table")
crash2<- read_xlsx('data/2021/1_Crash_Table_2.xlsx',sheet ="1_Crash_Table")
crash3<- read_xlsx('data/2021/1_Crash_Table_3.xlsx',sheet ="1_Crash_Table")
crash4<- read_xlsx('data/2021/1_Crash_Table_4.xlsx',sheet ="1_Crash_Table")


vehicle1<- read_xlsx('data/2021/2_Vehicle_Table_1.xlsx',sheet ="2_Vehicle_Table")
vehicle2<- read_xlsx('data/2021/2_Vehicle_Table_2.xlsx',sheet ="2_Vehicle_Table")
vehicle3<- read_xlsx('data/2021/2_Vehicle_Table_3.xlsx',sheet ="2_Vehicle_Table")
vehicle4<- read_xlsx('data/2021/2_Vehicle_Table_4.xlsx',sheet ="2_Vehicle_Table")


victim1<- read_xlsx('data/2021/3_Victim_Table_1.xlsx',sheet ="3_Victim_Table")
victim2<- read_xlsx('data/2021/3_Victim_Table_2.xlsx',sheet ="3_Victim_Table")
victim3<- read_xlsx('data/2021/3_Victim_Table_3.xlsx',sheet ="3_Victim_Table")
victim4<- read_xlsx('data/2021/3_Victim_Table_4.xlsx',sheet ="3_Victim_Table")

# Reading file that has police station names and districts 
pstation<- read.csv("data/2021/PS_lookup.csv")

################################################################################
# Assigning crash_ID to vehicle and victim files
################################################################################

crash1$Crash_ID_new<- paste0(crash1$Crash_ID, crash1$FIR_No, crash1$Police_Station)
crash_lookup<- subset(crash1, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle1<- vehicle1 %>% left_join(crash_lookup, by="Crash_ID")
victim1<- victim1 %>% left_join(crash_lookup, by="Crash_ID")

crash2$Crash_ID_new<- paste0(crash2$Crash_ID, crash2$FIR_No, crash2$Police_Station)
crash_lookup<- subset(crash2, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle2<- vehicle2 %>% left_join(crash_lookup, by="Crash_ID")
victim2<- victim2 %>% left_join(crash_lookup, by="Crash_ID")

crash3$Crash_ID_new<- paste0(crash3$Crash_ID, crash3$FIR_No, crash3$Police_Station)
crash_lookup<- subset(crash3, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle3<- vehicle3 %>% left_join(crash_lookup, by="Crash_ID")
victim3<- victim3 %>% left_join(crash_lookup, by="Crash_ID")

crash4$Crash_ID_new<- paste0(crash4$Crash_ID, crash4$FIR_No, crash4$Police_Station)
crash_lookup<- subset(crash4, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle4<- vehicle4 %>% left_join(crash_lookup, by="Crash_ID")
victim4<- victim4 %>% left_join(crash_lookup, by="Crash_ID")

crash<- rbind(crash1, crash2,crash3,crash4)
vehicle<- rbind(vehicle1, vehicle2,vehicle3,vehicle4)
victim<- rbind(victim1, victim2,victim3,victim4)

rm(crash1, crash2, crash3, crash4)
rm(vehicle1, vehicle2, vehicle3, vehicle4)
rm(victim1, victim2, victim3, victim4)

################################################################################
# Assigning crash_ID to those vehicle where it is not available
################################################################################

x<-unique(vehicle$Vehicle_ID[which(is.na(vehicle$Crash_ID))])
victim <- victim[-c(which(victim$Vehicle_ID %in% x)),]

################################################################################
# Joining crash file with police station look up file for district, state and police station names
################################################################################

colnames(pstation)[1]<-"Police_Station"
crash<- crash %>% left_join(pstation, by="Police_Station")
crash_lookup<- crash[,c("Crash_ID_new", "FIR_No", "Police_Station", "Time_Of_Crash", "Date_Of_Crash", "Crash_Severity", "Collision_Type", "PS_Dist", "PS_State", "PS_Name", "Note", "Road_1", "Road_2", "Road_3", "Distance_From_Landmark")]

################################################################################
# Adding crash details to vehicle file
################################################################################

vehicle <- vehicle %>% left_join(crash_lookup, by="Crash_ID_new")

vehicle$Vehicle_ID_new<- paste0(vehicle$Crash_ID_new,vehicle$Vehicle_ID)

################################################################################
# Adding crash details to victim file
################################################################################


victim$Vehicle_ID_new<- paste0(victim$Crash_ID_new,victim$Vehicle_ID)

#victim<- victim[,-which(colnames(victim) %in% c("Vehicle_ID", "Crash_ID", "Crash_ID_new"))]

victim <- victim %>% left_join(vehicle, by="Vehicle_ID_new")

victim$Impacting_VehOrObject[ which ( victim$Road_User=="Pedestrian" & 
                                       victim$Impacting_VehOrObject=="Pedestrian") ]<- 
  victim$Vehicle_Type[ which ( victim$Road_User=="Pedestrian" & 
                                 victim$Impacting_VehOrObject=="Pedestrian")]

victim$Vehicle_Type[which(victim$Road_User=="Pedestrian" )]<- "Pedestrian"


################################################################################
## adding in geocodes from crash file with geocodes
################################################################################

crash_gps<- as.data.frame(read_xlsx('data/2021/2021 FIR Delhi geocoded.xlsx',sheet ="Sheet1"))

crash_gps<- subset(crash_gps, select=c("FIR No.", "Police Station", "Latitude", "Longitude"))

colnames(crash_gps)[2]<-"PS_Name"
colnames(crash_gps)[1]<-"FIR_No"

# stripping spaces and punctuations from names and making them capital


#crash fil

crash$PS_Name<- str_to_upper(crash$PS_Name)

crash$PS_Name <- str_replace_all(crash$PS_Name, "[[:punct:]]", "")

crash$PS_Name <- str_replace_all(crash$PS_Name, " ", "")

# crash file with geocodes

crash_gps$PS_Name<- str_to_upper(crash_gps$PS_Name)

crash_gps$PS_Name <- str_replace_all(crash_gps$PS_Name, "[[:punct:]]", " ")

crash_gps$PS_Name <- str_replace_all(crash_gps$PS_Name, " ", "")

crash_gps$FIR_No<- as.character(crash_gps$FIR_No)

#victim file

victim$PS_Name<- str_to_upper(victim$PS_Name)

victim$PS_Name <- str_replace_all(victim$PS_Name, "[[:punct:]]", "")

victim$PS_Name <- str_replace_all(victim$PS_Name, " ", "")

# combining geocodes into victim file

victim <- victim %>% left_join(crash_gps, by=c("FIR_No","PS_Name"))


################################################################################
# Simplifying/replacing values
################################################################################

##simplifying vehicle type names and Impacting Vehicle or Object type names:


victim$Victim_Vehicle_Type <- victim$Vehicle_Type

##simplifying vehicle type names
victim$Victim_Vehicle_Type[which(victim$Vehicle_Type %in% c("Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim$Victim_Vehicle_Type[which(victim$Vehicle_Type %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim$Victim_Vehicle_Type[which(victim$Vehicle_Type %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim$Victim_Vehicle_Type[which(victim$Vehicle_Type %in% c("Mini Bus"))]<-"Bus"
victim$Victim_Vehicle_Type[which(victim$Vehicle_Type %in% c("Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Others"
victim$Victim_Vehicle_Type[which(victim$Vehicle_Type %in% c("Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private"))]<-"M3W"


victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("Wall", "Tree","Pole", "Guard Rail", "Median", "Other Road Infrastructure", "Shoulder"))]<-"Fixed object"
victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Other"
victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private"))]<-"M3W"
victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("Mini Bus"))]<-"Bus"
victim$Impacting_VehOrObject[which(victim$Impacting_VehOrObject %in% c("Bicycle - Manual", "Ambulance", "Construction Vehicle","Animal Drawn Vehicle","Cycle Rickshaw - Manual", "Others", "Cycle Rickshaw"))]<-"Other"



################################################################################
# Only people who died
################################################################################

victim_f<- victim[which(victim$Injury_Category %in% c("Fatal Injury with Offsite Death","Fatal Injury with Onsite Death")),]


################################################################################
# Tables needed
################################################################################

View(victim_f %>% 
       group_by(Victim_Vehicle_Type) %>% 
       summarise(n()))

View(  victim_f %>% 
       
       # remove NA values, considering them to be the unknown cases
       filter(!is.na(Victim_Vehicle_Type)) %>% 
         
       filter(Victim_Vehicle_Type!="Unknown") %>% 
       
       group_by(Victim_Vehicle_Type) %>% 
       
       summarise(n = n()) %>%
         
       mutate(percentage = round(n*100 / sum(n)))
     
     )

View(  victim_f %>% 
       filter(Impacting_VehOrObject!="Unknown") %>% 
       group_by(Impacting_VehOrObject) %>% 
       summarise(n = n()) %>%
       mutate(percentage = round(n*100 / sum(n)))
     )


View(victim_f %>% 
       filter(Victim_Vehicle_Type!="Unknown") %>% 
       filter(Impacting_VehOrObject!="Unknown") %>%
       group_by(Victim_Vehicle_Type, Impacting_VehOrObject) %>% 
       summarise(n = n()) %>% 
       mutate(percentage = round(n*100/sum(n))) %>% 
       select(-c("n")) %>% 
       spread(Impacting_VehOrObject, percentage) %>% 
       replace(is.na(.), 0)
)


#victim_f %>% filter(Age!="Age: Unknown") %>% filter(Sex!="Unknown") %>% filter(Sex!=) %>% group_by(Age, Sex) %>% summarize(n())
