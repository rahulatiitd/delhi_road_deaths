library(dplyr)
library(readxl)
library(stringr)
setwd('C:/Users/goelr/Work/Datasets/Delhi crash database/2021 Exctracted accident form Yatin')

crash1<- read_xlsx('1_Crash_Table_1.xlsx',sheet ="1_Crash_Table")
vehicle1<- read_xlsx('2_Vehicle_Table_1.xlsx',sheet ="2_Vehicle_Table")
victim1<- read_xlsx('3_Victim_Table_1.xlsx',sheet ="3_Victim_Table")

crash2<- read_xlsx('1_Crash_Table_2.xlsx',sheet ="1_Crash_Table")
vehicle2<- read_xlsx('2_Vehicle_Table_2.xlsx',sheet ="2_Vehicle_Table")
victim2<- read_xlsx('3_Victim_Table_2.xlsx',sheet ="3_Victim_Table")

crash3<- read_xlsx('1_Crash_Table_3.xlsx',sheet ="1_Crash_Table")
vehicle3<- read_xlsx('2_Vehicle_Table_3.xlsx',sheet ="2_Vehicle_Table")
victim3<- read_xlsx('3_Victim_Table_3.xlsx',sheet ="3_Victim_Table")

crash4<- read_xlsx('1_Crash_Table_4.xlsx',sheet ="1_Crash_Table")
vehicle4<- read_xlsx('2_Vehicle_Table_4.xlsx',sheet ="2_Vehicle_Table")
victim4<- read_xlsx('3_Victim_Table_4.xlsx',sheet ="3_Victim_Table")

##assigning crash_ID to vehicle and victim files
crash1$Crash_ID_new<- paste0(crash1$Crash_ID,crash1$FIR_No, crash1$Police_Station)
crash1_lookup<- subset(crash1, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle1<- vehicle1 %>% left_join(crash1_lookup, by="Crash_ID")
victim1<- victim1 %>% left_join(crash1_lookup, by="Crash_ID")

crash2$Crash_ID_new<- paste0(crash2$Crash_ID,crash2$FIR_No, crash2$Police_Station)
crash2_lookup<- subset(crash2, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle2<- vehicle2 %>% left_join(crash2_lookup, by="Crash_ID")
victim2<- victim2 %>% left_join(crash2_lookup, by="Crash_ID")

crash3$Crash_ID_new<- paste0(crash3$Crash_ID,crash3$FIR_No, crash3$Police_Station)
crash3_lookup<- subset(crash3, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle3<- vehicle3 %>% left_join(crash3_lookup, by="Crash_ID")
victim3<- victim3 %>% left_join(crash3_lookup, by="Crash_ID")

crash4$Crash_ID_new<- paste0(crash4$Crash_ID,crash4$FIR_No, crash4$Police_Station)
crash4_lookup<- subset(crash4, select=c("Crash_ID", "Police_Station", "Crash_ID_new"))
vehicle4<- vehicle4 %>% left_join(crash4_lookup, by="Crash_ID")
victim4<- victim4 %>% left_join(crash4_lookup, by="Crash_ID")

crash<- rbind(crash1, crash2,crash3,crash4)
vehicle<- rbind(vehicle1, vehicle2,vehicle3,vehicle4)
victim<- rbind(victim1, victim2,victim3,victim4)

#assigning crash_ID to those vehicle where it is not available
x<-unique(vehicle$Vehicle_ID[which(is.na(vehicle$Crash_ID))])
victim$Crash_ID[which(victim$Vehicle_ID %in% x)]

##reading file that has police station names and districts 
pstation<- read.csv("PS_lookup.csv")

#joining crash file with police station look up file for district, state and police station names (for Raipur and Balod)
colnames(pstation)[1]<-"Police_Station"
crash<- crash %>% left_join(pstation, by="Police_Station")

crash_lookup<- crash[,c("Crash_ID_new", "FIR_No", "Police_Station", "Time_Of_Crash", "Date_Of_Crash", "Crash_Severity", "Collision_Type", "PS_Dist", "PS_State", "PS_Name", "Note", "Road_1", "Road_2", "Road_3", "Distance_From_Landmark")]

##adding crash details to vehicle file
vehicle <- vehicle %>% left_join(crash_lookup, by="Crash_ID_new")
vehicle$Vehicle_ID_new<- paste0(vehicle$Crash_ID_new,vehicle$Vehicle_ID)
victim$Vehicle_ID_new<- paste0(victim$Crash_ID_new,victim$Vehicle_ID)
#victim<- victim[,-which(colnames(victim) %in% c("Vehicle_ID", "Crash_ID", "Crash_ID_new"))]
victim <- victim %>% left_join(vehicle, by="Vehicle_ID_new")
victim$Impacting_VehOrObject[which(victim$Road_User=="Pedestrian" & victim$Impacting_VehOrObject=="Pedestrian")]<- victim$Vehicle_Type[which(victim$Road_User=="Pedestrian" & victim$Impacting_VehOrObject=="Pedestrian")]
victim$Vehicle_Type[which(victim$Road_User=="Pedestrian" )]<- "Pedestrian"

victim_f<- victim[which(victim$Injury_Category %in% c("Fatal Injury with Offsite Death","Fatal Injury with Onsite Death")),]

View(victim_f[,c("Crash_ID_new", "Road_User","Injury_Category", "Vehicle_Occupant_Type", "Vehicle_Type","Impacting_VehOrObject")])

View(victim_f %>% group_by(Vehicle_Type) %>% summarise(n()))

write.csv((victim_f[which(victim_f$Vehicle_Type=="Unknown" | is.na(victim_f$Vehicle_Type)),c("Crash_ID_new", "Note")]), "lookup_corrected_vehicle_type.csv")


##crash file with geocodes
crash_gps<- as.data.frame(read_xlsx('2021 FIR Delhi geocoded.xlsx',sheet ="Sheet1"))
crash_gps<- subset(crash_gps, select=c("FIR No.", "Police Station", "Latitude", "Longitude"))
colnames(crash_gps)[2]<-"PS_Name"
colnames(crash_gps)[1]<-"FIR_No"
victim_f$FIR_No<- as.double(victim_f$FIR_No)
victim_f$PS_Name<- str_to_upper(victim_f$PS_Name,locale="en")
victim_f$PS_full<- victim_f$PS_Name
crash_gps<-crash_gps %>% left_join(victim_f, by=c("FIR_No","PS_Name"))
