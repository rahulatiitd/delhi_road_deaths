################################################################################
#### Reading in data ####
################################################################################

crash1 <- read.csv("data/2021/1_Crash_Table_1.csv")
crash2 <- read.csv("data/2021/1_Crash_Table_2.csv")
crash3 <- read.csv("data/2021/1_Crash_Table_3.csv")
crash4 <- read.csv("data/2021/1_Crash_Table_4.csv")

ps_names1 <- read.csv("data/2021/Usys1_2_PSDetail_1.csv")
ps_names2 <- read.csv("data/2021/Usys1_2_PSDetail_2.csv")
ps_names3 <- read.csv("data/2021/Usys1_2_PSDetail_3.csv")
ps_names4 <- read.csv("data/2021/Usys1_2_PSDetail_4.csv")


crash1$PS_Name <- NA
crash2$PS_Name <- NA
crash3$PS_Name <- NA
crash4$PS_Name <- NA


for (i in 1:nrow(crash1)){
  crash1$PS_Name[i] <- 
    ps_names1$PS_Name[ which( ps_names1$PS_ID == crash1$Police_Station[i] )]
}

for (i in 1:nrow(crash2)){
  crash2$PS_Name[i] <- 
    ps_names2$PS_Name[ which( ps_names2$PS_ID == crash2$Police_Station[i] )]
}

for (i in 1:nrow(crash3)){
  crash3$PS_Name[i] <- 
    ps_names3$PS_Name[ which( ps_names3$PS_ID == crash3$Police_Station[i] )]
}

for (i in 1:nrow(crash4)){
  crash4$PS_Name[i] <- 
    ps_names4$PS_Name[ which( ps_names4$PS_ID == crash4$Police_Station[i] )]
}

vehicle1 <- read.csv("data/2021/2_Vehicle_Table_1.csv")
vehicle2 <- read.csv("data/2021/2_Vehicle_Table_2.csv")
vehicle3 <- read.csv("data/2021/2_Vehicle_Table_3.csv")
vehicle4 <- read.csv("data/2021/2_Vehicle_Table_4.csv")

victim1 <- read.csv("data/2021/3_Victim_Table_1.csv")
victim2 <- read.csv("data/2021/3_Victim_Table_2.csv")
victim3 <- read.csv("data/2021/3_Victim_Table_3.csv")
victim4 <- read.csv("data/2021/3_Victim_Table_4.csv")

# Reading file that has police station names and districts 
pstation <- read.csv("data/2021/PS_lookup_181.csv")

################################################################################
#### Assigning crash_ID to vehicle and victim files ####
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
#### Removing the entries without crash ID ####
################################################################################

x <- unique( vehicle_21$Vehicle_ID[ which(is.na(vehicle_21$Crash_ID)) ])

victim_21 <- victim_21[-c(which(victim_21$Vehicle_ID %in% x)),]

################################################################################
#### Reassigning Victim IDs to be unique ####
################################################################################

for (i in 1:nrow(victim_21)){
  
  # check if this starting number is still ok w.r.t. 2019 and 2021
  victim_21$Victim_ID[i] <- 8565+i
}

################################################################################
# Removing unnecessary columns
################################################################################

crash_21 <- crash_21 %>% select(where(~!all(is.na(.))))

################################################################################
#### Joining crash file with police station look up file for district, state and police station names ####
################################################################################

crash_21<- crash_21 %>% left_join(pstation, by="PS_Name")

################################################################################
#### Adding crash details to vehicle file ####
################################################################################

vehicle_21 <- vehicle_21 %>% left_join(crash_21, by="Crash_ID_new")

vehicle_21$Vehicle_ID_new<- paste0(vehicle_21$Crash_ID_new,vehicle_21$Vehicle_ID)

################################################################################
#### Adding crash details to victim file ####
################################################################################


victim_21$Vehicle_ID_new<- paste0(victim_21$Crash_ID_new,victim_21$Vehicle_ID)

#victim<- victim[,-which(colnames(victim) %in% c("Vehicle_ID", "Crash_ID", "Crash_ID_new"))]

victim_21 <- victim_21 %>% left_join(vehicle_21, by="Vehicle_ID_new")

victim_21$Road_User[which(victim_21$Road_User=="Disembarked Vehicle Occupant")]<- "Pedestrian"

victim_21[which(victim_21$Road_User=="Pedestrian" & 
                  victim_21$Impacting_VehOrObject=="Pedestrian"), 
          c("Vehicle_Type", "Impacting_VehOrObject")] <- 
  rev(victim_21[which(victim_21$Road_User=="Pedestrian" & 
                        victim_21$Impacting_VehOrObject=="Pedestrian"),
                c("Vehicle_Type", "Impacting_VehOrObject")])

victim_21$Vehicle_Type[which(victim_21$Road_User=="Pedestrian")]<- "Pedestrian"


################################################################################
#### adding in geocodes from crash file with geocodes ####
################################################################################

# (this is being done because of a glitch which caused all the geocordinate 
# entries to be integers in the access form)


crash_gps_21<- read.csv('data/2021/2021 FIR Delhi geocoded.csv')

crash_gps_21<- subset(crash_gps_21, select=c("FIR_No", 
                                             "PS_Name", 
                                             "Latitude", 
                                             "Longitude"))

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

ps_name_change <- read.csv("data/2021/ps_name_change.csv")

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

# # testing the unmatched ones - to be commented in final code

# crash_21_iitd <- data.frame(crash_21$FIR_No, crash_21$PS_Name)
# crash_21_bbrg <- data.frame(crash_gps_21$FIR_No, crash_gps_21$PS_Name)
# colnames(crash_21_iitd) <- c("FIR_No", "PS_Name")
# colnames(crash_21_bbrg) <- c("FIR_No", "PS_Name")
# iitd_bbrg_21 <- setdiff(crash_21_iitd, crash_21_bbrg)
# bbrg_iitd_21 <- setdiff(crash_21_bbrg, crash_21_iitd)
# unique(iitd_bbrg_21$PS_Name)
# unique(bbrg_iitd_21$PS_Name)


# combining geocodes into victim file

victim_21 <- victim_21 %>% left_join(crash_gps_21, by=c("FIR_No","PS_Name"))
crash_21 <- crash_21 %>% left_join(crash_gps_21, by=c("FIR_No","PS_Name"))




################################################################################
#### Simplifying/replacing values ####
################################################################################

##simplifying vehicle type names and Impacting Vehicle or Object type names:


victim_21$Victim_Vehicle_Type <- victim_21$Vehicle_Type

##simplifying victim vehicle type names

victim_21$Victim_Vehicle_Type [
  which(victim_21$Vehicle_Type %in% c("Articulated Vehicle, Tractor Trailor",
                                      "Light Commercial Vehicle", 
                                      "Two-Axle Heavy Commercial Vehicle", 
                                      "Multi-Axle Heavy Commercial Vehicle", 
                                      "Intermediate Commercial Vehicle",
                                      "Two-Axle Medium Commercial Vehicle", 
                                      "Two_Axle Heavy Commercial Vehicle", 
                                      "Construction Vehicle"
                                      )
        )]<-"Truck/Tractor"

victim_21$Victim_Vehicle_Type[
  which(victim_21$Vehicle_Type %in% c("Car - Van, Passenger", 
                                      "Car - Jeep, Passenger", 
                                      "Car - Passenger", 
                                      "Car - Commercial", 
                                      "Car - Van, Commercial", 
                                      "Car - SUV, Passenger", 
                                      "Car - SUV, Commercial", 
                                      "Car - Jeep, Commercial"
                                      )
        )]<-"Car"

victim_21$Victim_Vehicle_Type[
  which(victim_21$Vehicle_Type %in% c("MTW - Motorcycle Light", 
                                      "MTW - Scooter/Moped",
                                      "MTW - Motorcycle Heavy"
                                      )
        )]<-"MTW"

victim_21$Victim_Vehicle_Type[
  which(victim_21$Vehicle_Type %in% c("Mini Bus"))]<-"Bus"

victim_21$Victim_Vehicle_Type[
  which(victim_21$Vehicle_Type %in% c("Bicycle - Manual"))]<-"Bicycle"

victim_21$Victim_Vehicle_Type[
  which(victim_21$Vehicle_Type %in% c("Ambulance",
                                      "Hand Drawn Vehicle",
                                      "Cycle Rickshaw - Manual", 
                                      "Animal Drawn Vehicle",
                                      "Others"
                                      )
        )]<-"Other"

victim_21$Victim_Vehicle_Type[
  which(victim_21$Vehicle_Type %in% c("Three Wheeler - Goods", 
                                      "Three Wheeler - Passenger, Commercial", 
                                      "Three Wheeler - Passenger, Private"
                                      )
        )]<-"M3W"



##simplifying impacting vehicle/ object type names

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("Articulated Vehicle, Tractor Trailor",
                                               "Light Commercial Vehicle", 
                                               "Two-Axle Heavy Commercial Vehicle", 
                                               "Multi-Axle Heavy Commercial Vehicle", 
                                               "Intermediate Commercial Vehicle",
                                               "Two-Axle Medium Commercial Vehicle", 
                                               "Two_Axle Heavy Commercial Vehicle",
                                               "Construction Vehicle"
                                               )
        )]<-"Truck/Tractor"

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("Car - Van, Passenger", 
                                               "Car - Jeep, Passenger", 
                                               "Car - Passenger", 
                                               "Car - Commercial", 
                                               "Car - Van, Commercial", 
                                               "Car - SUV, Passenger", 
                                               "Car - SUV, Commercial", 
                                               "Car - Jeep, Commercial"
                                               )
        )]<-"Car"

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("MTW - Motorcycle Light", 
                                               "MTW - Scooter/Moped",
                                               "MTW - Motorcycle Heavy"
                                               )
        )]<-"MTW"

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("Wall", 
                                               "Tree",
                                               "Pole", 
                                               "Guard Rail", 
                                               "Median", 
                                               "Other Road Infrastructure", 
                                               "Shoulder"
                                               )
        )]<-"Fixed object"

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("Ambulance",
                                               "Hand Drawn Vehicle",
                                               "Cycle Rickshaw - Manual", 
                                               "Animal Drawn Vehicle", 
                                               "Others", 
                                               "Cycle Rickshaw"
                                               )
        )]<-"Other"

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("Three Wheeler - Goods", 
                                               "Three Wheeler - Passenger, Commercial", 
                                               "Three Wheeler - Passenger, Private"
                                               )
        )]<-"M3W"

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("Mini Bus"))]<-"Bus"

victim_21$Impacting_VehOrObject[
  which(victim_21$Impacting_VehOrObject %in% c("Bicycle - Manual"))]<-"Bicycle"


################################################################################
#### Only people who died - 2021 ####
################################################################################

victim_f_21 <- 
  victim_21[which(victim_21$Injury_Category %in% 
                    c("Fatal Injury with Offsite Death",
                      "Fatal Injury with Onsite Death")),]


# getting rid of useless columns
victim_f_21 <- victim_f_21 %>% select(where(~!all(is.na(.))))

################################################################################
# TODO - compare with delhi police data 2021
################################################################################

# just making sure there no NA values in FIR no. and police station

victim_21 <- victim_21 %>% drop_na(PS_Name) %>% drop_na(FIR_No)

DP_crash_level_data <- read.csv("data/2021/1206 Fatal crashes 2021 .csv")

# Prepping the delhi police data to compare
#-------------------------------------------------------------------------------

# changing column names to be able to merge
colnames(DP_crash_level_data) <- c("SL NO.", "PS_Name", "No_of_crash" ,"FIR_No", "U/S", 
                                   "Date_Of_Crash_DP", "OFFENDING_VEHICLE_DP",  
                                   "VICTIMS_DP", "PLACE OF OCCURENCE_DP", 
                                   "ROAD NAME_DP", "YEAR_DP"  )

# changing police station names to be able to merge 
DP_crash_level_data$PS_Name <- str_replace_all(DP_crash_level_data$PS_Name, " ", "")

# fixing some compatibility issues
victim_21$FIR_No <- as.double(victim_21$FIR_No)

#removing the NA column which is not letting the left_join happen
DP_crash_level_data <- select(DP_crash_level_data, -c(12))

# joining the data
victim_21 <- victim_21 %>% left_join(DP_crash_level_data, by=c("FIR_No", "PS_Name") )

# selecting necessary columns to view and compare
view_21 <- 
  victim_21 %>% 
  filter(Injury_Category %in% c("Fatal Injury with Offsite Death",
                                "Fatal Injury with Onsite Death")) %>% 
  select(Crash_ID, Entry_Personnel, 
         Victim_ID, FIR_No, PS_Name, Date_Of_Crash, 
         Victim_Vehicle_Type, VICTIMS_DP, 
         Impacting_VehOrObject, `OFFENDING_VEHICLE_DP`,
         Note)

# Harmonizing the columns so that the vehicle categories can be compared 

view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("PED","PEDSTRN"))]<-"Pedestrian"
view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("TWW","S/C&M/C"))]<-"MTW"
view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("TSR", "GMS","GRM.SEW"))]<-"M3W"       # GMS - gramin seva vehicle taken as auto-rickshaw
view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("CAR","PVT CAR","TAXI"))]<-"Car"
view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("CYC","CYCLE"))]<-"Bicycle"
view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("TMP", "HTV", "TRC", "MBS", 
                                                   "DLV", "TNK", "TRL/CON","TEMPO",
                                                   "HTV/GDS","TANKER","DELIVRY"))]<-"Truck/Tractor"
view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("CYR", "AMB", "ERC", "HDC","C.RICKW",
                                                   "HANDCRT","ERCAW","CRANE"))]<-"Other"
view_21$VICTIMS_DP[which(view_21$VICTIMS_DP %in% c("DTC", "PAS","PASSNGR"))]<-"Bus"

view_21$`OFFENDING_VEHICLE_DP`[which(view_21$`OFFENDING_VEHICLE_DP` %in% c("AMBULNC", "CRANE", "ERCAW"))] <- "Other"
view_21$`OFFENDING_VEHICLE_DP`[which(view_21$`OFFENDING_VEHICLE_DP` %in% c("BUS O S", "DTC BUS", "BUS OTR", "CTR BUS", 
                                                                           "MIN.BUS", "BUS SCL"))] <- "Bus"
view_21$`OFFENDING_VEHICLE_DP`[which(view_21$`OFFENDING_VEHICLE_DP` %in% c("TEMPO", "HTV/GDS", "TRACTOR", "DELIVRY", 
                                                                           "TANKER", "Tractor with Trailer","TRL/CON"
))] <- "Truck/Tractor"
view_21$`OFFENDING_VEHICLE_DP`[which(view_21$`OFFENDING_VEHICLE_DP` %in% c("UNKNOWN"))] <- "Unknown"
view_21$`OFFENDING_VEHICLE_DP`[which(view_21$`OFFENDING_VEHICLE_DP` %in% c("S/C&M/C"))] <- "MTW"
view_21$`OFFENDING_VEHICLE_DP`[which(view_21$`OFFENDING_VEHICLE_DP` %in% c("PVT CAR", "TAXI"))] <- "Car"
view_21$`OFFENDING_VEHICLE_DP`[which(view_21$`OFFENDING_VEHICLE_DP` %in% c("GRM.SEW", "TSR"))] <- "M3W"

# 
view_21[which(view_21$Victim_Vehicle_Type==view_21$OFFENDING_VEHICLE_DP & 
                view_21$VICTIMS_DP==view_21$Impacting_VehOrObject), 
        c("Victim_Vehicle_Type", "Impacting_VehOrObject")] <- 
  rev(view_21[which(view_21$Victim_Vehicle_Type==view_21$OFFENDING_VEHICLE_DP & 
                      view_21$VICTIMS_DP==view_21$Impacting_VehOrObject),
              c("Victim_Vehicle_Type", "Impacting_VehOrObject")])

# finding the non-matching rows

diff_victims_vehs <- 
  view_21[
    which(
      (view_21$Victim_Vehicle_Type != view_21$VICTIMS_DP) | 
        (view_21$Impacting_VehOrObject != view_21$OFFENDING_VEHICLE_DP)
    ),
  ]

diff_victims_vehs <- diff_victims_vehs %>% filter(VICTIMS_DP!="SELF")

write_csv(diff_victims_vehs[,c(1,2,4,5,6,7,8,9,10,11)], "data/2021_doubtful_vehicle_type.csv")

# FIRs not entered into data acc to Delhi Police dataset
#-------------------------------------------------------------------------------
missing_firs <- anti_join( DP_crash_level_data, victim_21, by=c("FIR_No", "PS_Name"))

write_csv(missing_firs[,c(2,4,5,6)], "data/missing_FIRs_2021.csv")











