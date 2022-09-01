
################################################################################
# Reading in 2019 crash data
################################################################################


crash1 <- read.csv("data/2019/1_Crash_Form_1.csv")
crash2 <- read.csv("data/2019/1_Crash_Form_2.csv")

ps_names1 <- read.csv("data/2019/Usys1_2_PSDetail_1.csv")
ps_names2 <- read.csv("data/2019/Usys1_2_PSDetail_2.csv")


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

vehicle1 <- read.csv("data/2019/2_Vehicle_Form_1.csv")
vehicle2 <- read.csv("data/2019/2_Vehicle_Form_2.csv")

victim1 <- read.csv("data/2019/3_Person_Form_1.csv")
victim2 <- read.csv("data/2019/3_Person_Form_2.csv")

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

crash_19<- rbind(crash1, crash2)
vehicle_19<- rbind(vehicle1, vehicle2)
victim_19<- rbind(victim1, victim2)

rm(crash1, crash2)
rm(vehicle1, vehicle2)
rm(victim1, victim2)

crash_19$FIR_No <- substr(crash_19$FIR_No, 1, 4)

################################################################################
# removing entries without crash id
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

vehicle_19$Vehicle_ID_new <- paste0(vehicle_19$Crash_ID_new,vehicle_19$Vehicle_ID)


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
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Tractor with Trailer", "Truck (Generic)","Agricultural Tractor", "Articulated Vehicle, Tractor Trailor", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Mini Bus", "Bus (DTC Delhi)", "Bus (Cluster)"))]<-"Bus"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Bicycle - Manual"))]<-"Bicycle"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Cycle Rickshaw" , "Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Other"
victim_19$Victim_Vehicle_Type[which(victim_19$Vehicle_Type %in% c("Three Wheeler - Passenger", "E-rickshaw","Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private","Light Commercial Vehicle"))]<-"M3W"

victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Truck (Generic)", "Agricultural Tractor", "Articulated Vehicle, Tractor Trailor","Light Commercial Vehicle", "Two-Axle Heavy Commercial Vehicle", "Multi-Axle Heavy Commercial Vehicle", "Intermediate Commercial Vehicle","Two-Axle Medium Commercial Vehicle", "Two_Axle Heavy Commercial Vehicle"))]<-"Truck/Tractor"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Car - Van, Passenger", "Car - Jeep, Passenger", "Car - Passenger", "Car - Commercial", "Car - Van, Commercial", "Car - SUV, Passenger", "Car - SUV, Commercial", "Car - Jeep, Commercial"))]<-"Car"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("MTW - Motorcycle Light", "MTW - Scooter/Moped","MTW - Motorcycle Heavy"))]<-"MTW"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Wall", "Tree","Pole", "Guard Rail", "Median", "Other Road Infrastructure", "Shoulder", "Barrier", "Curb" ))]<-"Fixed object"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Animal", "Ambulance", "Construction Vehicle","Hand Drawn Vehicle","Cycle Rickshaw - Manual", "Animal Drawn Vehicle"))]<-"Other"
victim_19$Impacting_VehOrObject[which(victim_19$Impacting_VehOrObject %in% c("Three Wheeler - Goods", "Three Wheeler - Passenger, Commercial", "Three Wheeler - Passenger, Private", "Three Wheeler - Passenger"))]<-"M3W"
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



view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("PED"))]<-"Pedestrian"
view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("TWW"))]<-"MTW"
view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("TSR", "GMS"))]<-"M3W"       # GMS - gramin seva vehicle taken as auto-rickshaw
view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("CAR"))]<-"Car"
view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("CYC"))]<-"Bicycle"
view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("TMP", "HTV", "TRC", "MBS", "DLV", "TNK"))]<-"Truck/Tractor"
view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("CYR", "AMB", "ERC", "HDC" ))]<-"Other"
view_19$VICTIMS_DP[which(view_19$VICTIMS_DP %in% c("DTC", "PAS"))]<-"Bus"

view_19$`OFFENDING VEHICLE_DP`[which(view_19$`OFFENDING VEHICLE_DP` %in% c("AMBULNC", "CRANE", "ERCAW"))] <- "Other"
view_19$`OFFENDING VEHICLE_DP`[which(view_19$`OFFENDING VEHICLE_DP` %in% c("BUS O S", "DTC BUS", "BUS OTR", "CTR BUS", "MIN.BUS", "BUS SCL", "TRL/CON"))] <- "Bus"
view_19$`OFFENDING VEHICLE_DP`[which(view_19$`OFFENDING VEHICLE_DP` %in% c("TEMPO", "HTV/GDS", "TRACTOR", "DELIVRY", "TANKER"))] <- "Truck/Tractor"
view_19$`OFFENDING VEHICLE_DP`[which(view_19$`OFFENDING VEHICLE_DP` %in% c("UNKNOWN"))] <- "Unknown"
view_19$`OFFENDING VEHICLE_DP`[which(view_19$`OFFENDING VEHICLE_DP` %in% c("S/C&M/C"))] <- "MTW"
view_19$`OFFENDING VEHICLE_DP`[which(view_19$`OFFENDING VEHICLE_DP` %in% c("PVT CAR", "TAXI"))] <- "Car"
view_19$`OFFENDING VEHICLE_DP`[which(view_19$`OFFENDING VEHICLE_DP` %in% c("GRM.SEW", "TSR"))] <- "M3W"

diff_victims_vehs <- 
  view_19[
    which(
      (view_19$Victim_Vehicle_Type!=view_19$VICTIMS_DP) | 
        (view_19$Impacting_VehOrObject!=view_19$`OFFENDING VEHICLE_DP`)
    ),
  ]

diff_victims_vehs <- diff_victims_vehs %>% filter(VICTIMS_DP!="SLF")


write_csv(diff_victims_vehs[,c(2,3,4,5,7,9)], "data/2019_doubtful_vehicle_type.csv")

# FIRs not there
#-------------------------------------------------------------------------------


missing_firs <- anti_join( DP_crash_level_data, victim_19, by=c("FIR_No", "PS_Name"))

write_csv(missing_firs[,c(2,3,4,5)], "data/missing_FIRs_2019.csv")

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
