################################################################################
# Reading in 2020, 2013-16 and 2016-2018 data sets ("silver" quality)
################################################################################

# crash_20 <- read_xlsx('data/2020/FIR Fatal crash data 2020.xlsx',sheet ="Sheet1")

# changing to csv file in excel fixed some formatting issues
crash_20 <- read.csv("data/2020/FIR Fatal crash data 2020.csv")

crash_20 <- crash_20[which(!is.na(crash_20$S..no.)),]

################################################################################
# Making victim Table out of crash tables - 2020
################################################################################

victim_20 <- 
  data.frame(
  matrix(
    nrow= sum(crash_20$Total.killed), 
    ncol = length(colnames(crash_20))))

colnames(victim_20) <- colnames (crash_20)

# Assigning a unique victim id for each victim
# starting from 7000 here.


# Assigning each crash to the victim 
i_victim <- 1

victim_20$Victim_ID <- NA

for (i_crash in 1:nrow(crash_20)){
  
  victims <- crash_20$Total.killed[i_crash] # no injured victims taken - only dead!
  
  while(victims > 0){
    
    victim_20[i_victim,] <- crash_20[i_crash,]
    
    # giving a unique victim ID starting from 7001
    victim_20$Victim_ID[i_victim] <- 7000 + i_victim    
    
    if (victims == 2){
      victim_20$Age..killed.1.[i_victim] <- 
        victim_20$Age..killed.2[i_victim]
      
        victim_20$Gender..killed.2.[i_victim]
      
      victim_20$Road.user.type..killed.1[i_victim] <- 
        victim_20$Road.user.type..killed.2.[i_victim]
    }
    
    if (victims == 3){
      victim_20$Age..killed.1.[i_victim] <- 
        victim_20$Age..killed.3[i_victim] 
      
      victim_20$Gender..killed.1.[i_victim] <- 
        victim_20$Gender..killed.3.[i_victim]
      
      victim_20$Road.user.type..killed.1[i_victim] <- 
        victim_20$Road.user.type..killed.2..1 [i_victim]
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



victims_20_checked <- read.csv("data/2020/2020_multiple_death_FIRs.csv")

i <- 1

while (i <= nrow(victims_20_checked)){
  
  
  # add victim if not already there in victim_20 file
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
  victim_20$Victim.user.type..as.per.Delhi.Traffic.Police.[index] <-
    victims_20_checked$Victim_Vehicle_Type[i]
  
  # Age
  victim_20$Age..killed.1.[index] <- victims_20_checked$Age[i]
  
  # Sex
  victim_20$Gender..killed.1.[index] <- victims_20_checked$Sex[i]
  
  # victim vehicle type
  victim_20$Road.user.type..killed.1.[index] <- 
    victims_20_checked$Vehicle_Occupant_Type[i]
  
  # impacting veh or object
  victim_20$Responsible.road.user[index] <- victims_20_checked$Impacting_VehOrObject[i]
  
  
  i <- i+1
  
  
  
}

################################################################################
# Comparing with delhi police data 2016-2020 
################################################################################

# check for na values just in case
victim_20 <- victim_20 %>% drop_na(Police.station) %>% drop_na(FIR.No.)

DP_crash_level_data <- read.csv("data/2016_2020_crash_level_data.csv")

x <- unique(DP_crash_level_data$U.S)

x <-x[c(3,5,7)]

DP_crash_level_data <- 
  DP_crash_level_data %>% 
  filter(U.S %in% x) %>% 
  filter(YEAR==2020)

colnames(DP_crash_level_data) <- c("SL.NO", "FIR.No.", "PS_Name" , "U.S", 
                                   "Date_Of_Crash_DP", "OFFENDING_VEHICLE_DP",  
                                   "VICTIMS_DP", "PLACE OF OCCURANCE_DP", 
                                   "ROAD NAME_DP", "YEAR_DP"  )

names(victim_20)[names(victim_20) == "Police.station"] <- "PS_Name"
names(victim_20)[names(victim_20) == "Victim.user.type..as.per.Delhi.Traffic.Police."] <- "Victim_user_type"

victim_20 <- victim_20 %>% left_join(DP_crash_level_data, by=c("FIR.No.", "PS_Name") )

view_20 <- 
  victim_20 %>% 
  select(Victim_ID, FIR.No., PS_Name, Date.of.crash, 
         Victim_user_type, VICTIMS_DP, 
         Responsible.road.user, OFFENDING_VEHICLE_DP)

# changes in 2020 victim vehicle type
#-------------------------------------------------------------------------------
view_20$Victim_user_type[
  which(view_20$`Victim user type (as per Delhi Traffic Police)` %in% c("PED", 
                                                                        "Pedestrians", 
                                                                        "Disembarked Vehicle Occupant", 
                                                                        "Non-Road User Pedestrian"
                                                                        ))]<-"Pedestrian"

view_20$Victim_user_type[
  which(view_20$`Victim user type (as per Delhi Traffic Police)` %in% c("TWW", 
                                                                        "Motorised two wheeler"
                                                                        ))]<-"MTW"
view_20$Victim_user_type[
  which(view_20$`Victim user type (as per Delhi Traffic Police)` %in% c("Tempo/ Tractor", 
                                                                        "Heavy vehicles", 
                                                                        "HTV", 
                                                                        "TMP"
                                                                        ))]<-"Truck/Tractor"
view_20$Victim_user_type[
  which(view_20$`Victim user type (as per Delhi Traffic Police)` %in% c("Cyclists"
                                                                        ))]<-"Bicycle"
view_20$Victim_user_type[
  which(view_20$`Victim user type (as per Delhi Traffic Police)` %in% c("TSR", 
                                                                        "Auto-rickshaw- TSR"
                                                                        ))]<-"M3W"
view_20$Victim_user_type[
  which(view_20$`Victim user type (as per Delhi Traffic Police)` %in% c("Cycle rickshaw", 
                                                                        "ERC"
                                                                        ))]<-"Other"

# changes in DP victim vehicle type
#------------------------------------------------------------------------------
view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("PED"))]<-"Pedestrian"

view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("TWW"))]<-"MTW"

view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("CYC"))]<-"Bicycle"

view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("SLF"))]<-"Self"

view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("CAR"))]<-"Car"

view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("TMP", 
                                                   "HTV", 
                                                   "TRC", 
                                                   "MBS", 
                                                   "DLV", 
                                                   "TNK"
                                                   ))]<-"Truck/Tractor"

view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("TSR", 
                                                   "GMS"                        # GMS - gramin seva vehicle taken as auto-rickshaw
                                                   ))]<-"M3W"    

view_20$VICTIMS_DP[which(view_20$VICTIMS_DP %in% c("CYR", 
                                                   "AMB", 
                                                   "ERC", 
                                                   "HDC" 
                                                   ))]<-"Other"

# changes in 2020 other vehicle type
#-------------------------------------------------------------------------------
view_20$Responsible.road.user[which(view_20$Responsible.road.user %in% c("Motorised two wheeler"))] <- "MTW"

view_20$Responsible.road.user[which(view_20$Responsible.road.user %in% c("Car/ Jeep/ Van/ Taxi"))] <- "Car"

view_20$Responsible.road.user[which(view_20$Responsible.road.user %in% c("tempo-truck eicher", 
                                                                             "Heavy vehicles", 
                                                                             "Tempo/ Tractor", 
                                                                             "HTV", 
                                                                             "TMP"
                                                                             ))] <- "Truck/Tractor"

view_20$Responsible.road.user[which(view_20$Responsible.road.user %in% c("TSR", 
                                                                             "Auto-rickshaw- TSR"
                                                                             ))] <- "M3W"


# changes in DP other vehicle type
#-------------------------------------------------------------------------------
view_20$OFFENDING_VEHICLE_DP[which(view_20$OFFENDING_VEHICLE_DP %in% c("S/C&M/C"))] <- "MTW"
view_20$OFFENDING_VEHICLE_DP[which(view_20$OFFENDING_VEHICLE_DP %in% c("PVT CAR", 
                                                                           "TAXI"
                                                                           ))] <- "Car"
view_20$OFFENDING_VEHICLE_DP[which(view_20$OFFENDING_VEHICLE_DP %in% c("TEMPO", 
                                                                           "HTV/GDS", 
                                                                           "TRACTOR", 
                                                                           "DELIVRY", 
                                                                           "TANKER", 
                                                                           "CRANE"
                                                                           ))] <- "Truck/Tractor"

view_20$OFFENDING_VEHICLE_DP[which(view_20$OFFENDING_VEHICLE_DP %in% c("BUS O S", 
                                                                           "DTC BUS", 
                                                                           "BUS OTR", 
                                                                           "CTR BUS", 
                                                                           "MIN.BUS", 
                                                                           "BUS SCL", 
                                                                           "TRL/CON"
                                                                           ))] <- "Bus"

view_20$OFFENDING_VEHICLE_DP[which(view_20$OFFENDING_VEHICLE_DP %in% c("GRM.SEW", 
                                                                           "TSR"
                                                                           ))] <- "M3W"

view_20$OFFENDING_VEHICLE_DP[which(view_20$OFFENDING_VEHICLE_DP %in% c("AMBULNC", 
                                                                           "MILITRY", 
                                                                           "ERCAW"
                                                                           ))] <- "Other"






diff_victims_vehs <- 
  view_20[
    which(
      (view_20$Victim_user_type!=view_20$VICTIMS_DP) | 
        (view_20$Responsible.road.user!= view_20$OFFENDING_VEHICLE_DP)
    ),
  ]



write_csv(diff_victims_vehs[,c(2,3,4,5,7)], "data/2020_doubtful_vehicle_type.csv")

# FIRs not there
#-------------------------------------------------------------------------------


missing_firs <- anti_join( DP_crash_level_data, victim_20, by=c("FIR.No.", "PS_Name"))


write_csv(missing_firs[,c(2,3,4,5)], "data/missing_FIRs_2020.csv")


################################################################################
# Changing format of PS names - 2020
################################################################################

victim_20$PS_Name <- str_to_upper(victim_20$PS_Name)

victim_20$PS_Name <- str_replace_all(victim_20$PS_Name, "[[:punct:]]", "")

victim_20$PS_Name <- str_replace_all(victim_20$PS_Name, " ", "")
