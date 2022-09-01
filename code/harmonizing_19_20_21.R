

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
