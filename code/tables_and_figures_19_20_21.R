
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