##LOAD IN PACKAGES
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)


df <- read_csv("Appointment Data.csv") #Import Dataframe

##CLEAN DATA
df$CHECKIN_DATE <- dmy(df$CHECKIN_DATE)
df$CHECKIN_TIME <- hms::as.hms(df$CHECKIN_TIME)
df$APPOINTMENT_DATE <- dmy(df$APPOINTMENT_DATE)

##CREATE A DUMMY VARIABLE FOR AFTER HOURS APPOINTMENTS
df$AFTER_HOURS_CALL <- ifelse(((df$CHECKIN_TIME >= hms::as.hms("17:30:00")) & (df$SHOW_CODE == "Y") & #Assign a 1 if appointment held after 5:30pm AND was a telephone call AND was actually attended 
                                 is.na(df$CHECKIN_DATE) != TRUE & (df$APPOINTMENT_TYPE == "Telephone Visit") ), 1, 0)

print(nrow(df[df$AFTER_HOURS_CALL == 1,])) #Number of visits held in this time period

df2 <- df %>% group_by(CHECKIN_DATE) %>% summarize(AFTERHOURS_VISITS = sum(AFTER_HOURS_CALL))
df2 <- df2[is.na(df2$CHECKIN_DATE) != TRUE,] #Remove NA date
print(mean(df2$AFTERHOURS_VISITS))
print(max(df2$AFTERHOURS_VISITS))
print(min(df2$AFTERHOURS_VISITS))
print(sd(df2$AFTERHOURS_VISITS))

nrow(df[is.na(df$CHECKIN_TIME) != TRUE,])

##CHECK IF THERE ARE OUTLIERS 
df['AFTER_HOURS_CALL_SCHED'] <- NA
df$AFTER_HOURS_CALL_SCHED <- ifelse(((df$APPOINTMENT_TIME >= hms::as.hms("17:30:00")) &  #Assign a 1 if appointment scheduled after 5:30pm AND was a telephone call. Count even if appointment was NOT actually attended or was held at different time 
                                       (df$APPOINTMENT_TYPE == "Telephone Visit") ), 1, 0)

dfOutlier <- df %>% group_by(APPOINTMENT_DATE) %>% summarize(total_sched = sum(AFTER_HOURS_CALL_SCHED))
dfOutlier['attended'] <- df2$AFTERHOURS_VISITS
dfOutlier['difference'] <- dfOutlier$attended - dfOutlier$total_sched
dfOutlier <- arrange(dfOutlier, difference)

#Get appointment times

df3 <- df %>% group_by(CHECKIN_TIME) %>% summarize(AFTERHOURS_VISITS = sum(AFTER_HOURS_CALL))
df3 <- df3[df3$AFTERHOURS_VISITS != 0,]

print(hms::as.hms(max(df$CHECKIN_TIME[df$AFTER_HOURS_CALL == 1]))) #Latest appointment time
sum(df3$AFTERHOURS_VISITS[df3$CHECKIN_TIME <= hms::as.hms("20:30:00")])/sum(df3$AFTERHOURS_VISITS) #Percent of appointments taking place on or before 8:30pm
sum(df3$AFTERHOURS_VISITS[df3$CHECKIN_TIME <= hms::as.hms("20:00:00")])/sum(df3$AFTERHOURS_VISITS) #Percent of appointmnets taking place on or before 8:00pm

##CHECK IF DAY OF THE WEEK IMPACTS VISITS
df2['DAY_OF_WEEK'] <- NA
df2$DAY_OF_WEEK <- wday(as.Date(df2$CHECKIN_DATE)) #Assigns a day of the week identifier, with 1 = Sunday and 7 = Saturday

for (i in 1:7) {
  print(mean((df2$AFTERHOURS_VISITS[df2$DAY_OF_WEEK == i]))) #Average visits based on day of the week
}  

for (i in 1:7) {
  print(sd((df2$AFTERHOURS_VISITS[df2$DAY_OF_WEEK == i]))) #Standard deviation based on day of the week
}
##It appears as though weekdays and weekends may have different number of visits.
##Test if weekdays and weekend have different average numbers of visits
df2['WEEKDAY'] <- NA
df2$WEEKDAY <- ifelse((df2$DAY_OF_WEEK != 1) & (df2$DAY_OF_WEEK != 7), 1, 0) #Assign a 1 if the day of the week is not Sat/Sun
print(mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]))
print(mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0])) #Average number of visits appears to be near identical

print(sd((df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1])))
print(sd((df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]))) #Weekends appear to have a smaller standard deviation than weekdays

##RUN AN F TEST TO TEST IF STD DEV WEEKDAY == 1 is equal to STD DEV WEEKDAY == 0
var.test(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1], df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0], alternative = "two.sided") 
#With a P value of ~0, we can reject the null hypothesis that STD DEV 1 = STD DEV 0

##GET WEEKDAY VS WEEKEND STATISTICS
print(mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]))
print(max(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]))
print(min(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]))
print(sd(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]))

print(mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]))
print(max(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]))
print(min(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]))
print(sd(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]))

##CALCULATE AVERAGE NUMBER OF DAILY VISITS WITH 95% CONFIDENCE
print(mean(df2$AFTERHOURS_VISITS) + 1.96*sd((df2$AFTERHOURS_VISITS))) #Total
print(mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]) + 1.96*sd((df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]))) #Weekdays
print(mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]) + 1.96*sd((df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]))) #Weekends

##CALCULATE AVERAGE NUMBER OF physician hours WITH 95% CONFIDENCE
print((mean(df2$AFTERHOURS_VISITS) + 1.96*sd((df2$AFTERHOURS_VISITS)))/4) #Total
print((mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1]) + 1.96*sd((df2$AFTERHOURS_VISITS[df2$WEEKDAY == 1])))/4) #Weekdays
print((mean(df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0]) + 1.96*sd((df2$AFTERHOURS_VISITS[df2$WEEKDAY == 0])))/4) #Weekends

write.csv(df2, "df2.csv") #Export daily values to .csv to import to Tableau
write.csv(df3, "df3.csv") #Export time values to .csv to import to Tableau
write.csv(dfOutlier, "dfOutlier.csv") #Export appointment differences data to .csv to import to Tableau

##QUESTION 2
dfQ2 <- df %>% arrange(CHECKIN_DATE) %>% arrange(PATIENT_ID)
dfQ2 <- dfQ2[dfQ2$SHOW_CODE == "Y" & is.na(dfQ2$CHECKIN_DATE) == FALSE & is.na(dfQ2$PATIENT_ID) == FALSE,]

dfQ2['FOLLOWUP_TYPE'] <- NA #Create an identifier for what type of visit the follow up is for
dfQ2['DAYS_BETWEEN_FOLLOWUP'] <- NA #Days between the follow ups
dfQ2['VISIT_TYPE'] <- NA #Type of visit needing a follow up
followUp <- c() #Dataframe for normal hours telephone folow ups
walkinFollowUp <- c() #Dataframe for in person folow ups
afterHoursFU <- c() #Dataframe for after hours telephone folow ups

for (i in 1:nrow(dfQ2)) {
  if (dfQ2$AFTER_HOURS_CALL[i] == 1) { 
    if (dfQ2$APPOINTMENT_TYPE[i] == "Telephone Visit" && dfQ2$PATIENT_ID[i] == dfQ2$PATIENT_ID[i+1] && (dfQ2$CHECKIN_DATE[i+1]-dfQ2$CHECKIN_DATE[i] <= 7)){
      afterHoursFU <- rbind(afterHoursFU, dfQ2[i,])
      afterHoursFU <- rbind(afterHoursFU, dfQ2[i+1,])
      dfQ2$DAYS_BETWEEN_FOLLOWUP[i+1] <- dfQ2$CHECKIN_DATE[i+1]-dfQ2$CHECKIN_DATE[i]
      dfQ2$FOLLOWUP_TYPE[i+1] <- 1 #Follow up type 1 is a visit that's a follow up to an after hours visit
    } 
  }
}
    
for (i in 1:nrow(dfQ2)) {
  if (dfQ2$AFTER_HOURS_CALL[i] == 0) {
    if (dfQ2$APPOINTMENT_TYPE[i] == "Telephone Visit" && dfQ2$PATIENT_ID[i] == dfQ2$PATIENT_ID[i+1] && dfQ2$CHECKIN_DATE[i+1]-dfQ2$CHECKIN_DATE[i] <= 7){
      followUp <- rbind(followUp, dfQ2[i,])
      followUp <- rbind(followUp, dfQ2[i+1,])
      dfQ2$DAYS_BETWEEN_FOLLOWUP[i+1] <- dfQ2$CHECKIN_DATE[i+1]-dfQ2$CHECKIN_DATE[i]
      dfQ2$FOLLOWUP_TYPE[i+1] <- 2 #Follow up type 2 is a visit that's a follow up to an regular hours visit
    }
  }
}

for (i in 1:nrow(dfQ2)) {
  if (dfQ2$APPOINTMENT_TYPE[i] == "In-Person Visit" && dfQ2$PATIENT_ID[i] == dfQ2$PATIENT_ID[i+1] && dfQ2$CHECKIN_DATE[i+1]-dfQ2$CHECKIN_DATE[i] <= 7){
    walkinFollowUp <- rbind(walkinFollowUp, dfQ2[i,])
    walkinFollowUp <- rbind(walkinFollowUp, dfQ2[i+1,])
    dfQ2$DAYS_BETWEEN_FOLLOWUP[i+1] <- dfQ2$CHECKIN_DATE[i+1]-dfQ2$CHECKIN_DATE[i]
    dfQ2$FOLLOWUP_TYPE[i+1] <- 3 #Follow up type 3 is a visit that's a follow up to an in person visit
  }
}

print(paste0("Number of after hours telephone visits needing a follow up: ", nrow(afterHoursFU)/2))
print(paste0("Number of regular telephone visits needing a follow up: ", nrow(followUp)/2))
print(paste0("Number of in-person visits needing a follow up: ", nrow(walkinFollowUp)/2))

print(paste0("Number of total after hours telephone visits: ", nrow(dfQ2[dfQ2$AFTER_HOURS_CALL == 1,])))
print(paste0("Number of total regular telephone visits: ", nrow(dfQ2[dfQ2$APPOINTMENT_TYPE == "Telephone Visit" & dfQ2$AFTER_HOURS_CALL == 0,])))
print(paste0("Number of total in-person visits: ", nrow(dfQ2[dfQ2$APPOINTMENT_TYPE == "In-Person Visit",])))

print(paste0("Efficiency rate of after hours phone visits: ",  (1-(nrow(afterHoursFU)/2)/(nrow(dfQ2[dfQ2$AFTER_HOURS_CALL == 1,])))*100, "%"))
print(paste0("Efficiency rate of regular telephone visits: ",  (1-(nrow(followUp)/2)/(nrow(dfQ2[dfQ2$APPOINTMENT_TYPE == "Telephone Visit" & dfQ2$AFTER_HOURS_CALL == 0,])))*100, "%"))
print(paste0("Efficiency rate of in-person visits: ",  (1-(nrow(walkinFollowUp)/2)/(nrow(dfQ2[dfQ2$APPOINTMENT_TYPE == "In-Person Visit",])))*100, "%"))

write.csv(dfQ2, "dfQ2.csv")