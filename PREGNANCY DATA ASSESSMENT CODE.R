##LOAD IN PACKAGES
library(lubridate)
library(tidyverse)
library(dplyr)

##IMPORT DATA
df_encounter_dx <- read_csv("Encounter_dx_Table.csv")
df_encounter <- read_csv("Encounter_Table.csv")
df_location <- read_csv("Location_Table.csv")
df_patient <- read_csv("Patient_Table.csv")
df_pcp <- read_csv("PCP_Table.csv")
df_pregnancy_diagnosis <- read_csv("Pregnancy_Diagnosis.csv")
df_provider <- read_csv("Provider_Table.csv")

##IDENTIFY THE POPULATION OF 17+ FEMALE PATIENTS

##APPOINTMENTS WITH A DUE DATE OR PREGNANCY DIAGNOSIS
dfdx <- df_encounter_dx
dfdx <- dfdx[dfdx$dx_icd %in% df_pregnancy_diagnosis$code == TRUE,] #Get all encounters with a pregnancy diagnosis
dfenc <- df_encounter[is.na(df_encounter$due_date) != TRUE | (df_encounter$enc_id %in% dfdx$dx_enc_id) == TRUE,] #Get all encounters with a due date or had a pregnancy diagnosis

##FIND PATIENTS THAT ARE 17+ AND FEMALE
df <- df_patient
dfenc['over_17'] <- NA #Create a dummy variable for pregnant individuals over 17

##Assign a 1 if the encounter date happened more than 17 years after the patient's birth (Calculated with a year being 365.25 days)
for(i in 1:nrow(dfenc)) { 
  if ((dfenc$enc_date[i] - df$birth_date[df$unique_patient_id == dfenc$enc_patient_id[i]]) >= 365.25*17){
    dfenc$over_17[i] <- 1
  } else {
    if ((dfenc$enc_date[i] - df$birth_date[df$unique_patient_id == dfenc$enc_patient_id[i]]) < 365.25*17) {
      dfenc$over_17[i] <- 0
    }
  }
}

dfenc <- dfenc[dfenc$over_17 == 1,] #Get 17+ patients

dfenc['gender'] <- NA #Create gender dummy variable
for (i in 1:nrow(dfenc)) { #Assign patient's gender
  dfenc$gender[i] <- df$gender[df$unique_patient_id == dfenc$enc_patient_id[i]]
}

dfenc <- dfenc[dfenc$gender == "F",] #Get only female patients


dfprov <- df_provider[df_provider$prov_spec == "OBGYN",] #Get OBGYN appointments  
dfPop <- dfenc[dfenc$visit_prov_id %in% dfprov$prov_id,] #Get only visits with OBGYN


length(unique(dfPop$enc_patient_id)) #1148 patients qualify as being pregnant, over 17, and female




##NUMERATOR

##Patients who have an appointment within 27 and 40 weeks of due date
dfNum <- dfPop[((dfPop$due_date - dfPop$enc_date >= 7*27) & (dfPop$due_date - dfPop$enc_date) <= 40*7 & (is.na(dfPop$due_date) != TRUE)) &
                 dfPop$enc_status != "Cancelled",]
length(unique(dfNum$enc_patient_id))

##Patients who have a completed appointment within 6 weeks of diagnosis
dfNum2 <- dfPop[(is.na(dfPop$due_date) == TRUE),]
dfNum2 <- df_encounter[df_encounter$enc_patient_id %in% dfNum2$enc_patient_id,]
dfNum2 <- dfNum2 %>% arrange(enc_date) %>% arrange(enc_patient_id)
dfNum3 <- c()

for (i in 1:nrow(dfNum2)) {
  if ((dfNum2$enc_id[i] %in% dfdx$dx_enc_id == TRUE & dfNum2$enc_patient_id[i] == dfNum2$enc_patient_id[i+1])) {  
    if (dfNum2$enc_date[i+1] - dfNum2$enc_date[i] <= 6*7) {
      dfNum3 <- rbind(dfNum3, dfNum2[i,])
      dfNum3 <- rbind(dfNum3, dfNum2[i+1,])
    }
  }
}

length(unique(dfNum3$enc_patient_id))
##RATIO OF PATIENTS WHO COMPLETED AN OBGYN APPOINTMENT IN A TIMELY MANNER 
length(unique(c(dfNum$enc_patient_id, dfNum3$enc_patient_id)))/length(unique(dfPop$enc_patient_id))

