##Group Project Code
library(quantmod)
library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)

#Load in the datasets 
coviddata <- fread("national-history.csv", stringsAsFactors = FALSE)
consumersenti <- fread("UMCSENT.csv", stringsAsFactors = FALSE)
lightvehicles <- fread("ALTSALES.csv", stringsAsFactors = FALSE)
sentimentdata <- fread("scaum-208.csv", stringsAsFactors = FALSE)
disposableinc <- fread("DSPIC96.csv", stringsAsFactors = FALSE)


#Clean the dates where necessary
sentimentdata$yyyymm <- paste0(sentimentdata$yyyymm, "01")
sentimentdata$DATE <- as.Date(sentimentdata$yyyymm, "%Y%m%d")
consumersenti$DATE <- as.Date(consumersenti$DATE, "%Y-%m-%d")
coviddata$date <- format(as.Date(coviddata$date, format = "%m/%d/%Y", "%Y/%m/%d"))
lightvehicles$DATE <- as.Date(lightvehicles$DATE, "%Y-%m-%d")
disposableinc$DATE <- as.Date(disposableinc$DATE, "%Y-%m-%d")

#Arrange Covid Statistics oldest to newest
coviddata <- coviddata[order(as.Date(coviddata$date, format ="%Y-%m-%d")),]

#Trim the dates to between Jan 2008 and Feb 2021
lightvehicles <- lightvehicles[lightvehicles$DATE >= "2008-01-01" & lightvehicles$DATE <= "2021-02-01",]
disposableinc <- disposableinc[disposableinc$DATE >= "2008-01-01" & disposableinc$DATE <= "2021-02-01",]
sentimentdata <- sentimentdata[sentimentdata$DATE >= "2008-01-01" & sentimentdata$DATE <= "2021-02-01",]
consumersenti <- consumersenti[consumersenti$DATE >= "2008-01-01" & consumersenti$DATE <= "2021-02-01",]

#Unified master dataset
unifiedDF <- sentimentdata
unifiedDF <- left_join(unifiedDF, lightvehicles, by = "DATE")
unifiedDF <- left_join(unifiedDF, disposableinc, by = "DATE")
unifiedDF <- left_join(unifiedDF, consumersenti, by = "DATE")

colnames(unifiedDF)[which(names(unifiedDF) == "DSPIC96")] <- "disposable_Inc"
colnames(unifiedDF)[which(names(unifiedDF) == "ALTSALES")] <- "lightVehicles"
colnames(unifiedDF)[which(names(unifiedDF) == "UMCSENT")] <- "consumer_Sentiment"
unifiedDF$date <- unifiedDF$DATE  
###

#Trim the Covid dataset set to between Feb 2020 and Feb 2021
coviddata <- coviddata[coviddata$date >= "2020-02-01" & coviddata$date <= "2021-02-28",]

coviddataSubset <- coviddata[coviddata$date == "2021-02-01" |  #Subset the data based on data at the start of the month
                               coviddata$date == "2021-01-01" |
                               coviddata$date == "2020-12-01" |
                               coviddata$date == "2020-11-01" |
                               coviddata$date == "2020-10-01" |
                               coviddata$date == "2020-09-01" |
                               coviddata$date == "2020-08-01" |
                               coviddata$date == "2020-07-01" |
                               coviddata$date == "2020-06-01" |
                               coviddata$date == "2020-05-01" |
                               coviddata$date == "2020-04-01" |
                               coviddata$date == "2020-03-01" |
                               coviddata$date == "2020-02-01" ]

coviddataSubset$positiveIncrease <- c(2, tail(coviddataSubset$positive, -1) - head(coviddataSubset$positive, -1))
coviddataSubset[is.na(coviddataSubset)] = 0 #Change NAs to 0s


#Covid era specific dataset

#Trim the dates to between Feb 2020 and Feb 2021
covidDF <- unifiedDF
covidDF <- covidDF[covidDF$date >= "2020-02-01" & covidDF$date <= "2021-02-01",]
coviddataSubset$date <- as.Date(coviddataSubset$date)
covidDF <- merge(covidDF,coviddataSubset, by="date")

#Finished Datasets
head(unifiedDF[,1:5]) #All non-covid related data from Jan 2008 - Feb 2021
head(covidDF[,1:5]) #All data beteween Feb 2008 and Feb 2021

#Export Data
write.csv(unifiedDF, file="Cleaned Data.csv", row.names = FALSE)
write.csv(covidDF, "Cleaned Covid Data.csv", row.names = FALSE)

#PLOT DISPOSABLE INCOME VS LIGHT VEHICLE SALES

plot(unifiedDF$date, unifiedDF$disposable_Inc)
plot(unifiedDF$date, unifiedDF$lightVehicles)

#Check Linear regression
cor(unifiedDF$disposable_Inc, unifiedDF$lightVehicles)
incomeandvehicles <- lm(disposable_Inc ~ lightVehicles, data = unifiedDF)
summary(incomeandvehicles)

library(ggplot2)
ggplot(unifiedDF,aes(disposable_Inc, lightVehicles)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Disposable Income', y='Light Vehicle Sales', title='Linear Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

#Check Quadratic regression
unifiedDF$lightVehicles2 <- unifiedDF$lightVehicles^2
incomeandvehicles2 <- lm(disposable_Inc ~ lightVehicles2, data = unifiedDF)
summary(incomeandvehicles2)

ggplot(unifiedDF, aes(x=disposable_Inc, y=lightVehicles2)) + geom_point()+stat_smooth(se=F) +  labs(x='Disposable Income', y='Light Vehicle Sales^2', title='Quadratic Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

#PLOT DISPOSABLE INCOME VS LIGHT VEHICLE SALES DURING COVID

plot(covidDF$date, covidDF$disposable_Inc)
plot(covidDF$date, covidDF$lightVehicles)

#Check Linear regression
cor(covidDF$disposable_Inc, covidDF$lightVehicles)
covidincomeandvehicles <- lm(disposable_Inc ~ lightVehicles, data = covidDF)
summary(covidincomeandvehicles)

ggplot(covidDF,aes(disposable_Inc, lightVehicles)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Disposable Income during COVID', y='Light Vehicle Sales during COVID', title='Linear Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 



# PLOT DISPOSABLE INCOME VS. CONSUMER SENTIMENT
plot(unifiedDF$date, unifiedDF$disposable_Inc)
plot(unifiedDF$date, unifiedDF$consumer_Sentiment)

cor(unifiedDF$disposable_Inc, unifiedDF$consumer_Sentiment)

unifiedDFcsvsdi <- lm(disposable_Inc ~ consumer_Sentiment, data= unifiedDF)
summary(unifiedDFcsvsdi)

ggplot(unifiedDF, aes(disposable_Inc, consumer_Sentiment))+
  geom_point()+
  geom_smooth(method='lm', SE=FALSE, color='blue')+
  theme_minimal()+
  labs(x="Disposable Income", y="Consumer Sentiment", title= "Linear Regression: Disposable Income vs. Consumer Sentiment")

#QUADRATIC
unifiedDF$consumer_Sentiment2 <- unifiedDF$consumer_Sentiment^2
unifiedDFcsvsdi2 <- lm(disposable_Inc ~ consumer_Sentiment2, data = unifiedDF)
summary(unifiedDFcsvsdi2)

ggplot(unifiedDF, aes(x=disposable_Inc, y=consumer_Sentiment2)) + geom_point()+stat_smooth(se=F) +  labs(x='Disposable Income', y='Light Vehicle Sales^2', title='Quadratic Regression Plot: Disposable Income vs. Consumer Sentiment')


#PLOT DISPOSABLE INCOME VS. CONSUMER SENTIMENT DURING COVID
plot(covidDF$date, covidDF$disposable_Inc)
plot(covidDF$date, covidDF$consumer_Sentiment)

cor(covidDF$disposable_Inc, covidDF$consumer_Sentiment)

covidDFcsvsdi <- lm(disposable_Inc ~ consumer_Sentiment, data= covidDF)
summary(covidDFcsvsdi)


ggplot(covidDF, aes(disposable_Inc, consumer_Sentiment))+
  geom_point()+
  geom_smooth(method='lm', SE=FALSE, color='blue')+
  theme_minimal()+
  labs(x="Disposable Income", y="Consumer Sentiment", title= "Linear Regression: Disposable Income vs. Consumer Sentiment, COVID")


x <- ggplot(unifiedDF, aes(DATE, consumer_Sentiment)) + geom_area(color = "#861f41", size =1, fill = "#861f41") + coord_cartesian(ylim = c(40,120)) +
  annotate("rect", xmin = as.Date("2008-01-01"), xmax = as.Date("2009-06-01"), ymin = 0, ymax = Inf, alpha = .3, fill = "dark blue") + 
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2021-02-01"), ymin = 0, ymax = Inf, alpha = .3, fill = "dark blue") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") + xlab("Date") +ylab("Consumer Sentiment") +
  annotate("text", x = as.Date("2010-03-01"), y = 84, label = "Great Recession", cex = 5) + 
  annotate("text", x = as.Date("2019-03-01"), y = 105, label = "Covid-19 Pandemic", cex = 5)

print(x)
print(rgb(134,31,65))

covidDF1 <- covidDF[covidDF$date != as.Date("2020-04-01")]



ggplot(covidDF, aes(disposable_Inc, lightVehicles))+
  geom_point()+
  geom_smooth(method='lm', SE=FALSE, color='blue') +
  theme_minimal()+
  labs(x="Disposable Income (Billions of $)", y="Light Vehicle Sales", title= "Disposable Income vs. Light Vehicle Sales")

ggplot(sansCovidDF, aes(disposable_Inc, lightVehicles))+
  geom_point()+
  geom_smooth(method='lm', SE=FALSE, color='blue') +
  theme_minimal()+
  labs(x="Disposable Income (Billions of $)", y="Light Vehicle Sales", title= "Disposable Income vs. Light Vehicle Sales")


x <- lm(covidDF1$lightVehicles ~ covidDF1$disposable_Inc)
summary(lm(x))

sansCovidDF <- unifiedDF[unifiedDF$date <= as.Date("2020-01-01")]


ggplot(covidDF, aes(disposable_Inc, consumer_Sentiment))+
  geom_point()+
  geom_smooth(method='lm', SE=FALSE, color='blue')+
  theme_minimal()+
  labs(x="Disposable Income (Billions of $)", y="Consumer Sentiment", title= "Disposable Income vs. Consumer Sentiment")

ggplot(sansCovidDF, aes(disposable_Inc, consumer_Sentiment))+
  geom_point()+
  geom_smooth(method='lm', SE=FALSE, color='blue')+
  theme_minimal()+
  labs(x="Disposable Income (Billions of $)", y="Consumer Sentiment", title= "Disposable Income vs. Consumer Sentiment")

