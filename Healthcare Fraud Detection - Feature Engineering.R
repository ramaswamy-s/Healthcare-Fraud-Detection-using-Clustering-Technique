install.packages('sqldf')
install.packages("zipcode")
install.packages('maps')
install.packages('ggthemes')
install.packages('plotly')
install.packages('ggmap')
install.packages('rgeos')
install.packages('sp')
install.packages('maptools')
library(sqldf)
library(dplyr)
library(plotly)
library(ggplot2)
library(data.table)
library(zipcode)
library(tidyverse)
library(stringr)
library(ggthemes)
library(maps)
library(rgeos)
library(sp)
library(maptools)


setwd('C:/myfiles/Anomaly/assignment4')
payment=read.csv('inpatientCharges.csv')

str(payment)
head(payment)
tail(payment)

#Checking for NA in dataset
indx <- apply(payment, 2, function(x) any(is.na(x)))
indx

# Convert the Charges to numeric
p1 <- strsplit(x = as.character(payment$Average.Covered.Charges),split = "$",fixed = T)
payment$Average.Covered.Charges <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Total.Payments),split = "$",fixed = T)
payment$Average.Total.Payments <- as.numeric(sapply(p1,"[[",2))
p1 <- strsplit(x = as.character(payment$Average.Medicare.Payments),split = "$",fixed = T)
payment$Average.Medicare.Payments <- as.numeric(sapply(p1,"[[",2))
rm("p1")

colnames(payment)

#**********************EXPLORATORY DATA ANALYSIS*******************************

payment <- as.data.table(payment)

av1 <- payment[,Average.Covered.Charges,by=Provider.State]
ggplot(data = av1,mapping = aes(y = Provider.State,x = Average.Covered.Charges,colour=Average.Covered.Charges))+
  geom_point()

av1 <- payment[,Average.Total.Payments,by=Provider.State]
ggplot(data = av1,mapping = aes(y = Provider.State,x = Average.Total.Payments,colour=Average.Total.Payments))+
  geom_point()

#Create variables for Latitude & Longitude

payment$Provider.Zip.Code<-as.factor(payment$Provider.Zip.Code)
data(zipcode)
payment=merge(payment,zipcode,by.x='Provider.Zip.Code',by.y='zip')

#*****************************FEATURE ENGINEERING*****************************

#In order to aggregate Inpatient charges across different combinations of DRG with State, Zipcode and Hospital Referral Region (HRR)
#we create unique IDs for each of the combinations as follows.

# create unique ID based on DRG and State combination
payment <- mutate(payment, DRGbyState = group_indices(payment, DRG.Definition,Provider.State))

# create unique ID based on DRG and Zipcode combination
payment <- mutate(payment, DRGbyZip = group_indices(payment, DRG.Definition,Provider.Zip.Code))

# create unique ID based on DRG and Hospital Referral Region combination
payment <- mutate(payment, DRGbyHRR = group_indices(payment, DRG.Definition,Hospital.Referral.Region.Description))


#Feature 1: create variable for difference between 'Provider's charges for medicare-covered services' and 'Amount that Medicare paid to the provider'
payment$ChargePayDiff <- payment$Average.Covered.Charges - payment$Average.Medicare.Payments


#Feature 2: Mean difference between Provider's Average Covered Charges and Mean Medicare Payments for every unique DRG-State combination

MeanChargePayDiff_State <- sqldf('select DRGbyState, avg(ChargePayDiff) as MeanChargePayDiff_State from payment group by DRGbyState')
nrow(MeanChargePayDiff_State)
payment <- merge(payment, MeanChargePayDiff_State)
payment$ChargePayDiff_State <- payment$ChargePayDiff - payment$MeanChargePayDiff_State 
#The above feature contains both +ve & _ve values; we're interested only in +ve values higher than a specific set limit, say top 30%
#The following plot shows some outlying points which are DRG-State combos where the average amount Providers charge are usually 
#way higher than what the Medicare finally pays to provider for the DRG on an average in the respective States

payment <- as.data.table(payment)
df_outlier <- payment[payment$ChargePayDiff_State > 0.7*(max(payment$ChargePayDiff_State) - min(payment$ChargePayDiff_State[payment$ChargePayDiff_State> 0])),]
av1 <- payment[,ChargePayDiff_State,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = ChargePayDiff_State,colour=ChargePayDiff_State))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=ChargePayDiff_State,y=DRGbyState), color='red',size=1.5)

#Feature 3: Mean difference between Provider's Average Covered Charges and Mean Medicare Payments for every unique DRG-Zipcode combination

MeanChargePayDiff_Zip <- sqldf('select DRGbyZip, avg(ChargePayDiff) as MeanChargePayDiff_Zip from payment group by DRGbyZip')
payment <- merge(payment, MeanChargePayDiff_Zip)
payment$ChargePayDiff_Zip <- payment$ChargePayDiff - payment$MeanChargePayDiff_Zip
#The above feature contains both +ve & _ve values; we're interested only in +ve values higher than a specific set limit, say top 30%
#The following plot shows some outlying points which are DRG-Zipcode combos where the average amount Providers charge are usually 
#way higher than what the Medicare finally pays to provider for the DRG on an average in the respective Zipcodes
payment <- as.data.table(payment)
df_outlier <- payment[payment$ChargePayDiff_Zip > 0.7*(max(payment$ChargePayDiff_Zip) - min(payment$ChargePayDiff_Zip[payment$ChargePayDiff_Zip> 0])),]
av1 <- payment[,ChargePayDiff_Zip,by=DRGbyZip]
ggplot(data = av1,mapping = aes(y = DRGbyZip,x = ChargePayDiff_Zip,colour=ChargePayDiff_Zip))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=ChargePayDiff_Zip,y=DRGbyZip), color='red',size=1.5)

#Feature 4: Mean difference between Provider's Average Covered Charges and Mean Medicare Payments for every unique DRG-HRR combination

MeanChargePayDiff_HRR <- sqldf('select DRGbyHRR, avg(ChargePayDiff) as MeanChargePayDiff_HRR from payment group by DRGbyHRR')
payment <- as.data.frame(payment)
payment <- merge(payment,MeanChargePayDiff_HRR)
payment$ChargePayDiff_HRR <- payment$ChargePayDiff - payment$MeanChargePayDiff_HRR
#The above feature contains both +ve & _ve values; we're interested only in +ve values higher than a specific set limit, say top 30%
#The following plot shows some outlying points which are DRG-Hospital Referral Region combos where the average amount Providers charge are usually 
#way higher than what the Medicare finally pays to provider for the DRG on an average in the respective HRRs
payment <- as.data.table(payment)
df_outlier <- payment[payment$ChargePayDiff_HRR > 0.7*(max(payment$ChargePayDiff_HRR) - min(payment$ChargePayDiff_HRR[payment$ChargePayDiff_HRR> 0])),]
av1 <- payment[,ChargePayDiff_HRR,by=DRGbyHRR]
ggplot(data = av1,mapping = aes(y = DRGbyHRR,x = ChargePayDiff_HRR,colour=ChargePayDiff_HRR))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=ChargePayDiff_HRR,y=DRGbyHRR), color='red',size=1.5)


#Feature 5: Difference between Total payment to the provider for a DRG and the mean of the Total payment to the Provider grouped by DRG-State combination
payment <- as.data.frame(payment)
MeanTotalPmt_State <- sqldf('select DRGbyState, avg("Average.Total.Payments") as MeanTotalPmt_State from payment group by DRGbyState')
payment <- merge(payment, MeanTotalPmt_State)
payment$TotalPmtDiff_State <- payment$Average.Total.Payments - payment$MeanTotalPmt_State 
#The above feature contains both +ve & _ve values; we're interested only in +ve values higher than a specific set limit, say top 50%
#The following plot shows some outlying points which are DRG-State combos where the Total payments to the provider are  
#way higher than the mean of the Total payment to the Provider grouped by DRG-State combination
payment <- as.data.table(payment)
df_outlier <- payment[payment$TotalPmtDiff_State > 0.5*(max(payment$TotalPmtDiff_State) - min(payment$TotalPmtDiff_State[payment$TotalPmtDiff_State> 0])),]
av1 <- payment[,TotalPmtDiff_State,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = TotalPmtDiff_State,colour=TotalPmtDiff_State))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=TotalPmtDiff_State,y=DRGbyState), color='red',size=1.5)

#Feature 6: Difference between Total payment to the provider for a DRG and the mean of the Total payment to the Provider grouped by DRG-Zipcode combination
payment <- as.data.frame(payment)
MeanTotalPmt_Zip <- sqldf('select DRGbyZip, avg("Average.Total.Payments") as MeanTotalPmt_Zip from payment group by DRGbyZip')
payment <- merge(payment, MeanTotalPmt_Zip)
payment$TotalPmtDiff_Zip <- payment$Average.Total.Payments - payment$MeanTotalPmt_Zip 
#The above feature contains both +ve & _ve values; we're interested only in +ve values higher than a specific set limit, say top 50%
#The following plot shows some outlying points which are DRG-Zip combos where the Total payments to the provider are  
#way higher than the mean of the Total payment to the Provider grouped by DRG-Zip combination
payment <- as.data.table(payment)
df_outlier <- payment[payment$TotalPmtDiff_Zip > 0.5*(max(payment$TotalPmtDiff_Zip) - min(payment$TotalPmtDiff_Zip[payment$TotalPmtDiff_Zip> 0])),]
av1 <- payment[,TotalPmtDiff_Zip,by=DRGbyZip]
ggplot(data = av1,mapping = aes(y = DRGbyZip,x = TotalPmtDiff_Zip,colour=TotalPmtDiff_Zip))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=TotalPmtDiff_Zip,y=DRGbyZip), color='red',size=1.5)

#Feature 7: Difference between Total payment to the provider for a DRG and the mean of the Total payment to the Provider grouped by DRG-Hospital Referral Region combination
payment <- as.data.frame(payment)
MeanTotalPmt_HRR <- sqldf('select DRGbyHRR, avg("Average.Total.Payments") as MeanTotalPmt_HRR from payment group by DRGbyHRR')
payment <- merge(payment, MeanTotalPmt_HRR)
payment$TotalPmtDiff_HRR <- payment$Average.Total.Payments - payment$MeanTotalPmt_HRR 
#The above feature contains both +ve & _ve values; we're interested only in +ve values higher than a specific set limit
#The following plot shows some outlying points which are DRG-HRR combos where the Total payments to the provider are  
#way higher than the mean of the Total payment to the Provider grouped by DRG-HRR combination, say top 50%
payment <- as.data.table(payment)
df_outlier <- payment[payment$TotalPmtDiff_HRR > 0.5*(max(payment$TotalPmtDiff_HRR) - min(payment$TotalPmtDiff_HRR[payment$TotalPmtDiff_HRR> 0])),]
av1 <- payment[,TotalPmtDiff_HRR,by=DRGbyHRR]
ggplot(data = av1,mapping = aes(y = DRGbyHRR,x = TotalPmtDiff_HRR,colour=TotalPmtDiff_HRR))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=TotalPmtDiff_HRR,y=DRGbyHRR), color='red',size=1.5)

#Feature 8: Ratio of total discharges for a DRG at a Provider level over total discharges for a DRG at a state level
payment <- as.data.frame(payment)
TotalDschrg_State <- sqldf('select DRGbyState, sum("Total.Discharges") as TotalDschrg_State from payment group by DRGbyState')
payment <- merge(payment, TotalDschrg_State)
payment$DschrgRatio_State <- payment$Total.Discharges/payment$TotalDschrg_State 

#The following plot shows some outlying points representing Ratio of total discharges for a DRG at a Provider level over 
#total discharges for a DRG at a state level;  we're interested in values higher than a specific set limit, say top 25% with high ratios
#P.S.: The entries with ratio 1 in the plot are single entries at a DRG-State combination level. So, they are ignored.

payment <- as.data.table(payment)
df_outlier <- payment[(payment$DschrgRatio_State > 0.75*(max(payment$DschrgRatio_State) - min(payment$DschrgRatio_State[payment$DschrgRatio_State> 0]))) & (payment$DschrgRatio_State < 1),]
av1 <- payment[,DschrgRatio_State,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = DschrgRatio_State,colour=DschrgRatio_State))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=DschrgRatio_State,y=DRGbyState), color='red',size=1.5)

#Feature 9: Ratio of total discharges for a DRG at a Provider level over total discharges for a DRG at a Zipcode level
payment <- as.data.frame(payment)
TotalDschrg_Zip <- sqldf('select DRGbyZip, sum("Total.Discharges") as TotalDschrg_Zip from payment group by DRGbyZip')
payment <- merge(payment, TotalDschrg_Zip)
payment$DschrgRatio_Zip <- payment$Total.Discharges/payment$TotalDschrg_Zip 

#The following plot shows some outlying points representing Ratio of total discharges for a DRG at a Provider level over 
#total discharges for a DRG at a Zipcode level;  we're interested in values higher than a specific set limit, say top 10% with really high ratios
#P.S.: The entries with ratio 1 in the plot are single entries at a DRG-Zip combination level. So, they are ignored.

payment <- as.data.table(payment)
df_outlier <- payment[(payment$DschrgRatio_Zip > 0.90*(max(payment$DschrgRatio_Zip) - min(payment$DschrgRatio_Zip[payment$DschrgRatio_Zip> 0]))) & (payment$DschrgRatio_Zip < 1),]
av1 <- payment[,DschrgRatio_Zip,by=DRGbyZip]
ggplot(data = av1,mapping = aes(y = DRGbyZip,x = DschrgRatio_Zip,colour=DschrgRatio_Zip))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=DschrgRatio_Zip,y=DRGbyZip), color='red',size=1.5)

#Feature 10: Ratio of total discharges for a DRG at a Provider level over total discharges for a DRG at a HRR level
payment <- as.data.frame(payment)
TotalDschrg_HRR <- sqldf('select DRGbyHRR, sum("Total.Discharges") as TotalDschrg_HRR from payment group by DRGbyHRR')
payment <- merge(payment, TotalDschrg_HRR)
payment$DschrgRatio_HRR <- payment$Total.Discharges/payment$TotalDschrg_HRR 

#The following plot shows some outlying points representing Ratio of total discharges for a DRG at a Provider level over 
#total discharges for a DRG at a HRR level;  we're interested in values higher than a specific set limit, say top 10% with really high ratios
#P.S.: The entries with ratio 1 in the plot are single entries at a DRG-HRR combination level. So, they are ignored.

payment <- as.data.table(payment)
df_outlier <- payment[(payment$DschrgRatio_HRR > 0.90*(max(payment$DschrgRatio_HRR) - min(payment$DschrgRatio_HRR[payment$DschrgRatio_HRR> 0]))) & (payment$DschrgRatio_HRR < 1),]
av1 <- payment[,DschrgRatio_HRR,by=DRGbyHRR]
ggplot(data = av1,mapping = aes(y = DRGbyHRR,x = DschrgRatio_HRR,colour=DschrgRatio_HRR))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=DschrgRatio_HRR,y=DRGbyHRR), color='red',size=1.5)

#Feature 11: Standard deviation between Total payment to the provider for a DRG and the mean of the Total payment to the Provider grouped by DRG-State combination
payment <- as.data.frame(payment)
SDTotalPmt_State <- sqldf('select DRGbyState, stdev("Average.Total.Payments") as SDTotalPmt_State from payment group by DRGbyState')
payment <- merge(payment, SDTotalPmt_State)
payment$SDTotalPmt_State[is.na(payment$SDTotalPmt_State)] <- 0 #replacing NAs for single entries with 0s for SD
payment$SDTotalPmtDiff_State <- (payment$TotalPmtDiff_State/payment$SDTotalPmt_State)
#Replacing all NAs for single entries and negative values to 0s for SD as we are interested only in amount higher, not lesser than the mean
payment$SDTotalPmtDiff_State[is.na(payment$SDTotalPmtDiff_State)] <- 0
payment$SDTotalPmtDiff_State[payment$SDTotalPmtDiff_State < 0] <- 0
#The following plot shows some outlying points which are DRG-State combos where the Standard Deviation of the Total payments to the provider are  
#way higher than the mean of the Total payment to the Provider grouped by DRG-State combination.
#we're interested in values higher than a specific set limit, say top 30% with really high deviations
payment <- as.data.table(payment)
df_outlier <- payment[payment$SDTotalPmtDiff_State > 0.70*(max(payment$SDTotalPmtDiff_State) - min(payment$SDTotalPmtDiff_State[payment$SDTotalPmtDiff_State> 0])),]
av1 <- payment[,SDTotalPmtDiff_State,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = SDTotalPmtDiff_State,colour=SDTotalPmtDiff_State))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=SDTotalPmtDiff_State,y=DRGbyState), color='red',size=1.5)

#Feature 12: Standard deviation between Total payment to the provider for a DRG and the mean of the Total payment to the Provider grouped by DRG-Zipcode combination
payment <- as.data.frame(payment)
SDTotalPmt_Zip <- sqldf('select DRGbyZip, stdev("Average.Total.Payments") as SDTotalPmt_Zip from payment group by DRGbyZip')
payment <- merge(payment, SDTotalPmt_Zip)
payment$SDTotalPmt_Zip[is.na(payment$SDTotalPmt_Zip)] <- 0 #replacing NAs for single entries with 0s for SD
payment$SDTotalPmtDiff_Zip <- (payment$TotalPmtDiff_Zip/payment$SDTotalPmt_Zip)
#Replacing all NAs for single entries and negative values to 0s for SD as we are interested only in amount higher, not lesser than the mean
payment$SDTotalPmtDiff_Zip[is.na(payment$SDTotalPmtDiff_Zip)] <- 0
payment$SDTotalPmtDiff_Zip[payment$SDTotalPmtDiff_Zip < 0] <- 0
#The following plot shows some outlying points which are DRG-Zip combos where the Standard Deviation of the Total payments to the provider are  
#way higher than the mean of the Total payment to the Provider grouped by DRG-Zipcode combination.
#we're interested in values higher than a specific set limit, say top 30% with really high deviations
payment <- as.data.table(payment)
df_outlier <- payment[payment$SDTotalPmtDiff_Zip > 0.70*(max(payment$SDTotalPmtDiff_Zip) - min(payment$SDTotalPmtDiff_Zip[payment$SDTotalPmtDiff_Zip> 0])),]
av1 <- payment[,SDTotalPmtDiff_Zip,by=DRGbyZip]
ggplot(data = av1,mapping = aes(y = DRGbyZip,x = SDTotalPmtDiff_Zip,colour=SDTotalPmtDiff_Zip))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=SDTotalPmtDiff_Zip,y=DRGbyZip), color='red',size=1.5)

#Feature 13: Standard deviation between Total payment to the provider for a DRG and the mean of the Total payment to the Provider grouped by DRG-HRR combination
payment <- as.data.frame(payment)
SDTotalPmt_HRR <- sqldf('select DRGbyHRR, stdev("Average.Total.Payments") as SDTotalPmt_HRR from payment group by DRGbyHRR')
payment <- merge(payment, SDTotalPmt_HRR)
payment$SDTotalPmt_HRR[is.na(payment$SDTotalPmt_HRR)] <- 0 #replacing NAs for single entries with 0s for SD
payment$SDTotalPmtDiff_HRR <- (payment$TotalPmtDiff_HRR/payment$SDTotalPmt_HRR)
#Replacing all NAs for single entries and negative values to 0s for SD as we are interested only in amount higher, not lesser than the mean
payment$SDTotalPmtDiff_HRR[is.na(payment$SDTotalPmtDiff_HRR)] <- 0
payment$SDTotalPmtDiff_HRR[payment$SDTotalPmtDiff_HRR < 0] <- 0
#The following plot shows some outlying points which are DRG-HRR combos where the Standard Deviation of the Total payments to the provider are  
#way higher than the mean of the Total payment to the Provider grouped by DRG-HRR combination.
#we're interested in values higher than a specific set limit, say top 30% with really high deviations
payment <- as.data.table(payment)
df_outlier <- payment[payment$SDTotalPmtDiff_HRR > 0.70*(max(payment$SDTotalPmtDiff_HRR) - min(payment$SDTotalPmtDiff_HRR[payment$SDTotalPmtDiff_HRR> 0])),]
av1 <- payment[,SDTotalPmtDiff_HRR,by=DRGbyHRR]
ggplot(data = av1,mapping = aes(y = DRGbyHRR,x = SDTotalPmtDiff_HRR,colour=SDTotalPmtDiff_HRR))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=SDTotalPmtDiff_HRR,y=DRGbyHRR), color='red',size=1.5)

#Feature 14: Standard deviation between Average Covered Charges for a DRG and Mean Medicare Payments for every unique DRG-State combination
payment <- as.data.frame(payment)

MeanTotalChrg_State <- sqldf('select DRGbyState, avg("Average.Covered.Charges") as MeanTotalChrg_State from payment group by DRGbyState')
payment <- merge(payment, MeanTotalChrg_State)
payment$TotalChrgDiff_State <- payment$Average.Covered.Charges - payment$MeanTotalChrg_State 

SDTotalChrg_State <- sqldf('select DRGbyState, stdev("Average.Covered.Charges") as SDTotalChrg_State from payment group by DRGbyState')
payment <- merge(payment, SDTotalChrg_State)
payment$SDTotalChrg_State[is.na(payment$SDTotalChrg_State)] <- 0 #replacing NAs for single entries with 0s for SD
payment$SDTotalChrgDiff_State <- (payment$TotalChrgDiff_State/payment$SDTotalChrg_State)
#Replacing all NAs for single entries and negative values to 0s for SD as we are interested only in amount higher, not lesser than the mean
payment$SDTotalChrgDiff_State[is.na(payment$SDTotalChrgDiff_State)] <- 0
payment$SDTotalChrgDiff_State[payment$SDTotalChrgDiff_State < 0] <- 0
#The following plot shows some outlying points which are DRG-State combos where the Standard Deviation of the Provider's charges for a DRG are  
#way higher than the mean of the difference between Average Covered Charges and Mean Medicare payments grouped by DRG-State combination.
#we're interested in values higher than a specific set limit, say top 30% with really high deviations
payment <- as.data.table(payment)
df_outlier <- payment[payment$SDTotalChrgDiff_State > 0.70*(max(payment$SDTotalChrgDiff_State) - min(payment$SDTotalChrgDiff_State[payment$SDTotalChrgDiff_State> 0])),]
av1 <- payment[,SDTotalChrgDiff_State,by=DRGbyState]
ggplot(data = av1,mapping = aes(y = DRGbyState,x = SDTotalChrgDiff_State,colour=SDTotalChrgDiff_State))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=SDTotalChrgDiff_State,y=DRGbyState), color='red',size=1.5)  

#Feature 15: Standard deviation between Average Covered Charges for a DRG and Mean Medicare Payments for every unique DRG-Zip combination
payment <- as.data.frame(payment)

MeanTotalChrg_Zip <- sqldf('select DRGbyZip, avg("Average.Covered.Charges") as MeanTotalChrg_Zip from payment group by DRGbyZip')
payment <- merge(payment, MeanTotalChrg_Zip)
payment$TotalChrgDiff_Zip <- payment$Average.Covered.Charges - payment$MeanTotalChrg_Zip 

SDTotalChrg_Zip <- sqldf('select DRGbyZip, stdev("Average.Covered.Charges") as SDTotalChrg_Zip from payment group by DRGbyZip')
payment <- merge(payment, SDTotalChrg_Zip)
payment$SDTotalChrg_Zip[is.na(payment$SDTotalChrg_Zip)] <- 0 #replacing NAs for single entries with 0s for SD
payment$SDTotalChrgDiff_Zip <- (payment$TotalChrgDiff_Zip/payment$SDTotalChrg_Zip)
#Replacing all NAs for single entries and negative values to 0s for SD as we are interested only in amount higher, not lesser than the mean
payment$SDTotalChrgDiff_Zip[is.na(payment$SDTotalChrgDiff_Zip)] <- 0
payment$SDTotalChrgDiff_Zip[payment$SDTotalChrgDiff_Zip < 0] <- 0
#The following plot shows some outlying points which are DRG-Zip combos where the Standard Deviation of the Provider's charges for a DRG are  
#way higher than the mean of the difference between Average Covered Charges and Mean Medicare payments grouped by DRG-Zip combination.
#we're interested in values higher than a specific set limit, say top 30% with really high deviations
payment <- as.data.table(payment)
df_outlier <- payment[payment$SDTotalChrgDiff_Zip > 0.70*(max(payment$SDTotalChrgDiff_Zip) - min(payment$SDTotalChrgDiff_Zip[payment$SDTotalChrgDiff_Zip> 0])),]
av1 <- payment[,SDTotalChrgDiff_Zip,by=DRGbyZip]
ggplot(data = av1,mapping = aes(y = DRGbyZip,x = SDTotalChrgDiff_Zip,colour=SDTotalChrgDiff_Zip))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=SDTotalChrgDiff_Zip,y=DRGbyZip), color='red',size=1.5)  


#Feature 16: Standard deviation between Average Covered Charges for a DRG and Mean Medicare Payments for every unique DRG-HRR combination
payment <- as.data.frame(payment)
MeanTotalChrg_HRR <- sqldf('select DRGbyHRR, avg("Average.Covered.Charges") as MeanTotalChrg_HRR from payment group by DRGbyHRR')
payment <- merge(payment, MeanTotalChrg_HRR)
payment$TotalChrgDiff_HRR <- payment$Average.Covered.Charges - payment$MeanTotalChrg_HRR 

SDTotalChrg_HRR <- sqldf('select DRGbyHRR, stdev("Average.Covered.Charges") as SDTotalChrg_HRR from payment group by DRGbyHRR')
payment <- merge(payment, SDTotalChrg_HRR)
payment$SDTotalChrg_HRR[is.na(payment$SDTotalChrg_HRR)] <- 0 #replacing NAs for single entries with 0s for SD
payment$SDTotalChrgDiff_HRR <- (payment$TotalChrgDiff_HRR/payment$SDTotalChrg_HRR)
#Replacing all NAs for single entries and negative values to 0s for SD as we are interested only in amount higher, not lesser than the mean
payment$SDTotalChrgDiff_HRR[is.na(payment$SDTotalChrgDiff_HRR)] <- 0
payment$SDTotalChrgDiff_HRR[payment$SDTotalChrgDiff_HRR < 0] <- 0
#The following plot shows some outlying points which are DRG-HRR combos where the Standard Deviation of the Provider's charges for a DRG are  
#way higher than the mean of the difference between Average Covered Charges and Mean Medicare payments grouped by DRG-HRR combination.
#we're interested in values higher than a specific set limit, say top 30% with really high deviations
payment <- as.data.table(payment)
df_outlier <- payment[payment$SDTotalChrgDiff_HRR > 0.70*(max(payment$SDTotalChrgDiff_HRR) - min(payment$SDTotalChrgDiff_HRR[payment$SDTotalChrgDiff_HRR> 0])),]
av1 <- payment[,SDTotalChrgDiff_HRR,by=DRGbyHRR]
ggplot(data = av1,mapping = aes(y = DRGbyHRR,x = SDTotalChrgDiff_HRR,colour=SDTotalChrgDiff_HRR))+
  geom_point() +
  geom_point(data=df_outlier, aes(x=SDTotalChrgDiff_HRR,y=DRGbyHRR), color='red',size=1.5)  

tail(payment,10)



#CONCLUSION

#1. Features 8,9,10 related to the ratio of discharges between DRG-Provider combination and total discharges at a State, Zipcode and
#HRR level respectively can help provide us outlier information on how patients from the same location are potentially targeted by provider 
#groups as per Level 6 of Healthcare Fraud Control as defined by Sparrow M. K. (2000)

#2. The remaining features that are derived based on the differences and deviations of charges and payments related to every DRG at a State, Zip and 
#HRR level relative to the mean values can provide us information on the following potential healthcare frauds:
#a. Upcoding - Billing for a service with a higher reimbursement rate than the service provided 
#b. Excessive or Unnecessary Services - Provides medically excessive or unnecessary services to a patient

#3. The cut-off for the outliers will need to be set for every feature with its own criteria based on judgements from interviews
#with experts to focus on potential cases of fraud





#remove column 
#payment <- within(payment,rm(SDTotalPmtDiff_State))