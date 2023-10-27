### FORMATING NEWFOUNDLAND RV DATA ----
# Purpose: Ensure Newfoundland RV data is clean and error free
# Author: S. Zabihi-Seissan


                                                  ### Packages ----
library(data.table)
library(ggplot2)
library(googledrive)


                                                ### Load Data ----

# Load data from temporary folder
Data<- fread("code/temp/Newfoundland_RV_Data_Cleaned.csv")


                                                  ### Data Check ----

# Region - Should only include "Newfoundland"
unique(Data$region)

# Year_surv - Months should be in appropriate survey year (i.e. 1, 2 should be in previous year)
Data$date<-paste0(Data$year,Data$month)
Data$date<-as.numeric(Data$date)
Data[month>2 & month<8,season:="spring"]
Data[month<3,season:="fall"]
Data[month>8,season:="fall"]
datacheck<-Data[date<50000]
ggplot()+
  geom_point(data=datacheck, aes(date,year_surv,colour=season),size=1)+
  geom_vline(xintercept=c(19950,19960,19970,19980,19990,20000,20010,20020,20030,20040,20050,20060,20070,20080,
                          20090,20100,20110,20120,20130,20140,20150,20160,20170,20180))
datacheck<-Data[date>50000]
ggplot()+
  geom_point(data=datacheck, aes(date,year_surv,colour=season),size=1)+
  geom_vline(xintercept=c(199500,199600,199700,199800,199900,200000,200100,200200,200300,200400,200500,200600,200700,
                          200800,200900,201000,201100,201200,201300,201400,201500,201600,201700,201800))

# Year - No zeroes. relatively similar sample size between years.
hist(Data$year)

# Month - No zeroes.No odd values outside of survey periods,
unique (Data$month)

# Day - No zeroes. Days correspond to appropriate months
Data$day<-as.numeric(Data$day)
january<-Data[month==1]
range(january$day)
february<-Data[month==2]
range(february$day)
march<-Data[month==3]
range(march$day)
april<-Data[month==4]
range(april$day)
may<-Data[month==5]
range(may$day)
june<-Data[month==6]
range(june$day)
july<-Data[month==7]
range(july$day)
august<-Data[month==8]
range(august$day)
september<-Data[month==9]
range(september$day)
october<-Data[month==10]
range(october$day)
november<-Data[month==11]
range(november$day)
december<-Data[month==12]
range(december$day)
rm(list=setdiff(ls(), "Data"))

# Strata - No zeroes. 
unique(Data$strata)

# Strata_area - No zeroes.
range(Data$strata_area)

# set - No duplicates within year,vessel,trip (Output will be TRUE if no duplicates)
Data$ID<-paste(Data$year,Data$vessel,Data$trip,Data$set)
length(unique(Data$ID)) == nrow(Data)
Data$ID<-NULL

# trip - No zeroes.
range(Data$trip)

# NAFO - No zeroes. Values consistent with NAFOs in region. Latitude/longitude will be checked in abiotic data
unique(Data$nafo)

# shrimp_sp - No NAs. no extreme values, check distribution
hist(Data$shrimp_sp)
hist(log(Data$shrimp_sp))
range(Data$shrimp_sp)
plot(Data$year_surv,Data$shrimp_sp)

# pandalus_sp - No NAs. no extreme values, check distribution
hist(Data$pandalus_sp)
hist(log(Data$pandalus_sp))
range(Data$pandalus_sp)
plot(Data$year_surv,Data$pandalus_sp)

# pandalus_borealis - No NAs. no extreme values, check distribution
hist(Data$pandalus_borealis)
hist(log(Data$pandalus_borealis))
range(Data$pandalus_borealis)
plot(Data$year_surv,Data$pandalus_borealis)

# pandalus_montagui - No NAs. no extreme values, check distribution
hist(Data$pandalus_montagui)
hist(log(Data$pandalus_montagui))
range(Data$pandalus_montagui)
plot(Data$year_surv,Data$pandalus_montagui)

# chionoecetes_opilio - No NAs. no extreme values, check distribution
hist(Data$chionoecetes_opilio)
hist(log(Data$chionoecetes_opilio))
range(Data$chionoecetes_opilio)
plot(Data$year_surv,Data$chionoecetes_opilio)

# gadus_morhua - No NAs. no extreme values, check distribution
hist(Data$gadus_morhua)
hist(log(Data$gadus_morhua))
range(Data$gadus_morhua)
plot(Data$year_surv,Data$gadus_morhua)

# reinhardtius+hippoglossoides - No NAs. no extreme values, check distribution
hist(Data$reinhardtius_hippoglossoides)
hist(log(Data$reinhardtius_hippoglossoides))
range(Data$reinhardtius_hippoglossoides)
plot(Data$year_surv,Data$reinhardtius_hippoglossoides)

# sebastes_mentella - No NAs. no extreme values, check distribution
hist(Data$sebastes_mentella)
hist(log(Data$sebastes_mentella))
range(Data$sebastes_mentella)  #EXTREMELY LARGE VALUE
plot(Data$year_surv,Data$sebastes_mentella)

# large_benthivores - No NAs. no extreme values, check distribution
hist(Data$large_benthivores)
hist(log(Data$large_benthivores))
range(Data$large_benthivores)
plot(Data$year_surv,Data$large_benthivores)

# medium_benthivores - No NAs. no extreme values, check distribution
hist(Data$medium_benthivores)
hist(log(Data$medium_benthivores))
range(Data$medium_benthivores)
plot(Data$year_surv,Data$medium_benthivores)

# small_benthivores - No NAs. no extreme values, check distribution
hist(Data$small_benthivores)
hist(log(Data$small_benthivores))
range(Data$small_benthivores)
plot(Data$year_surv,Data$small_benthivores)

# piscivores - No NAs. no extreme values, check distribution
hist(Data$piscivores)
hist(log(Data$piscivores))
range(Data$piscivores)
plot(Data$year_surv,Data$piscivores)

# planktivores - No NAs. no extreme values, check distribution
hist(Data$planktivores)
hist(log(Data$planktivores))
range(Data$planktivores)
plot(Data$year_surv,Data$planktivores)

# plankpiscivores - No NAs. no extreme values, check distribution
hist(Data$plankpiscivores)
hist(log(Data$plankpiscivores))
range(Data$plankpiscivores)
plot(Data$year_surv,Data$plankpiscivores)

# shellfish - No NAs. no extreme values, check distribution
hist(Data$shellfish)
hist(log(Data$shellfish))
range(Data$shellfish)
plot(Data$year_surv,Data$shellfish)

# Remove unecessary columns
Data[,c(27,28)]<-NULL

# Remove all but data
rm(list=setdiff(ls(), "Data"))
