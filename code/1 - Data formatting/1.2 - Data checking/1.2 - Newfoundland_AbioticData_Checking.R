### FORMATING NEWFOUNDLAND RV DATA ----
# Purpose: Ensure Newfoundland Abiotic data is clean and error free
# Author: S. Zabihi-Seissan


                                                  ### Packages ----
library(data.table)
library(ggplot2)


                                                ### Load Data ----

# Load data from temporary folder
Data<- fread("code/temp/Newfoundland_Abiotic_Data_Cleaned.csv")


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
hist(Data$year_surv)

# Year - No zeroes. relatively similar sample size between years.
hist(Data$year)

# Month - No zeroes.No odd values outside of survey periods,
unique(Data$month)

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
september<-Data[month==9]
range(september$day)
october<-Data[month==10]
range(october$day)
november<-Data[month==11]
range(november$day)
december<-Data[month==12]
range(december$day)
rm(list=setdiff(ls(), "Data"))

#Strata - No zeroes. Latitude/longitude correspond with appropriate stratum
#strata <- readOGR('temp/AllStrata_NL.shp')
#coordinates(Data)<-~longitude + latitude
#proj4string(Data)<-proj4string(strata)
#Data2<- fread("temp/AbioticData_NL.csv")
#Data<-cbind(Data2,over(Data,strata))
#datacheck<-subset(Data, !(Data$strata %in% Data$stratum))
#ST<-fortify(strata)
#datacheck$strata<-as.factor(datacheck$strata)
#datacheck$stratum<-as.factor(datacheck$stratum)
ggplot(subset(Data)) + 
#  geom_polygon(data=ST,aes(long,lat,group=group),fill="white",color = "black", size = 0.1, linetype = 1) +
  geom_point(aes(longitude,latitude,colour=strata),size=1)+
  guides(fill=FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_equal()
stratacheck<-datacheck[strata==375] # One set in 375 is way off in another stratum
stratacheck<-datacheck[strata==363] # Couple of sets are north and in stratum 364
stratacheck<-datacheck[strata==347] # Couple of sets are south and in stratum 348
stratacheck<-datacheck[strata==339] # Couple of sets are east and in stratum 340
stratacheck<-datacheck[strata==296] # One set are in stratum 295

# Strata_area - No zeroes.
range(Data$strata_area)

# set - No duplicates within year,vessel,trip (Output will be TRUE if no duplicates)
Data$ID<-paste(Data$year,Data$vessel,Data$trip,Data$set)
length(unique(Data$ID)) == nrow(Data)
Data$ID<-NULL

# trip - No zeroes.
range(Data$trip)

# NAFO - No zeroes. Values consistent with NAFOs in region. Latitude/longitude will be checked in abiotic data
paste(unique(Data$nafo))
range(Data$strata)
ST<-fortify(strata)
ggplot() + 
  geom_polygon(data=ST,aes(long,lat,group=group),fill="white",color = "black", size = 0.1, linetype = 1) +
  geom_point(data=Data,aes(longitude,latitude,colour=nafo),size=0.1)+
  guides(fill=FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_equal()

# Depth - Ensure no extreme values using histograms and plots
hist(Data$depth)
ggplot() + 
#  geom_polygon(data=ST,aes(long,lat,group=group),fill="white",color = "black", size = 0.1, linetype = 1) +
  geom_point(data=Data,aes(longitude,latitude,colour=depth),size=0.1)+
  guides(fill=FALSE)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_equal()

# Temperature_at_bottom - No extreme values using histogram
hist(Data$temperature_at_bottom)

# Tow area - No zeroes or extreme values
hist(Data$tow_area)
range(Data$tow_area)

# Remove all but data
rm(list=setdiff(ls(), "Data"))

# Remove unecessary columns
Data[,c(19:21)]<-NULL
