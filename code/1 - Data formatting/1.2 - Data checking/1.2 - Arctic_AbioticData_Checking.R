### FORMATING ARCTIC RV DATA ----
# Purpose: Ensure Arctic Abiotic data is clean and error free
# Author: S. Zabihi-Seissan


                                                        ### Packages ----

library(data.table)
library(ggplot2)
library(googledrive)


                                                ### Download Data from GDrive ----

# Load data from temporary folder
Data<- fread("code/temp/Arctic_Abiotic_Data_Cleaned.csv")


                                                        ### Data Check ----

# Region - Should only include "Arctic"
unique(Data$region)

# Year_surv - Months should be in appropriate survey year (i.e. 1, 2 should be in previous year)
ggplot()+
  geom_point(data=Data, aes(year,year_surv),size=1)

# Year - No zeroes. relatively similar sample size between years.
unique(Data$year)

# Month - No zeroes.No odd values outside of survey periods.
unique (Data$month)

# Day - No zeroes. Days correspond to appropriate months
Data$day<-as.numeric(Data$day)
august<-Data[month==8]
range(august$day)
september<-Data[month==9]
range(september$day)
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
paste(unique(Data$nafo))
range(Data$strata)
ggplot() + 
  geom_point(data=Data,aes(longitude,latitude),size=0.1)+
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
  geom_point(data=Data,aes(longitude,latitude,colour=depth),size=2)+
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
