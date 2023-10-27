### ENSURE QUEBEC DIET DATA IS ERROR FREE ----
# Purpose: Ensure Quebec diet data is clean and error free
# Author: S. Zabihi-Seissan


                                                  ### Packages ----

library(data.table)
library(ggplot2)
library(googledrive)


                                                  ### Load Data ----

# Load data from temporary folder
Data<- fread("code/temp/Quebec_Diet_Data_Cleaned.csv")


                                                  ### Data Check ----

# Region - Should only include "Quebec"
unique(Data$region)

# Year_surv - Months should be in appropriate survey year (i.e. 1, 2 should be in previous year)
Data$date<-paste0(Data$year,Data$month)
Data$date<-as.numeric(Data$date)
Data[month>2 & month<6,season:="spring"]
Data[month<3,season:="fall"]
Data[month>6,season:="summer"]
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

july<-Data[month==7]
range(july$day)
august<-Data[month==8]
range(august$day)
september<-Data[month==9]
range(september$day)

rm(list=setdiff(ls(), "Data"))

# Strata - No zeroes. 
range(Data$strata)

# Strata_area - No zeroes.
range(Data$strata_area)

# set - No zeroes or NAs
range(Data$set)
unique(Data$set)

# trip - No zeroes.
unique(Data$trip)

# Vessel - Corresponds with correct vessels employed during surveys
unique(Data$vessel)

# NAFO - No zeroes. Values consistent with NAFOs in region. Latitude/longitude will be checked in abiotic data
unique(Data$nafo)

# stomach_id - No NAs. Should all be unique (output will read TRUE)
length(unique(Data$stomach_id)) == nrow(Data)

# predator_species - No NAs. Should only include : gadus_morhua, sebastes_mentella and reinhardtius_hippoglossoides
unique(Data$predator_species)

# predator_length - No NAs or zeros. No extreme values and check distribution within each species
Data[predator_species=="gadus_morhua",hist(predator_length)]
Data[predator_species=="gadus_morhua",range(predator_length)]
Data[predator_species=="sebastes_mentella",hist(predator_length)]
Data[predator_species=="sebastes_mentella",range(predator_length)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(predator_length)]
Data[predator_species=="reinhardtius_hippoglossoides",range(predator_length)]

# shrimp_weight - No NAs. no extreme values, check distribution (by predator species)
Data[predator_species=="gadus_morhua",hist(shrimp_weight)]
Data[predator_species=="gadus_morhua",range(shrimp_weight)]
Data[predator_species=="gadus_morhua",plot(predator_length,shrimp_weight)]
Data[predator_species=="sebastes_mentella",hist(shrimp_weight)]
Data[predator_species=="sebastes_mentella",range(shrimp_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,shrimp_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(shrimp_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",range(shrimp_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,shrimp_weight)]
plot(Data$year_surv,Data$shrimp_weight)

# pandalus_sp_weight - No NAs. no extreme values, check distribution (by predator species)
Data[predator_species=="gadus_morhua",hist(pandalus_sp_weight)]
Data[predator_species=="gadus_morhua",range(pandalus_sp_weight)]
Data[predator_species=="gadus_morhua",plot(predator_length,pandalus_sp_weight)]
Data[predator_species=="sebastes_mentella",hist(pandalus_sp_weight)]
Data[predator_species=="sebastes_mentella",range(pandalus_sp_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,pandalus_sp_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(pandalus_sp_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",range(pandalus_sp_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,pandalus_sp_weight)]
plot(Data$year_surv,Data$pandalus_sp_weight)

# pandalus_borealis_weight - No NAs. no extreme values, check distribution (by predator species)
Data[predator_species=="gadus_morhua",hist(pandalus_borealis_weight)]
Data[predator_species=="gadus_morhua",range(pandalus_borealis_weight)]
Data[predator_species=="gadus_morhua",plot(predator_length,pandalus_borealis_weight)]
Data[predator_species=="sebastes_mentella",hist(pandalus_borealis_weight)]
Data[predator_species=="sebastes_mentella",range(pandalus_borealis_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,pandalus_borealis_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(pandalus_borealis_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",range(pandalus_borealis_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,pandalus_borealis_weight)]
plot(Data$year_surv,Data$pandalus_borealis_weight)

# pandalus_montagui_weight - No NAs. no extreme values, check distribution (by predator species)
Data[predator_species=="gadus_morhua",hist(pandalus_montagui_weight)]
Data[predator_species=="gadus_morhua",range(pandalus_montagui_weight)]
Data[predator_species=="gadus_morhua",plot(predator_length,pandalus_montagui_weight)]
Data[predator_species=="sebastes_mentella",hist(pandalus_montagui_weight)]
Data[predator_species=="sebastes_mentella",range(pandalus_montagui_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,pandalus_montagui_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(pandalus_montagui_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",range(pandalus_montagui_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,pandalus_montagui_weight)]
plot(Data$year_surv,Data$pandalus_montagui_weight)

# chionoecetes_opilio_weight - No NAs. no extreme values, check distribution (by predator species)
Data[predator_species=="gadus_morhua",hist(chionoecetes_opilio_weight)]
Data[predator_species=="gadus_morhua",range(chionoecetes_opilio_weight)]
Data[predator_species=="gadus_morhua",plot(predator_length,chionoecetes_opilio_weight)]
Data[predator_species=="sebastes_mentella",hist(chionoecetes_opilio_weight)]
Data[predator_species=="sebastes_mentella",range(chionoecetes_opilio_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,chionoecetes_opilio_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(chionoecetes_opilio_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",range(chionoecetes_opilio_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,chionoecetes_opilio_weight)]
plot(Data$year_surv,Data$chionoecetes_opilio_weight)

# foragefish_weight - No NAs. no extreme values, check distribution (by predator species)
Data[predator_species=="gadus_morhua",hist(foragefish_weight)]
Data[predator_species=="gadus_morhua",range(foragefish_weight)]
Data[predator_species=="gadus_morhua",plot(predator_length,foragefish_weight)]
Data[predator_species=="sebastes_mentella",hist(foragefish_weight)]
Data[predator_species=="sebastes_mentella",range(foragefish_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,foragefish_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(foragefish_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",range(foragefish_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,foragefish_weight)]
plot(Data$year_surv,Data$foragefish_weight)

# other_weight - No NAs. no extreme values, check distribution (by predator species)
Data[predator_species=="gadus_morhua",hist(other_weight)]
Data[predator_species=="gadus_morhua",range(other_weight)]
Data[predator_species=="gadus_morhua",plot(predator_length,other_weight)]
Data[predator_species=="sebastes_mentella",hist(other_weight)]
Data[predator_species=="sebastes_mentella",range(other_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,other_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",hist(other_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",range(other_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,other_weight)]
plot(Data$year_surv,Data$other_weight)

# Ensure total weight per stomach ID is reasonable given body size
Data$total_weight<-(Data$shrimp_weight+Data$pandalus_sp_weight+Data$pandalus_borealis_weight
                    +Data$pandalus_montagui_weight+Data$chionoecetes_opilio_weight+
                      Data$foragefish_weight+Data$other_weight)
Data[predator_species=="gadus_morhua",plot(predator_length,total_weight)]
Data[predator_species=="sebastes_mentella",plot(predator_length,total_weight)]
Data[predator_species=="reinhardtius_hippoglossoides",plot(predator_length,total_weight)]

# Remove all but data
rm(list=setdiff(ls(), "Data"))
