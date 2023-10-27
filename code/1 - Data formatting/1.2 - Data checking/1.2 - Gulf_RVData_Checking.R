### ENSURE GULF RV DATA IS ERROR FREE ----
# Purpose: Ensure Gulf RV data is clean and error free
# Author: S. Zabihi-Seissan


                                                    ### Packages ----

library(data.table)
library(ggplot2)


                                            ### Download Data from GDrive ----

# Load data
Data<- fread("code/temp/Gulf_RV_Data_Cleaned.csv")


                                                    ### Data Check ----

# Region - Should only include "Gulf"
unique(Data$region)

# Year_surv - Months should be in appropriate survey year (i.e. 1, 2 should be in previous year)
all(Data$year_surv==Data$year)

# Year - No zeroes. relatively similar sample size between years.
hist(Data$year)

# Month - No zeroes.No odd values outside of survey periods,
unique (Data$month)

# Day - No zeroes. Days correspond to appropriate months
Data$day<-as.numeric(Data$day)
august<-Data[month==8]
range(august$day)
september<-Data[month==9]
range(september$day)
october<-Data[month==10]
range(october$day)

# Strata - No zeroes.
range(Data$strata,na.rm=TRUE)

# Strata_area - No zeroes.
range(Data$strata_area,na.rm=TRUE)

# set - No duplicates within year,vessel,trip (Output will be TRUE if no duplicates)
Data$ID<-paste(Data$year,Data$vessel,Data$trip,Data$set)
Data<-Data[year_surv==2004|year_surv==2005|year_surv==2006]
length(unique(Data$ID)) == nrow(Data)
Data$ID<-NULL

# NAFO - No zeroes. Values consistent with NAFOs in region. Latitude/longitude will be checked in abiotic data
paste(unique(Data$nafo))

# shrimp_sp - No NAs. no extreme values, check distribution
hist(Data$shrimp_sp)
plot(Data$shrimp_sp)
hist(log(Data$shrimp_sp))
range(Data$shrimp_sp,na.rm=TRUE)

# pandalus_sp - No NAs. no extreme values, check distribution
hist(Data$pandalus_sp)
plot(Data$pandalus_sp)
hist(log(Data$pandalus_sp))
range(Data$pandalus_sp,na.rm=TRUE)

# pandalus_borealis - No NAs. no extreme values, check distribution
hist(Data$pandalus_borealis)
plot(Data$pandalus_borealis)
hist(log(Data$pandalus_borealis))
range(Data$pandalus_borealis,na.rm=TRUE)

# pandalus_montagui - No NAs. no extreme values, check distribution
hist(Data$pandalus_montagui)
plot(Data$pandalus_montagui)
hist(log(Data$pandalus_montagui))
range(Data$pandalus_montagui,na.rm=TRUE)

# chionoecetes_opilio - No NAs. no extreme values, check distribution
hist(Data$chionoecetes_opilio)
plot(Data$chionoecetes_opilio)
hist(log(Data$chionoecetes_opilio))
range(Data$chionoecetes_opilio)

# gadus_morhua - No NAs. no extreme values, check distribution
hist(Data$gadus_morhua)
plot(Data$gadus_morhua)
hist(log(Data$gadus_morhua))
range(Data$gadus_morhua)

# reinhardtius+hippoglossoides - No NAs. no extreme values, check distribution
hist(Data$reinhardtius_hippoglossoides)
plot(Data$reinhardtius_hippoglossoides)
hist(log(Data$reinhardtius_hippoglossoides))
range(Data$reinhardtius_hippoglossoides)

# sebastes_mentella - No NAs. no extreme values, check distribution
hist(Data$sebastes_mentella)
plot(Data$sebastes_mentella)
hist(log(Data$sebastes_mentella))
range(Data$sebastes_mentella)  #EXTREMELY LARGE VALUE

# large_benthivores - No NAs. no extreme values, check distribution
hist(Data$large_benthivores)
plot(Data$large_benthivores)
hist(log(Data$large_benthivores))
range(Data$large_benthivores)

# medium_benthivores - No NAs. no extreme values, check distribution
hist(Data$medium_benthivores)
plot(Data$medium_benthivores)
hist(log(Data$medium_benthivores))
range(Data$medium_benthivores)

# small_benthivores - No NAs. no extreme values, check distribution
hist(Data$small_benthivores)
plot(Data$small_benthivores)
hist(log(Data$small_benthivores))
range(Data$small_benthivores)

# piscivores - No NAs. no extreme values, check distribution
hist(Data$piscivores)
plot(Data$piscivores)
hist(log(Data$piscivores))
range(Data$piscivores)

# planktivores - No NAs. no extreme values, check distribution
hist(Data$planktivores)
plot(Data$planktivores)
hist(log(Data$planktivores))
range(Data$planktivores)

# plankpiscivores - No NAs. no extreme values, check distribution
hist(Data$plankpiscivores)
plot(Data$plankpiscivores)
hist(log(Data$plankpiscivores))
range(Data$plankpiscivores)

# shellfish - No NAs. no extreme values, check distribution
hist(Data$shellfish)
plot(Data$shellfish)
hist(log(Data$shellfish)) #Weird distribution
range(Data$shellfish)

# Remove unecessary columns
Data[,c(27,28)]<-NULL

# Remove all but data
rm(list=setdiff(ls(), "Data"))
