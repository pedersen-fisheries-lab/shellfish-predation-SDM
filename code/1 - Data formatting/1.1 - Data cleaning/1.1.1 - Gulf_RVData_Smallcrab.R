### FORMATTING GULF RV CRAB DATA ----
# Purpose: Adding small crab (<60cm) as seperate column in data
# Author: S. Zabihi-Seissan

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
### Packages ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(data.table)
library(tidyr)
library(googledrive)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
### Load data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Get authorization to access google drive. Browser will prompt you to sign in and copy paste code into console.
drive_auth(reset=TRUE) #Re-run this line if you want to clear authorization

# Download data into working directory 
drive_download(as_id("1cYa7PeSvQ3LtUY6Naueh43bV2HAxLDWO"),path="code/temp/gul_smallcrab_data.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/gul_smallcrab_data.csv')

# Load cleaned RV data
RV_Data<- fread("code/temp/Gulf_RV_Data_Cleaned.csv")

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
### Processing crab data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Convert NA values to 0s
Data[is.na(sum.less.60),sum.less.60:=0]
Data[is.na(sum.all),sum.all:=0]

# Rename columns
colnames(Data)[colnames(Data)=="cruise.number"] <- "trip"
colnames(Data)[colnames(Data)=="set.number"] <- "set"
colnames(Data)[colnames(Data)=="vessel.str"] <- "vessel"
colnames(Data)[colnames(Data)=="stratum"] <- "strata"

# Rename vessels consistent with RV data
Data[vessel=="CCGS Alfred Needler",vessel:="NED"]
Data[vessel=="CCGS Templeman",vessel:="TEM"]
Data[vessel=="CCGS Teleost",vessel:="TEL"]

# Calculate year_surv
Data$year_surv<-Data$year

# Remove unnecessary columns
Data[,c(1:2,8:22)]<-NULL

# Calculate total weight per tow
Table<-Data[,sum(sum.less.60),by=c('set','vessel','strata','month','day','year_surv')]
colnames(Table)[colnames(Table)=="V1"] <- "chionoecetes_opilio_small"

Table_all<-Data[,sum(sum.all),by=c('set','vessel','strata','month','day','year_surv')]
colnames(Table_all)[colnames(Table_all)=="V1"] <- "chionoecetes_opilio"


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
### Merging crab data to RV data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Create merging ID
Table$ID<-paste(Table$year_surv,Table$vessel,Table$strata,Table$set)
Table_all$ID<-paste(Table_all$year_surv,Table_all$vessel,Table_all$strata,Table_all$set)
RV_Data$ID<-paste(RV_Data$year_surv,RV_Data$vessel,RV_Data$strata,RV_Data$set)

# Remove duplicate data columns from RV data
Table[,c(1:6)]<-NULL
Table_all[,c(1:6)]<-NULL

# Remove original snow crab data (was not correct)
RV_Data[,c(16)]<-NULL

# Merge the datasets
Data<-merge(RV_Data,Table,all.x=TRUE, by ="ID")
Data<-merge(Data,Table_all,all.x=TRUE,by="ID")

# Remove ID column
Data[,c(1)]<-NULL

# Re-order data
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio","chionoecetes_opilio_small",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))


#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
### Save output ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Gulf_RV_Data_Cleaned.csv")
