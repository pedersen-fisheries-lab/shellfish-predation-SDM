### FORMATTING MARITIMES RV CRAB DATA ----
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
drive_download(as_id("1Ba0pZ1Ypop-g0z9qz8QoMcbBUki6wfSS"),path="code/temp/mar_smallcrab_data.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/mar_smallcrab_data.csv')

# Load cleaned RV data
RV_Data<- fread("code/temp/Maritimes_RV_Data_Cleaned.csv")

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Processing crab data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Subset <60cm carapace width
Data<-Data[flen<=60]

# Rename columns
colnames(Data)[colnames(Data)=="fwt"] <- "weight"
colnames(Data)[colnames(Data)=="setno"] <- "set"

# Fix trip ID
Data[trip<100&trip>9,trip2:=as.character(paste("0",trip,sep=""))]
Data[trip<10,trip2:=as.character(paste("00",trip,sep=""))]
Data[trip>99,trip2:=as.character(trip)]
Data$trip<-paste(Data$vessel,Data$year,Data$trip2,sep="")

# Calculate missing weights
Data$weight<-as.numeric(Data$weight)
Data[is.na(weight),weight:=((exp((log(flen)*3.1089568861) - 8.268567721)))]

# Remove outliers
Data<-Data[weight<250]
Data<-Data[!(weight>25&flen<20)]

# Convert to kilograms
Data[,weight:=(weight/1000)]

# Calculate year_surv
colnames(Data)[colnames(Data)=="year"] <- "year_surv"

# Calculate total weight per tow
Data<-as.data.table(Data)
Table<-Data[,sum(weight),by=c('year_surv','vessel','trip','set')]

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                              ### Merging crab data to RV data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Create merging ID
Table$ID<-paste(Table$year_surv,Table$vessel,Table$trip,Table$set)
RV_Data$ID<-paste(RV_Data$year_surv,RV_Data$vessel,RV_Data$trip,RV_Data$set)

# Remove duplicate data columns from RV data
Table[,c(1:4)]<-NULL

# Merge the datasets
Data<-merge(RV_Data,Table,all.x=TRUE, by ="ID")

# Rename small crab column and replace NAs with 0 for data 1999+
colnames(Data)[colnames(Data)=="V1"] <- "chionoecetes_opilio_small"
Data[is.na(chionoecetes_opilio_small)&year_surv>=1999,chionoecetes_opilio_small:=0]

# Remove ID column
Data[,c(1)]<-NULL

# Re-order columns
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio","chionoecetes_opilio_small",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                    ### Save output ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Maritimes_RV_Data_Cleaned.csv")

