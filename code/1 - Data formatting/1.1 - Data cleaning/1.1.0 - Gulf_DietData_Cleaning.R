### FORMATING GULF DATA ----
# Purpose: Format Gulf diet data to match template
# Author: S. Zabihi-Seissan

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                          ### Packages ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(data.table)
library(dplyr)
library(googledrive)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Download Data from GDrive ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Get authorization to access google drive. Browser will prompt you to sign in and copy paste code into console.
drive_auth(reset=TRUE) #Re-run this line if you want to clear authorization

# Download data into working directory 
drive_download(as_id("1cEtE40u1oO6pQBlMNPYB4Z5L3l7EG4wV"),path="code/temp/Gulf_Diet_Data.csv",overwrite=TRUE)
drive_download(as_id("1NTK3_InPHkyuuNgO8XvokBznQYXyH4rq"),path="code/temp/Gulf_StrataInfo.csv",overwrite=TRUE)
drive_download(as_id("1nWfEw7GwOg9XG17f6zHshvIV9lB4rP45"),path="code/temp/Gulf_AbioticData.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Gulf_Diet_Data.csv')
Strata_info<-fread('code/temp/Gulf_StrataInfo.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                        ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove unecessary columns
Data[,c(1,3:7,11,23:26)]<-NULL

# Add mising columns with values if possible or rename columns
Data$region<- "Gulf"
colnames(Data)[colnames(Data)=="SeqNoUhl"] <- "stomach_id"
colnames(Data)[colnames(Data)=="Set_N"] <- "set"
colnames(Data)[colnames(Data)=="Survey"] <- "vessel"
colnames(Data)[colnames(Data)=="STRATA_N"] <- "strata"
colnames(Data)[colnames(Data)=="Year"] <- "year"
colnames(Data)[colnames(Data)=="Month"] <- "month"
colnames(Data)[colnames(Data)=="Day"] <- "day"
colnames(Data)[colnames(Data)=="PredLen"] <- "predator_length"
Data$year_surv<-Data$year

# Reduce Strata_info to strata ID and strata area
Data<-na.omit(Data, cols="strata")
Data<-merge(Data,Strata_info, by ="strata",all=TRUE)
rm(list=setdiff(ls(), "Data"))

# Add predator names to predator_species column
Data[Predator==10,predatorspecies:= "gadus_morhua"]
Data[Predator==23,predatorspecies:= "sebastes_mentella"]
Data[Predator==31,predatorspecies:= "reinhardtius_hippoglossoides"]
Data$Predator<-Data$predatorspecies
Data[,c(9,19)]<-NULL
Data<-na.omit(Data, cols="Predator")
colnames(Data)[colnames(Data)=="Predator"] <- "predator_species"

# Remove samples with no measured stomachs (NAs)
Data[StomWt==-99,StomWt:=NA]
Data<-na.omit(Data, cols="StomWt")

# Change -99 predator length to NAs
Data[predator_length==-99,predator_length:=NA]
Data[predator_length==0.0,predator_length:=NA]

# Add wanted weight columns
Data[PreyName=="SHRIMPS",shrimp_weight:=PreyWt]
Data[PreyName=="PANDALUS SP.",pandalus_sp_weight:=PreyWt]
Data[PreyName=="PANDALUS BOREALIS",pandalus_borealis_weight:=PreyWt]
Data[PreyName=="PANDALUS MONTAGUI",pandalus_montagui_weight:=PreyWt]
Data[PreyName=="SNOW CRAB (QUEEN)",chionoecetes_opilio_weight:=PreyWt]
Data[PreyName=="HERRING(ATLANTIC)"|PreyName=="CAPELIN"|PreyName=="SAND LANCE (NS)",foragefish_weight:=PreyWt]
Data[!PreyName=="HERRING(ATLANTIC)"&!PreyName=="CAPELIN"&!PreyName=="SAND LANCE (NS)"&!PreyName=="SHRIMPS"
     &!PreyName=="PANDALUS SP."&!PreyName=="PANDALUS BOREALIS"&!PreyName=="PANDALUS MONTAGUI"
     &!PreyName=="SNOW CRAB (QUEEN)",other_weight:=PreyWt]

# Remove unnecessary prey columns
Data[,c(10:14)]<-NULL

# Add zeros to NA weights
Data[,13:19][is.na(Data[,13:19])] <- 0
Data[other_weight==-99,other_weight:=0]

# Fix one istance of missing data (cod stomach id 4743)
Data[stomach_id=="4743",year:=2006]
Data[stomach_id=="4743",month:=9]
Data[stomach_id=="4743",day:=25]
Data[stomach_id=="4743",year_surv:=2006]

# Merge diet weights by stomach ID
Shrimp<-aggregate(Data$shrimp_weight, by=list(Category=Data$stomach_id), FUN=sum)
Shrimp$shrimp_weight<-Shrimp$x
Shrimp$x<-NULL
Pandalus_sp<-aggregate(Data$pandalus_sp_weight, by=list(Category=Data$stomach_id), FUN=sum)
Pandalus_sp$pandalus_sp_weight<-Pandalus_sp$x
Pandalus_sp$x<-NULL
Pandalus_borealis<-aggregate(Data$pandalus_borealis_weight, by=list(Category=Data$stomach_id), FUN=sum)
Pandalus_borealis$pandalus_borealis_weight<-Pandalus_borealis$x
Pandalus_borealis$x<-NULL
Pandalus_montagui<-aggregate(Data$pandalus_montagui_weight, by=list(Category=Data$stomach_id), FUN=sum)
Pandalus_montagui$pandalus_montagui_weight<-Pandalus_montagui$x
Pandalus_montagui$x<-NULL
Chionoecetes_opilio<-aggregate(Data$chionoecetes_opilio_weight, by=list(Category=Data$stomach_id), FUN=sum)
Chionoecetes_opilio$chionoecetes_opilio_weight<-Chionoecetes_opilio$x
Chionoecetes_opilio$x<-NULL
Foragefish<-aggregate(Data$foragefish_weight, by=list(Category=Data$stomach_id), FUN=sum)
Foragefish$foragefish_weight<-Foragefish$x
Foragefish$x<-NULL
Other<-aggregate(Data$other_weight, by=list(Category=Data$stomach_id), FUN=sum)
Other$other_weight<-Other$x
Other$x<-NULL

AgData<-merge(Shrimp,Pandalus_sp, by ="Category")
AgData<-merge(AgData,Pandalus_borealis, by ="Category")
AgData<-merge(AgData,Pandalus_montagui, by ="Category")
AgData<-merge(AgData,Chionoecetes_opilio, by ="Category")
AgData<-merge(AgData,Foragefish, by ="Category")
AgData<-merge(AgData,Other, by ="Category")
AgData$stomach_id<-AgData$Category
AgData$Category<-NULL

RefData<-Data
RefData[,c(13:19)]<-NULL
RefData<-unique(RefData)

AgData<-merge(AgData,RefData, by ="stomach_id")
Data<-AgData

# Add other missing columns
Data$nafo<-NA

# Add place holder for gulf trip column
Data$trip<-NA

# Standardize vessel names
Data<-as.data.table(Data)
Data$vessel<-as.factor(Data$vessel)
Data[vessel=="T",vessel:="TEL"]
Data[vessel=="N",vessel:="NED"]

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","stomach_id","predator_species","predator_length","shrimp_weight","pandalus_sp_weight",
                    "pandalus_borealis_weight","pandalus_montagui_weight","chionoecetes_opilio_weight","foragefish_weight",
                    "other_weight"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                    ### Save cleaned data----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Gulf_Diet_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Gulf_Diet_Data.csv")
file.remove("code/temp/Gulf_StrataInfo.csv")

