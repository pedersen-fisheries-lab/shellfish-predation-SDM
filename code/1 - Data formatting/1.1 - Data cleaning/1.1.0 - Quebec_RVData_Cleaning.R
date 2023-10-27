### FORMATING QUEBEC RV DATA ----
# Purpose: Format Quebec RV data to match template
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
drive_download(as_id("17ZzqTNijNK0kVUjfbs81W7t7klJSIHhd"),path="code/temp/QuebecRVRawData.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/QuebecRVRawData.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                      ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove set types that are not "Random stratified"
Data<-Data[Tow_Typ==1]

# Remove damaged sets
Data<-Data[Result<3]

# Remove NA strata
Data<-na.omit(Data, cols="Stratum")

# Remove unecessary columns
Data[,c(2,11:23)]<-NULL

# Add mising columns with values if possible or rename columns
Data$region<- "Quebec"
colnames(Data)[colnames(Data)=="Stratum"] <- "strata"
colnames(Data)[colnames(Data)=="Nafo"] <- "nafo"
colnames(Data)[colnames(Data)=="No_Releve"] <- "trip"
colnames(Data)[colnames(Data)=="Source"] <- "vessel"
colnames(Data)[colnames(Data)=="No_Stn"] <- "set"
colnames(Data)[colnames(Data)=="Year"] <- "year"
colnames(Data)[colnames(Data)=="Month"] <- "month"
colnames(Data)[colnames(Data)=="Day"] <- "day"
colnames(Data)[colnames(Data)=="Strate_Area"] <- "strata_area"

# Add year_surv
Data$year_surv<-Data$year

# Group species codes in wanted taxonomical groups
#Data[species %in% c(8020,8021),shrimp_sp:=wgt] # MAY NEED CHANGES    NOT PRESENT IN DATA
Data$shrimp_sp<-0
Data[Cod_Esp_Gen==660,pandalus_sp:=Catch]
Data[Cod_Esp_Gen==661,pandalus_borealis:=Catch]
Data[Cod_Esp_Gen==662,pandalus_montagui:=Catch]
Data[Cod_Esp_Gen==720,chionoecetes_opilio:=Catch]
Data[Cod_Esp_Gen==1093,gadus_morhua:=Catch]
Data[Cod_Esp_Gen==1505,reinhardtius_hippoglossoides:=Catch]
Data[Cod_Esp_Gen==1233,sebastes_mentella:=Catch]    
Data[Cod_Esp_Gen %in% c(808,813,816:818,865,866,871:876,879,889,904,916,974,1095,1100,1126,1147,1159,1417,1421:1424)
                    ,large_benthivores:=Catch]
Data[Cod_Esp_Gen %in% c(835,868,869,877,903,908,931,933,934,1083:1085,1097,1110,1122,1254,1258,1260,1261,1263,1277,1312
                      ,1390,1392,1400,1402,1411,1413,1486,1499,1503),medium_benthivores:=Catch]
Data[Cod_Esp_Gen %in% c(968,970,971,973,978,1001,1005,1008,1029,1033,1046,1087,1101,1102,1118,1131,1133,1154,1190
                        ,1192,1216,1246,1247,1253,1255,1257,1259,1262,1264,1269:1273,1278,1282,1345,1384,1405,1408
                        ,1418,1426,1471),small_benthivores:=Catch]
Data[Cod_Esp_Gen %in% c(811,828,830,831,848:850,853,855,937,994,1015,1031,1036,1048,1054,1055,1059:1061,1082,1089
                        ,1094,1098,1108,1112,1114:1116,1145,1447,1501),piscivores:=Catch]
Data[Cod_Esp_Gen %in% c(833,941,944:947,964:966,977,983,1063,1174,1201,1210,1212,1215,1410,1435,1456)
                        ,planktivores:=Catch]
Data[Cod_Esp_Gen %in% c(939,1091,1196,1231,1283),plankpiscivores:=Catch]
#Data[species %in% c(),shellfish:=wgt] # MAY NEED CHANGES       NONE OF SPECIES PRESENT
Data$shellfish<-0

# Turn NAs into 0
Data[, 15:29][is.na(Data[, 15:29])] <- 0

# Remove unnecessary columns
Data[,c(10:12)]<-NULL

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                  ### Grouping data by survey year, vessel, trip and set ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Create set unique ID
Data$ID<-paste(Data$year,Data$vessel,Data$trip,Data$set)

# Group data for each measure of abundance
shrimp_sp<-aggregate(Data$shrimp_sp, by=list(Category=Data$ID), FUN=sum)
colnames(shrimp_sp)[colnames(shrimp_sp)=="x"] <- "shrimp_sp"
pandalus_sp<-aggregate(Data$pandalus_sp, by=list(Category=Data$ID), FUN=sum)
colnames(pandalus_sp)[colnames(pandalus_sp)=="x"] <- "pandalus_sp"
pandalus_borealis<-aggregate(Data$pandalus_borealis, by=list(Category=Data$ID), FUN=sum)
colnames(pandalus_borealis)[colnames(pandalus_borealis)=="x"] <- "pandalus_borealis"
pandalus_montagui<-aggregate(Data$pandalus_montagui, by=list(Category=Data$ID), FUN=sum)
colnames(pandalus_montagui)[colnames(pandalus_montagui)=="x"] <- "pandalus_montagui"
chionoecetes_opilio<-aggregate(Data$chionoecetes_opilio, by=list(Category=Data$ID), FUN=sum)
colnames(chionoecetes_opilio)[colnames(chionoecetes_opilio)=="x"] <- "chionoecetes_opilio"
gadus_morhua<-aggregate(Data$gadus_morhua, by=list(Category=Data$ID), FUN=sum)
colnames(gadus_morhua)[colnames(gadus_morhua)=="x"] <- "gadus_morhua"
reinhardtius_hippoglossoides<-aggregate(Data$reinhardtius_hippoglossoides, by=list(Category=Data$ID), FUN=sum)
colnames(reinhardtius_hippoglossoides)[colnames(reinhardtius_hippoglossoides)=="x"] <- "reinhardtius_hippoglossoides"
sebastes_mentella<-aggregate(Data$sebastes_mentella, by=list(Category=Data$ID), FUN=sum)
colnames(sebastes_mentella)[colnames(sebastes_mentella)=="x"] <- "sebastes_mentella"
large_benthivores<-aggregate(Data$large_benthivores, by=list(Category=Data$ID), FUN=sum)
colnames(large_benthivores)[colnames(large_benthivores)=="x"] <- "large_benthivores"
medium_benthivores<-aggregate(Data$medium_benthivores, by=list(Category=Data$ID), FUN=sum)
colnames(medium_benthivores)[colnames(medium_benthivores)=="x"] <- "medium_benthivores"
small_benthivores<-aggregate(Data$small_benthivores, by=list(Category=Data$ID), FUN=sum)
colnames(small_benthivores)[colnames(small_benthivores)=="x"] <- "small_benthivores"
piscivores<-aggregate(Data$piscivores, by=list(Category=Data$ID), FUN=sum)
colnames(piscivores)[colnames(piscivores)=="x"] <- "piscivores"
planktivores<-aggregate(Data$planktivores, by=list(Category=Data$ID), FUN=sum)
colnames(planktivores)[colnames(planktivores)=="x"] <- "planktivores"
plankpiscivores<-aggregate(Data$plankpiscivores, by=list(Category=Data$ID), FUN=sum)
colnames(plankpiscivores)[colnames(plankpiscivores)=="x"] <- "plankpiscivores"
shellfish<-aggregate(Data$shellfish, by=list(Category=Data$ID), FUN=sum)
colnames(shellfish)[colnames(shellfish)=="x"] <- "shellfish"

# Merge all species data together
mergedData<-Reduce(function(x, y) merge(x, y, all=TRUE), list(shrimp_sp,pandalus_sp,pandalus_borealis,pandalus_montagui,chionoecetes_opilio,gadus_morhua,
                                                              reinhardtius_hippoglossoides,sebastes_mentella,large_benthivores,medium_benthivores,small_benthivores,
                                                              piscivores,planktivores,plankpiscivores,shellfish))
colnames(mergedData)[colnames(mergedData)=="Category"] <- "ID"

# Reference data for merged data
refData<-Data[,c(1:11,27)]
refData<-unique(refData)

# Final data ouput
Data<-merge(mergedData,refData, by ="ID")
Data$ID<-NULL
rm(list=setdiff(ls(), "Data"))
Data<-as.data.table(Data)

# Standardize vessel names
Data<-as.data.table(Data)
Data$vessel<-as.factor(Data$vessel)
Data[vessel==6,vessel:="NED"]
Data[vessel==16,vessel:="TEL"]

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                    ### Save output in GDrive ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Quebec_RV_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/QuebecRVRawData.csv")

