### FORMATING GULF RV DATA ----
# Purpose: Format Gulf RV data to match template
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
drive_download(as_id("1Ocnqh3uKnlSKHDachJL36aZRcwl-eGiZ"),path="code/temp/Gulf_RVDataFishGroup.csv",overwrite=TRUE)
drive_download(as_id("1QlT7M1jkIbhVFdUzygFbn8QO7KGWU0gc"),path="code/temp/Gulf_RVDataMainFish.csv",overwrite=TRUE)
drive_download(as_id("1IHvXXl9bTqp0hZsR6lqTjew7wHePfuJw"),path="code/temp/Gulf_RVDataCrab.csv",overwrite=TRUE)
drive_download(as_id("1Thgi_231jogCm-itU3EdG1pOBbhj_eCM"),path="code/temp/Gulf_RVDataShrimp.csv",overwrite=TRUE)

# Load data
Data_FishGroup<-fread('code/temp/Gulf_RVDataFishGroup.csv')
Data_MainFish<-fread('code/temp/Gulf_RVDataMainFish.csv')
Data_Crab<-fread('code/temp/Gulf_RVDataCrab.csv')
Data_Shrimp<-fread('code/temp/Gulf_RVDataShrimp.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                      ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Create ID to merge both datasets
Data_MainFish$ID<-paste(Data_MainFish$year,Data_MainFish$set.number,Data_MainFish$vessel.code,Data_MainFish$stratum)
Data_FishGroup$ID<-paste(Data_FishGroup$year,Data_FishGroup$set.number,Data_FishGroup$vessel.code,Data_FishGroup$stratum)
Data_Crab$ID<-paste(Data_Crab$year,Data_Crab$set.number,Data_Crab$vessel.code,Data_Crab$stratum)
Data_Shrimp$ID<-paste(Data_Shrimp$year,Data_Shrimp$set,Data_Shrimp$vessel,Data_Shrimp$stratum)

# Remove duplicate columns from data sets
Data_FishGroup[,c(1:5)]<-NULL
Data_Crab[,c(1:61,63:66)]<-NULL
Data_Shrimp[,c(1:17,21)]<-NULL

# Merge datasets by ID
Data<-merge(Data_MainFish,Data_FishGroup, by ="ID",all=TRUE)
Data<-merge(Data,Data_Crab,by="ID",all=TRUE)
Data<-merge(Data,Data_Shrimp,by="ID",all=TRUE)

# Remove original datasets and ID column
rm(list=setdiff(ls(), c("Data")))
Data$ID<-NULL

# Add mising columns with values if possible or rename columns
Data$region<- "Gulf"
Data$year_surv<-Data$year
colnames(Data)[colnames(Data)=="stratum"] <- "strata"
colnames(Data)[colnames(Data)=="area"] <- "strata_area"
colnames(Data)[colnames(Data)=="set.number"] <- "set"
colnames(Data)[colnames(Data)=="vessel.code"] <- "vessel"
Data$nafo<-0
colnames(Data)[colnames(Data)=="Shrimp.biomass..kg."] <- "shrimp_sp"
Data$pandalus_sp<-0
colnames(Data)[colnames(Data)=="Pandalus_borealis"] <- "pandalus_borealis"
colnames(Data)[colnames(Data)=="Pandalus_montagui"] <- "pandalus_montagui"
colnames(Data)[colnames(Data)=="SNOW.CRAB.(QUEEN).weight.caught"] <- "chionoecetes_opilio"
colnames(Data)[colnames(Data)=="Atlantic cod"] <- "gadus_morhua"
colnames(Data)[colnames(Data)=="Shrimp.biomass..kg."] <- "shrimp_sp"
colnames(Data)[colnames(Data)=="Turbot"] <- "reinhardtius_hippoglossoides"
colnames(Data)[colnames(Data)=="Redfish, Unseperated"] <- "sebastes_mentella"
colnames(Data)[colnames(Data)=="Large Benthivore_biomass"] <- "large_benthivores"
colnames(Data)[colnames(Data)=="Medium Benthivore_biomass"] <- "medium_benthivores"
colnames(Data)[colnames(Data)=="Small Benthivore_biomass"] <- "small_benthivores"
colnames(Data)[colnames(Data)=="Piscivore_biomass"] <- "piscivores"
colnames(Data)[colnames(Data)=="Planktivores_biomass"] <- "planktivores"
colnames(Data)[colnames(Data)=="PlankPiscivore_biomass"] <- "plankpiscivores"
colnames(Data)[colnames(Data)=="Shellfish_biomass"] <- "shellfish"

# Remove unnecessary columns
Data[,c(7,10,13,15,17,19,21,23,25,27)]<-NULL

# Add place holder for trip column
Data$trip<-NA

# Standardize vessel names
Data[vessel=="P",vessel:="TEM"]
Data[vessel=="H",vessel:="HAM"]
Data[vessel=="N",vessel:="NED"]
Data[vessel=="T",vessel:="TEL"]

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                    ### Save output in GDrive ---
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Gulf_RV_Data_Cleaned.csv")

# Remove temporary files
file.remove("code/temp/Gulf_RVDataFishGroup.csv")
file.remove("code/temp/Gulf_RVDataMainFish.csv")
file.remove("code/temp/Gulf_RVDataCrab.csv")
file.remove("code/temp/Gulf_RVDataShrimp.csv")
