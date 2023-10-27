### FORMATING NEWFOUNDLAND DATA ----
# Purpose: Format Newfoundland diet data to match template
# Author: S. Zabihi-Seissan

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                          ### Packages ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(data.table)
library(googledrive)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Download Data from GDrive ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Get authorization to access google drive. Browser will prompt you to sign in and copy paste code into console.
drive_auth(reset=TRUE) #Re-run this line if you want to clear authorization

# Download data into working directory 
drive_download(as_id("1LC51kMoKf1xxcgGD3Hrcdk6Ul8HaQH_L"),path="code/temp/Newfoundland_DietData.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Newfoundland_DietData.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                        ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Add mising columns with values if possible or rename columns
Data$region<- "Newfoundland"
colnames(Data)[colnames(Data)=="strat"] <- "strata"
colnames(Data)[colnames(Data)=="strat_area"] <- "strata_area"
colnames(Data)[colnames(Data)=="pandalus_borealis_wieght"] <- "pandalus_borealis_weight"
colnames(Data)[colnames(Data)=="chionoecete_opilio_weight"] <- "chionoecetes_opilio_weight"

# Remove unnecessary columns
Data[,c(22)]<-NULL

# Standardize predator names
Data[predator_species=="GADUS MORHUA",predatorspecies:= "gadus_morhua"]
Data[predator_species=="HIPPOGLOSSOIDES PLATESSOIDES",predatorspecies:= "hippoglossoides_platessoides"]
Data[predator_species=="REINHARDTIUS HIPPOGLOSSOIDES",predatorspecies:= "reinhardtius_hippoglossoides"]
Data[predator_species=="SEBASTES MENTELLA",predatorspecies:= "sebastes_mentella"]
Data[predator_species=="GLYPTOCEPHALUS CYNOGLOSSUS",predatorspecies:= "glyptocephalus_cynoglossus"]
Data[predator_species=="LIMANDA FERRUGINEA",predatorspecies:= "limanda_ferruginea"]
Data[predator_species=="HIPPOGLOSSUS HIPPOGLOSSUS",predatorspecies:= "hippoglossus_hippoglossus"]
Data[predator_species=="MELANOGRAMMUS AEGLEFINUS",predatorspecies:= "melanogrammus_aeglefinus"]
Data[predator_species=="RAJA RADIATA",predatorspecies:= "raja_radiata"]
Data[predator_species=="MERLUCCIUS BILINEARIS",predatorspecies:= "merluccius_bilinearis"]
Data[predator_species=="RAJA SENTA",predatorspecies:= "raja_senta"]
Data[predator_species=="SQUALUS ACANTHIAS",predatorspecies:= "squalus_acanthias"]
Data[predator_species=="UROPHYCIS TENUIS",predatorspecies:= "urophycis_tenuis"]
Data[predator_species=="MACROURUS BERGLAX",predatorspecies:= "macrourus_berglax"]
Data$predator_species<-Data$predatorspecies
Data[,c(22)]<-NULL

# Turn weight column NAs into zeros
Data[, 15:21][is.na(Data[, 15:21])] <- 0

# Standardize vessel names
Data<-as.data.table(Data)
Data$vessel<-as.factor(Data$vessel)
Data[vessel=="30",vessel:="TEM"]
Data[vessel=="34",vessel:="NED"]
Data[vessel=="39",vessel:="TEL"]

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","stomach_id","predator_species","predator_length","shrimp_weight","pandalus_sp_weight",
                    "pandalus_borealis_weight","pandalus_montagui_weight","chionoecetes_opilio_weight","foragefish_weight",
                    "other_weight"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Fixes based on checking code ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Missing lengths turn to NA
Data[predator_length==99999,predator_length:=NA]
# Remove giant turbots (+600cm)
Data<-Data[!(stomach_id==1168605|stomach_id==1180627|stomach_id==1189181)]
# Remove a small cod with 1000 g of shrimp (outlier)
Data[predator_species=="gadus_morhua"&pandalus_borealis_weight>800,pandalus_borealis_weight:=NA]
# Outlier forage fish weight in cod
Data[(predator_species=="gadus_morhua"&foragefish_weight>4000),foragefish_weight:=NA]
# Outlier forage fish weight in redfish
Data[(predator_species=="sebastes_mentella"&foragefish_weight>150),foragefish_weight:=NA]
# Outlier other weight in turbot
Data[(predator_species=="reinhardtius_hippoglossoides"&other_weight>1500),other_weight:=NA]

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                      ### Save cleaned data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Newfoundland_Diet_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Newfoundland_DietData.csv")
