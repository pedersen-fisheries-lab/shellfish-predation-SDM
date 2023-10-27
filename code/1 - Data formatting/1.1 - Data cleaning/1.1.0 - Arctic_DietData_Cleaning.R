### FORMATING ARCTIC DIET DATA ----
# Purpose: Format Arctic diet data to match template
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
drive_download(as_id("1KjS6tXu3oLSZj4BE9fu2Valb23Y2UkFw"),path="code/temp/Arctic_Diet_Data.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Arctic_Diet_Data.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                        ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#Rename region
Data$region<-"Arctic"

# add predator names to predator_species column
Data[predator_species=='REINHARDTIUS HIPPOGLOSSOIDES',predatorspecies:= "reinhardtius_hippoglossoides"]
Data[predator_species=='GADUS MORHUA',predatorspecies:= "gadus_morhua"]
Data[predator_species=='SEBASTES SP',predatorspecies:= "sebastes_mentella"]
Data[predator_species=='SEBASTES MENTELLA',predatorspecies:= "sebastes_mentella"]
Data[predator_species=='RAJIDAE',predatorspecies:= "other"]
Data[predator_species=='RAJA RADIATA = AMBLYRAJA RADIATA',predatorspecies:= "other"]
Data$predator_species<-Data$predatorspecies
Data[,c(22)]<-NULL
Data<-Data[!predator_species=="other"]

# Add zeros to stomach weights instead of NAs
Data[, 15:21][is.na(Data[, 15:21])] <- 0

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","stomach_id","predator_species","predator_length","shrimp_weight","pandalus_sp_weight",
                    "pandalus_borealis_weight","pandalus_montagui_weight","chionoecetes_opilio_weight","foragefish_weight",
                    "other_weight"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Save cleaned data----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Arctic_Diet_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Arctic_Diet_Data.csv")

