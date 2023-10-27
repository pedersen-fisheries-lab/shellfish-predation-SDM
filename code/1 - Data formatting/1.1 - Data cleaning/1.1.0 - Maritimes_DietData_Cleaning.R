### FORMATING MARITIMES DATA ----
# Purpose: Format Maritimes diet data to match template
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
drive_download(as_id("16w_I14HGWR41aDmfV1pxY1pCvAcoXsTr"),path="code/temp/Maritimes_DietData.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Maritimes_DietData.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                          ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove unecessary columns
Data[,c(1)]<-NULL

# Add mising columns with values if possible or rename columns
Data$region<- "Maritimes"

# add predator names to predator_species column
Data[predator_species==10,predatorspecies:= "gadus_morhua"]
Data[predator_species==23,predatorspecies:= "sebastes_mentella"]
Data[predator_species==31,predatorspecies:= "reinhardtius_hippoglossoides"]
Data$predator_species<-Data$predatorspecies
Data[,c(22)]<-NULL

# Extract day from datetime
Data$day<-format(as.Date(Data$day,format="%Y-%m-%d"), "%d")
Data$day<-as.integer(Data$day)

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","stomach_id","predator_species","predator_length","shrimp_weight","pandalus_sp_weight",
                    "pandalus_borealis_weight","pandalus_montagui_weight","chionoecetes_opilio_weight","foragefish_weight",
                    "other_weight"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                    ### Fixes from data checking script ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove cod with +4000g other weight
Data<-Data[!stomach_id==174813]

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                        ### Save cleaned data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Maritimes_Diet_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Maritimes_DietData.csv")
