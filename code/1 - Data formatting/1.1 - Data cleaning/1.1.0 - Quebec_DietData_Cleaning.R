### FORMATING QUEBEC DATA ----
# Purpose: Format Quebec diet data to match template
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
drive_download(as_id("1-ELs66SLh_Bd0_m_ubT-eJ6jSEZI6Rrx"),path="code/temp/Quebec_DietData.csv",overwrite=TRUE)
drive_download(as_id("15ELw2h2E7SaPHWO3aFKZaewju793cRad"),path="code/temp/Quebec_StrataInfo.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Quebec_DietData.csv')
Strata<-fread('code/temp/Quebec_StrataInfo.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                          ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Rename region to match other datasets
Data$region<-"Quebec"

# Add predator names to predator_species column
Data[predator_species==438,predatorspecies:= "gadus_morhua"]
Data[predator_species==792,predatorspecies:= "sebastes_mentella"]
Data[predator_species==892,predatorspecies:= "reinhardtius_hippoglossoides"]
Data$predator_species<-Data$predatorspecies
Data[,c(21)]<-NULL

# Add strata area
Strata[,c(2:4,6:12)]<-NULL
colnames(Strata)[colnames(Strata)=="stratum_area"] <- "strata_area"
colnames(Strata)[colnames(Strata)=="stratum_nb"] <- "strata"
Data<-merge(Data,Strata,by =c("strata"),all.x=TRUE)

# Create unique ID for stomachs
Data$stomach_id<-seq.int(nrow(Data))

# Convert predator_length from mm to cm
Data$predator_length<-(Data$predator_length/10)

# Standardize vessel names
Data<-as.data.table(Data)
Data$vessel<-as.factor(Data$vessel)
Data[vessel==34,vessel:="NED"]
Data[vessel==39,vessel:="TEL"]

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","stomach_id","predator_species","predator_length","shrimp_weight","pandalus_sp_weight",
                    "pandalus_borealis_weight","pandalus_montagui_weight","chionoecetes_opilio_weight","foragefish_weight",
                    "other_weight"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                      ### Save cleaned data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Quebec_Diet_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Quebec_DietData.csv")
file.remove("code/temp/Quebec_StrataInfo.csv")

