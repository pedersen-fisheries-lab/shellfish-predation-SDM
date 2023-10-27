### FORMATING ARCTIC RV DATA ----
# Purpose: Format Arctic RV data to match template
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
drive_download(as_id("11PlbFVdYHbhIofejduKWg1gJTezTg8CD"),path="code/temp/Arctic_RV_Data.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Arctic_RV_Data.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                          ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Add mising columns with values if possible or rename columns
colnames(Data)[colnames(Data)=="Region"] <- "region"
Data$region<- "Arctic"
Data[, 12:27][is.na(Data[, 12:27])] <- 0
colnames(Data)[colnames(Data)=="sebastes_sp"] <- "sebastes_mentella"
colnames(Data)[colnames(Data)=="large_benthivore"] <- "large_benthivores"
colnames(Data)[colnames(Data)=="medium_benthivore"] <- "medium_benthivores"
colnames(Data)[colnames(Data)=="small_benthivore"] <- "small_benthivores"
colnames(Data)[colnames(Data)=="piscivore"] <- "piscivores"
colnames(Data)[colnames(Data)=="plankpiscivore"] <- "plankpiscivores"

# Remove hippoglossoides_platessoides (no stomach samples)
Data$hippoglossoides_platessoides<-NULL

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Save output in GDrive ---
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Arctic_RV_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Arctic_RV_Data.csv")
