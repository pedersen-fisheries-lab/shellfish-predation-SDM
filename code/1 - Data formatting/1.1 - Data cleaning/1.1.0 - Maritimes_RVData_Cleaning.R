### FORMATING MARITIMES RV DATA ----
# Purpose: Format Maritime RV data to match template
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
drive_download(as_id("1xHuVHU5YYW8ahggnZr0sIoQx4OkIxq0s"),path="code/temp/Maritimes_RVData.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Maritimes_RVData.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove unecessary columns
Data[,c(1)]<-NULL

# Add mising columns with values if possible or rename columns
Data$region<- "Maritimes"
colnames(Data)[colnames(Data)=="sebastesSP"] <- "sebastes_mentella"
Data$shrimp_sp<-0

# Extract day from datetime
Data$day<-format(as.Date(Data$day,format="%Y-%m-%d"), "%d")
Data$day<-as.integer(Data$day)

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

# Remove shrimp and crab 0s associated with years where they were not recorded (convert to NAs)
Data[year_surv<1998&pandalus_borealis==0,pandalus_borealis:=NA]
Data[year_surv<1981&chionoecetes_opilio==0,chionoecetes_opilio:=NA]

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                            ### Save output in GDrive ---
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Maritimes_RV_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Maritimes_RVData.csv")

