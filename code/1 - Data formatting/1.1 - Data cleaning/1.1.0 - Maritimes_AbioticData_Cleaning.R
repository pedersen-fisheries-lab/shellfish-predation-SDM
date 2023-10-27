### FORMATING MARITIMES ABIOTIC DATA ----
# Purpose: Format Maritimes abiotic data to match template
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
drive_download(as_id("1MB6lwe-tIWAyVimh955-M9H-E4IcDtTF"),path="code/temp/MRAbiotic_Data.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/MRAbiotic_Data.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove unnecessary columns
Data[,c(1)]<-NULL

# Add mising columns with values
Data$region<- "Maritimes"
colnames(Data)[colnames(Data)=="sakm2"] <- "tow_area"
Data$oxygen<-NA

# Extract day from datetime
Data$day<-format(as.Date(Data$day,format="%Y-%m-%d"), "%d")
Data$day<-as.integer(Data$day)

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","latitude","longitude","depth","temperature_at_bottom","tow_area","salinity","oxygen"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Save output to GDrive ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Maritimes_Abiotic_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/MRAbiotic_Data.csv")

