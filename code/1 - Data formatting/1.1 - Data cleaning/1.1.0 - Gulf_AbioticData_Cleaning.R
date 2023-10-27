### FORMATING GULF ABIOTIC DATA ----
# Purpose: Format Gulf abiotic data to match template
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
drive_download(as_id("1G7ENMC13hcpGSVY9axWSQe9TYyHQP8E6"),path="code/temp/Gulf_Abiotic_Data.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/Gulf_Abiotic_Data.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                      ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove duplicate rows
Data<-unique(Data)

# Add mising columns with values if possible or rename columns
Data$region<- "Gulf"
Data$year_surv<-Data$year
colnames(Data)[colnames(Data)=="stratum"] <- "strata"
colnames(Data)[colnames(Data)=="area (km^2)"] <- "strata_area"
colnames(Data)[colnames(Data)=="set.number"] <- "set"
colnames(Data)[colnames(Data)=="vessel.code"] <- "vessel"
Data$nafo<-NA
colnames(Data)[colnames(Data)=="latitude.start"] <- "latitude"
colnames(Data)[colnames(Data)=="longitude.start"] <- "longitude"
colnames(Data)[colnames(Data)=="bottom.temperature"] <- "temperature_at_bottom"
colnames(Data)[colnames(Data)=="tow.distance_km"] <- "tow_length"
colnames(Data)[colnames(Data)=="bottom.salinity"] <- "salinity"
Data$oxygen<-NA

# Calculate average depth of tow
Data$depth<-((Data$depth.start+Data$depth.end)/2)

# Remove unnecessary columns
Data[,c(7,11:13,15)]<-NULL

# Calculate tow area
Data$tow_area<-(Data$tow_length*0.012)

# Fix 99 values for temperature and salimity to NAs
Data[temperature_at_bottom==99.9,temperature_at_bottom:=NA]
Data[salinity==99.9,salinity:=NA]

#Add place holder for trip (not used in Gulf region)
Data$trip<-NA

#Remove tow_length (redundant)
Data$tow_length<-NULL

# Standardize vessel names
Data<-as.data.table(Data)
Data$vessel<-as.factor(Data$vessel)
Data[vessel=="P",vessel:="TEM"]
Data[vessel=="H",vessel:="HAM"]
Data[vessel=="N",vessel:="NED"]
Data[vessel=="T",vessel:="TEL"]

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","latitude","longitude","depth","temperature_at_bottom","tow_area","salinity","oxygen"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Save output in GDrive ---
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Gulf_Abiotic_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/Gulf_Abiotic_Data.csv")

