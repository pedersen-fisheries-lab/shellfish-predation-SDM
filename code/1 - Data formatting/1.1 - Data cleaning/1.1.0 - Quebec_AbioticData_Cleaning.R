### FORMATING QUEBEC ABIOTIC DATA ----
# Purpose: Format Quebec abiotic data to match template
# Author: S. Zabihi-Seissan

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                      ### Packages ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(data.table)
library(googledrive)
library(geosphere)
library(raster)

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

# Remove unnecessary columns
Data[,c(2,11:14,24:26)]<-NULL

# Add mising columns with values
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
colnames(Data)[colnames(Data)=="Temp"] <- "temperature_at_bottom"
colnames(Data)[colnames(Data)=="Salinity"] <- "salinity"
colnames(Data)[colnames(Data)=="Oxygene"] <- "oxygen"
colnames(Data)[colnames(Data)=="Lat1"] <- "latitude"
colnames(Data)[colnames(Data)=="Long1"] <- "longitude"

# Calculate mean depth
Data$depth<-(as.numeric(Data$Prof_1)+as.numeric(Data$Prof_2))/2

# Calculate tow_length
Data$Lat2<-as.numeric(Data$Lat2)
Data$Long2<-as.numeric(Data$Long2)
Data$tow_length<-pointDistance(cbind(Data$longitude,Data$latitude),cbind(Data$Long2,Data$Lat2),type='GreatCircle',lonlat=TRUE)
Data$tow_length<-(Data$tow_length/1000)

# Calculate tow_area
Data[vessel=="6",tow_area:=tow_length*0.01341]
Data[vessel=="16",tow_area:=tow_length*0.01694]

# Fix vessel ID
Data<-as.data.table(Data)
Data$vessel<-as.factor(Data$vessel)
Data[vessel==6,vessel:="NED"]
Data[vessel==16,vessel:="TEL"]

# Remove tow_length (redundant)
Data[,c(21)]<-NULL

# Add year_surv
Data$year_surv<-Data$year

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                              ### Grouping data by survey sets ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

Data<-unique(Data)
rm(list=setdiff(ls(), "Data"))

# Remove unnecessary columns
Data[,12:15]<-NULL

# Turn "." into NAs
Data[ Data == "." ] <- NA

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","latitude","longitude","depth","temperature_at_bottom","tow_area","salinity","oxygen"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                ### Save output to GDrive ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Quebec_Abiotic_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/QuebecRVRawData.csv")

