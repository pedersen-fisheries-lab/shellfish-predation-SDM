### FORMATING NEWFOUNDLAND ABIOTIC DATA ----
# Purpose: Format Newfoundland abiotic data to match template
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
drive_download(as_id("1OD1J8YHQfhOmtxC-EAeTTrqWgTXT_DmL"),path="code/temp/All MS data NL fall95_spring_2018.12.03.csv",overwrite=TRUE)
drive_download(as_id("1Wg4fmlp-8FPhvKF_pidv-osSNXW_ek_V"),path="code/temp/Strata_info_2018.12.03.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/All MS data NL fall95_spring_2018.12.03.csv')
Strata<-fread('code/temp/Strata_info_2018.12.03.csv')

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Processing ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Remove damaged sets
Data<-Data[dam<3]

# Remove set types that are not "survey sets"
Data<-Data[settype==1]

# Remove NA strata
Data<-na.omit(Data, cols="stratum")

# Keep appropriate gear
Data<-Data[gear %in% c(61,101,102,103)]

# Remove unnecessary columns
Data[,c(4,8,11:19,21,23:25,31:41)]<-NULL

# Add mising columns with values
Data$region<- "Newfoundland"
colnames(Data)[colnames(Data)=="stratum"] <- "strata"
colnames(Data)[colnames(Data)=="nafodiv"] <- "nafo"
colnames(Data)[colnames(Data)=="Trip"] <- "trip"
colnames(Data)[colnames(Data)=="Vessel"] <- "vessel"
colnames(Data)[colnames(Data)=="depth_mean"] <- "depth"
colnames(Data)[colnames(Data)=="T"] <- "temperature_at_bottom"
colnames(Data)[colnames(Data)=="dis"] <- "tow_length"
Data$tow_length<-((Data$tow_length)*1.852)
Data$salinity<-NA
Data$oxygen<-NA

# Convert degrees minutes seconds into decimal degrees
Data$start_latmin<-(Data$start_latmin/600)
Data$start_longmin<-(Data$start_longmin/600)
colnames(Data)[colnames(Data)=="start_latdeg"] <- "latitude"
colnames(Data)[colnames(Data)=="start_longdeg"] <- "longitude"
Data$latitude<-(Data$latitude+Data$start_latmin)
Data$longitude<-(-(Data$longitude+Data$start_longmin))
Data$start_latmin<-NULL
Data$start_longmin<-NULL

# Fix temperature data
Data$temperature_at_bottom<-as.numeric(Data$temperature_at_bottom)
Data[temperature_at_bottom<900,temperature_at_bottom:= (temperature_at_bottom/10)]
Data[temperature_at_bottom==900,temperature_at_bottom:=0]
Data[temperature_at_bottom>900,temperature_at_bottom:=((-(temperature_at_bottom-900))/10)]

# Calculate strata area
Data$strata_area<-Strata[match(Data$strata, Strata$stratum),2]

# Modify year format
Data$year<-as.numeric(Data$year)
Data[year>50,year:=(year+1900)]
Data[year<50,year:=(year+2000)]

# Add year_surv
Data[year==1995|((year==1996 & month<3)),year_surv:=1995]
Data[((year==1996 & month > 2)|(year==1997 & month < 3)),year_surv:=1996]
Data[((year==1997 & month > 2)|(year==1998 & month < 3)),year_surv:=1997]
Data[((year==1998 & month > 2)|(year==1999 & month < 3)),year_surv:=1998]
Data[((year==1999 & month > 2)|(year==2000 & month < 3)),year_surv:=1999]
Data[((year==2000 & month > 2)|(year==2001 & month < 3)),year_surv:=2000]
Data[((year==2001 & month > 2)|(year==2002 & month < 3)),year_surv:=2001]
Data[((year==2002 & month > 2)|(year==2003 & month < 3)),year_surv:=2002]
Data[((year==2003 & month > 2)|(year==2004 & month < 3)),year_surv:=2003]
Data[((year==2004 & month > 2)|(year==2005 & month < 3)),year_surv:=2004]
Data[((year==2005 & month > 2)|(year==2006 & month < 3)),year_surv:=2005]
Data[((year==2006 & month > 2)|(year==2007 & month < 3)),year_surv:=2006]
Data[((year==2007 & month > 2)|(year==2008 & month < 3)),year_surv:=2007]
Data[((year==2008 & month > 2)|(year==2009 & month < 3)),year_surv:=2008]
Data[((year==2009 & month > 2)|(year==2010 & month < 3)),year_surv:=2009]
Data[((year==2010 & month > 2)|(year==2011 & month < 3)),year_surv:=2010]
Data[((year==2011 & month > 2)|(year==2012 & month < 3)),year_surv:=2011]
Data[((year==2012 & month > 2)|(year==2013 & month < 3)),year_surv:=2012]
Data[((year==2013 & month > 2)|(year==2014 & month < 3)),year_surv:=2013]
Data[((year==2014 & month > 2)|(year==2015 & month < 3)),year_surv:=2014]
Data[((year==2015 & month > 2)|(year==2016 & month < 3)),year_surv:=2015]
Data[((year==2016 & month > 2)|(year==2017 & month < 3)),year_surv:=2016]
Data[((year==2017 & month > 2)|(year==2018 & month < 3)),year_surv:=2017]
Data[((year==2018 & month > 2)),year_surv:=2018]

# Calculate tow area (wing spread 55.25 feet or 16.8402 m / 0.0168402 km)
Data$tow_area<-(0.0168402*Data$tow_length)

# Remove tow length (redundant)
Data[,c(9)]<-NULL

# Standardize vessel names
Data$vessel<-as.factor(Data$vessel)
Data<-as.data.table(Data)
Data[vessel==30,vessel:="TEM"]
Data[vessel==34,vessel:="NED"]
Data[vessel==39,vessel:="TEL"]

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Grouping data by survey sets ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

Data<-unique(Data)
rm(list=setdiff(ls(), "Data"))

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","latitude","longitude","depth","temperature_at_bottom","tow_area","salinity","oxygen"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Save output to GDrive ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Newfoundland_Abiotic_Data_Cleaned.csv")

# Remove unnecessary temporary files
file.remove("code/temp/All MS data NL fall95_spring_2018.12.03.csv")
file.remove("code/temp/Strata_info_2018.12.03.csv")
