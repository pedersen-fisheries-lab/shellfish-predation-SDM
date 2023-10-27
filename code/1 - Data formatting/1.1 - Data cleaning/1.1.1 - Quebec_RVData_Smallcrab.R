### FORMATTING QUEBEC RV CRAB DATA ----
# Purpose: Adding small crab (<60cm) as seperate column in data
# Author: S. Zabihi-Seissan

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                            ### Packages ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(data.table)
library(googledrive)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                            ### Load data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Get authorization to access google drive. Browser will prompt you to sign in and copy paste code into console.
drive_auth(reset=TRUE) #Re-run this line if you want to clear authorization

# Download data into working directory 
drive_download(as_id("1sSSBPxNYfwR150c6tT9HppEJACWIBoHr"),path="code/temp/qc_smallcrab_data.csv",overwrite=TRUE)

# Load data
Data<-fread('code/temp/qc_smallcrab_data.csv')

# Load cleaned RV data
RV_Data<- fread("code/temp/Quebec_RV_Data_Cleaned.csv")

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                    ### Processing crab data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Subset <=60cm carapace width
Data<-Data[LCephalo<=60]

# Calculate missing weights
Data[,weight:=((exp((log(LCephalo)*3.1089568861) - 8.268567721)))]

# Convert to kilograms
Data[,weight:=(weight/1000)]

# Calculate year_surv
Data$year_surv<-Data$Year

# Rename important variables
colnames(Data)[colnames(Data)=="Cfv"] <- "vessel"
colnames(Data)[colnames(Data)=="No_Releve"] <- "trip"
colnames(Data)[colnames(Data)=="No_Stn"] <- "set"

# Rename vessels
Data$vessel<-as.factor(Data$vessel)
Data[vessel==34,vessel:="NED"]
Data[vessel==39,vessel:="TEL"]

# Calculate total weight per tow
Table<-Data[,sum(weight),by=c('year_surv','vessel','trip','set')]

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Merging crab data to RV data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Create merging ID
Table$ID<-paste(Table$year_surv,Table$vessel,Table$trip,Table$set)
RV_Data$ID<-paste(RV_Data$year_surv,RV_Data$vessel,RV_Data$trip,RV_Data$set)

# Remove duplicate data columns from RV data
Table[,c(1:4)]<-NULL

# Merge the datasets
Data<-merge(RV_Data,Table,all.x=TRUE, by ="ID")

# Rename small crab column and replace NAs with 0
colnames(Data)[colnames(Data)=="V1"] <- "chionoecetes_opilio_small"
Data[is.na(chionoecetes_opilio_small),chionoecetes_opilio_small:=0]

# Cap small crab weight to total crab weight
Data$dif<-(Data$chionoecetes_opilio-Data$chionoecetes_opilio_small)
Data[dif<0,chionoecetes_opilio_small:=chionoecetes_opilio]
Data[,c(29)]<-NULL

# Remove ID column
Data[,c(1)]<-NULL

# Re-order columns
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio","chionoecetes_opilio_small",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

### Save output ----

fwrite(Data, "code/temp/Quebec_RV_Data_Cleaned.csv")

