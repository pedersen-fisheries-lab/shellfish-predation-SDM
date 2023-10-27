### FORMATING NEWFOUNDLAND RV DATA ----
# Purpose: Format Newfoundland RV data to match template
# Author: S. Zabihi-Seissan

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Packages ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(data.table)
library(dplyr)
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

# Remove unecessary columns
Data[,c(4,8,11:32,34,36:41)]<-NULL

# Add mising columns with values if possible or rename columns
Data$region<- "Newfoundland"
colnames(Data)[colnames(Data)=="stratum"] <- "strata"
colnames(Data)[colnames(Data)=="nafodiv"] <- "nafo"
colnames(Data)[colnames(Data)=="Trip"] <- "trip"
colnames(Data)[colnames(Data)=="Vessel"] <- "vessel"

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

# Group species codes in wanted taxonomical groups
Data[species %in% c(8020,8021),shrimp_sp:=wgt] # MAY NEED CHANGES
Data[species==8110,pandalus_sp:=wgt]
Data[species %in% c(8111,9801,9803,9804,9806,9807,9809,9810,9811),pandalus_borealis:=wgt]
Data[species %in% c(8112,9812:9819),pandalus_montagui:=wgt]
Data[species %in% c(8213,9851,9852),chionoecetes_opilio:=wgt]
Data[species==438,gadus_morhua:=wgt]
Data[species==892,reinhardtius_hippoglossoides:=wgt]
Data[species==794,sebastes_mentella:=wgt]
Data[species %in% c(12,89,90,92,95:98,100,102,117,120:122,134,164,368,385,386,441,458,474,698,699,700,
                    701,721,744,889,966,980,981),large_benthivores:=wgt]
Data[species %in% c(56,91,93,94,99,359,365,369,370,373,391,394,431:434,440,445,481,482,609,697,714,716,726,
                    727,729,730,746,809,817,819,820,821,849,882,890,891,895),medium_benthivores:=wgt]
Data[species %in% c(146,147,168,195,199,200,202,205,206,211,220,304,307,311,387,427,435,453,461,471,478,483,500,502,
                    514,517,520,527,616,679,703,711,717,745,747,781,808,810,813,818,822,823,828:830,832,835:838,
                    843,853,907,969,982),small_benthivores:=wgt]
Data[species %in% c(15,20,23,24,27,28,50:52,173,227,229,230,246,250,297,316,323:325,329,379,430,436,439,443,
                    447:450,452,456,465,547,548,893,964),piscivores:=wgt]
Data[species %in% c(48,149:152,155,167,187,192:194,272,375,398,421,423,426,572,694,712),planktivores:=wgt]
Data[species %in% c(300,382,44,451,506,791,793,863),plankpiscivores:=wgt]
Data[species %in% c(7927,8024:8026,8029,8030,8033,8034,8040:8042,8046,8047,8051:8053,8055:8057,8059,8060,8074,
                    8075,8077,8079:8081,8084:8087,8091:8093,8095,8113,8119:8121,8124,8125:8130,8133:8135,8138,
                    8139,8145,8164,8177,8196,8200,8203,8205:8207,8216:8218,8231,8232,8252),shellfish:=wgt] # MAY NEED CHANGES

# Turn NAs into 0
Data[, 14:28][is.na(Data[, 14:28])] <- 0

# Remove unnecessary columns
Data[,c(9:10)]<-NULL

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                         ### Grouping data by survey year, vessel, trip and set ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Create set unique ID
Data$ID<-paste(Data$year,Data$vessel,Data$trip,Data$set)

# Group data for each measure of abundance
shrimp_sp<-aggregate(Data$shrimp_sp, by=list(Category=Data$ID), FUN=sum)
colnames(shrimp_sp)[colnames(shrimp_sp)=="x"] <- "shrimp_sp"
pandalus_sp<-aggregate(Data$pandalus_sp, by=list(Category=Data$ID), FUN=sum)
colnames(pandalus_sp)[colnames(pandalus_sp)=="x"] <- "pandalus_sp"
pandalus_borealis<-aggregate(Data$pandalus_borealis, by=list(Category=Data$ID), FUN=sum)
colnames(pandalus_borealis)[colnames(pandalus_borealis)=="x"] <- "pandalus_borealis"
pandalus_montagui<-aggregate(Data$pandalus_montagui, by=list(Category=Data$ID), FUN=sum)
colnames(pandalus_montagui)[colnames(pandalus_montagui)=="x"] <- "pandalus_montagui"
chionoecetes_opilio<-aggregate(Data$chionoecetes_opilio, by=list(Category=Data$ID), FUN=sum)
colnames(chionoecetes_opilio)[colnames(chionoecetes_opilio)=="x"] <- "chionoecetes_opilio"
gadus_morhua<-aggregate(Data$gadus_morhua, by=list(Category=Data$ID), FUN=sum)
colnames(gadus_morhua)[colnames(gadus_morhua)=="x"] <- "gadus_morhua"
reinhardtius_hippoglossoides<-aggregate(Data$reinhardtius_hippoglossoides, by=list(Category=Data$ID), FUN=sum)
colnames(reinhardtius_hippoglossoides)[colnames(reinhardtius_hippoglossoides)=="x"] <- "reinhardtius_hippoglossoides"
sebastes_mentella<-aggregate(Data$sebastes_mentella, by=list(Category=Data$ID), FUN=sum)
colnames(sebastes_mentella)[colnames(sebastes_mentella)=="x"] <- "sebastes_mentella"
large_benthivores<-aggregate(Data$large_benthivores, by=list(Category=Data$ID), FUN=sum)
colnames(large_benthivores)[colnames(large_benthivores)=="x"] <- "large_benthivores"
medium_benthivores<-aggregate(Data$medium_benthivores, by=list(Category=Data$ID), FUN=sum)
colnames(medium_benthivores)[colnames(medium_benthivores)=="x"] <- "medium_benthivores"
small_benthivores<-aggregate(Data$small_benthivores, by=list(Category=Data$ID), FUN=sum)
colnames(small_benthivores)[colnames(small_benthivores)=="x"] <- "small_benthivores"
piscivores<-aggregate(Data$piscivores, by=list(Category=Data$ID), FUN=sum)
colnames(piscivores)[colnames(piscivores)=="x"] <- "piscivores"
planktivores<-aggregate(Data$planktivores, by=list(Category=Data$ID), FUN=sum)
colnames(planktivores)[colnames(planktivores)=="x"] <- "planktivores"
plankpiscivores<-aggregate(Data$plankpiscivores, by=list(Category=Data$ID), FUN=sum)
colnames(plankpiscivores)[colnames(plankpiscivores)=="x"] <- "plankpiscivores"
shellfish<-aggregate(Data$shellfish, by=list(Category=Data$ID), FUN=sum)
colnames(shellfish)[colnames(shellfish)=="x"] <- "shellfish"

# Merge all species data together
mergedData<-Reduce(function(x, y) merge(x, y, all=TRUE), list(shrimp_sp,pandalus_sp,pandalus_borealis,pandalus_montagui,chionoecetes_opilio,gadus_morhua,
                                                              reinhardtius_hippoglossoides,sebastes_mentella,large_benthivores,medium_benthivores,small_benthivores,
                                                              piscivores,planktivores,plankpiscivores,shellfish))
colnames(mergedData)[colnames(mergedData)=="Category"] <- "ID"

# Reference data for merged data
refData<-Data[,c(1:11,27)]
refData<-unique(refData)

# Final data ouput
Data<-merge(mergedData,refData, by ="ID")
Data$ID<-NULL
rm(list=setdiff(ls(), "Data"))

# Standardize vessel names
Data<-as.data.table(Data)
Data$vessel<-as.factor(Data$vessel)
Data[vessel=="30",vessel:="TEM"]
Data[vessel=="34",vessel:="NED"]
Data[vessel=="39",vessel:="TEL"]

# Re-order columsn to match template
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                  ### Save output in GDrive ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Newfoundland_RV_Data_Cleaned.csv")

# Remove unnnecessary temporary files
file.remove("code/temp/All Ms data NL fall95_spring_2018.12.03.csv")
file.remove("code/temp/Strata_info_2018.12.03.csv")
