### FORMATTING NEWFOUNDLAND RV CRAB DATA ----
# Purpose: Adding small crab (<60cm) as seperate column in data
# Author: S. Zabihi-Seissan

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                        ### Packages ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(data.table)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                       ### Load data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Load crab data
Data<-fread("code/temp/alldata_allsources.csv")

# Load cleaned RV data
RV_Data<- fread("code/temp/Newfoundland_RV_Data_Cleaned.csv")

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                   ### Processing crab data ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Subset <60cm carapace width
Data<-Data[cw<=60]

# Calculate missing weights
Data$weight<-as.numeric(Data$weight)
Data[is.na(weight),weight:=((exp((log(cw)*3.1089568861) - 8.268567721)))]

# Remove outlier
Data<-Data[weight<250]

# Multiply weights by number of crab (due to sub-sampling)
Data[,weight:=(weight*n)]

# Convert to kilograms
Data[,weight:=(weight/1000)]

# Fix vessel names
Data$vessel<-as.factor(Data$vessel)
Data[vessel=="30",vessel:="TEM"]
Data[vessel=="34",vessel:="NED"]
Data[vessel=="39",vessel:="TEL"]

# Calculate year_surv
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
Data[((year==2018 & month > 2)|(year==2019 & month < 3)),year_surv:=2018]
Data[((year==2019 & month > 2)),year_surv:=2019]

# Calculate total weight per tow
Table<-Data[,sum(weight),by=c('year_surv','vessel','trip','set')]

# Subset RV vessels
Table<-Table[vessel=="TEM"|vessel=="TEL"|vessel=="NED"]

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
Data[,c(1)]<-NULL

# Re-order columns
setcolorder(Data, c("region", "year_surv","year","month","day","strata","strata_area","set","trip","vessel",
                    "nafo","shrimp_sp","pandalus_sp","pandalus_borealis","pandalus_montagui","chionoecetes_opilio","chionoecetes_opilio_small",
                    "gadus_morhua","reinhardtius_hippoglossoides","sebastes_mentella","large_benthivores",
                    "medium_benthivores","small_benthivores","piscivores","planktivores","plankpiscivores","shellfish"))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
                                                    ### Save output ----
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

fwrite(Data, "code/temp/Newfoundland_RV_Data_Cleaned.csv")

