### MERGING RV SURVEY DATA ----
# Purpose: Merge formatted RV data from all five Atlantic regions
# Author: S. Zabihi-Seissan


                                                            ### Packages ----

library(data.table)
library(googledrive)


                    ### Load cleaned data from temporary files folder (After cleaning using 1.1 cleaning code) ----

Arctic_Data<-fread('code/temp/Arctic_RV_Data_Cleaned.csv')
Gulf_Data<-fread('code/temp/Gulf_RV_Data_Cleaned.csv')
Maritimes_Data<-fread('code/temp/Maritimes_RV_Data_Cleaned.csv')
Newfoundland_Data<-fread('code/temp/Newfoundland_RV_Data_Cleaned.csv')
Quebec_Data<-fread('code/temp/Quebec_RV_Data_Cleaned.csv')

# Rbind regions together
RV_Data<-rbind(Gulf_Data,Maritimes_Data,Newfoundland_Data,Quebec_Data,Arctic_Data,fill=TRUE)

rm(list=setdiff(ls(), "RV_Data"))


                                          ### Upload cleaned aggregated abiotic data----

fwrite(RV_Data, "code/temp/RV_Data_Allregions.csv")
drive_upload("code/temp/RV_Data_Allregions.csv",path=as_id("1n9T_4BIZxJqB7KypYUIOx0Slh4ytgYnh",name="RV_Data_Allregions"))
