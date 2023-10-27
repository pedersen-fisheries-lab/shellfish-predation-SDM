### MERGING DIET DATA ----
# Purpose: Merge formatted diet data from all five Atlantic regions
# Author: S. Zabihi-Seissan


                                                            ### Packages ----

library(data.table)


                 ### Load cleaned data from temporary files folder (After cleaning using 1.1 cleaning code) ----

Arctic_Data<-fread('code/temp/Arctic_Diet_Data_Cleaned.csv')
Gulf_Data<-fread('code/temp/Gulf_Diet_Data_Cleaned.csv')
Maritimes_Data<-fread('code/temp/Maritimes_Diet_Data_Cleaned.csv')
Newfoundland_Data<-fread('code/temp/Newfoundland_Diet_Data_Cleaned.csv')
Quebec_Data<-fread('code/temp/Quebec_Diet_Data_Cleaned.csv')

# Rbind regions together
Diet_Data<-rbind(Arctic_Data,Gulf_Data)
Diet_Data<-rbind(Diet_Data,Maritimes_Data)
Diet_Data<-rbind(Diet_Data,Newfoundland_Data)
Diet_Data<-rbind(Diet_Data,Quebec_Data)

rm(list=setdiff(ls(), "Diet_Data"))


                                              ### Upload cleaned aggregated diet data----

fwrite(Diet_Data, "code/temp/Diet_Data_Allregions.csv")
drive_upload("code/temp/Diet_Data_Allregions.csv",path=as_id("1n9T_4BIZxJqB7KypYUIOx0Slh4ytgYnh",name="Diet_Data_Allregions"))
