### MERGING Abiotic SURVEY DATA ----
# Purpose: Merge formatted Abiotic data from all five Atlantic regions
# Author: S. Zabihi-Seissan


                                                        ### Packages ----

library(data.table)


                    ### Load cleaned data from temporary files folder (After cleaning using 1.1 cleaning code) ----

Arctic_Data<-fread('code/temp/Arctic_Abiotic_Data_Cleaned.csv')
Gulf_Data<-fread('code/temp/Gulf_Abiotic_Data_Cleaned.csv')
Maritimes_Data<-fread('code/temp/Maritimes_Abiotic_Data_Cleaned.csv')
Newfoundland_Data<-fread('code/temp/Newfoundland_Abiotic_Data_Cleaned.csv')
Quebec_Data<-fread('code/temp/Quebec_Abiotic_Data_Cleaned.csv')

# Rbind regions together
Abiotic_Data<-rbind(Arctic_Data,Gulf_Data)
Abiotic_Data<-rbind(Abiotic_Data,Maritimes_Data)
Abiotic_Data<-rbind(Abiotic_Data,Newfoundland_Data)
Abiotic_Data<-rbind(Abiotic_Data,Quebec_Data)

rm(list=setdiff(ls(), "Abiotic_Data"))


                                           ### Upload cleaned aggregated abiotic data----

fwrite(Abiotic_Data, "code/temp/Abiotic_Data_Allregions.csv")
drive_upload("code/temp/Abiotic_Data_Allregions.csv",path=as_id("1n9T_4BIZxJqB7KypYUIOx0Slh4ytgYnh",name="Abiotic_Data_Allregions"))
