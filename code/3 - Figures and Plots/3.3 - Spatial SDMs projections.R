### MODEL PROJECTIONS FOR SPATIAL SPECIES DISTRIBUTION MODELS ----
# Purpose: Produce plots to visualize the latitude/longitude SDMs
# Author: S. Zabihi-Seissan


                                                        ### Packages ----

library(data.table)
library(ggplot2)
library(googledrive)
library(statmod)
library(mgcv)
library(ggpubr)


                                                    ### Bring in real data ----

# Abiotic data
drive_download(as_id("14a9UWxAe4oHO3l_p3dBvas-gK6psh6MX"),path="code/temp/Abiotic_Data_Allregions.csv",overwrite=TRUE)
Abiotic_Data<-fread('code/temp/Abiotic_Data_Allregions.csv')

# RV data
drive_download(as_id("1GiCLxZXfHeeicGiIQb59t9E2JDl41jHR"),path="code/temp/RV_Data_Allregions.csv",overwrite=TRUE)
RV_Data<-fread('code/temp/RV_Data_Allregions.csv')


                                                      ### Merge datasets ----

# Create merging ID
Abiotic_Data$ID<-paste(Abiotic_Data$region,Abiotic_Data$year_surv,Abiotic_Data$vessel,Abiotic_Data$trip,Abiotic_Data$set)
RV_Data$ID<-paste(RV_Data$region,RV_Data$year_surv,RV_Data$vessel,RV_Data$trip,RV_Data$set)

# Remove duplicate data columns from RV data
RV_Data[,c(1:11)]<-NULL

# Merge the datasets
Data<-merge(Abiotic_Data,RV_Data, by ="ID")


                                                ### Modify coordimate system ----

coordinates(Data)<-~longitude + latitude
proj4string(Data)<- CRS ("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=tmerc +lat_0=0 +lon_0=-59.5 +k=0.998 +x_0=1000000 +y_0=0 +ellps=clrk66 +units=m +no_defs") #determined using proj4string(NL)
Data<- spTransform(Data, CRS.new)
Data<-as.data.table(Data)


                                                ### Prepare data for modelling ----

# Calculate total biomass
Data$total_biomass<-rowSums(Data[,c(18:32)],na.rm=TRUE)

# Standardize abundances based on tow_area
Data$shrimp_sp<-(Data$shrimp_sp/Data$tow_area)
Data$pandalus_sp<-(Data$pandalus_sp/Data$tow_area)
Data$pandalus_borealis<-(Data$pandalus_borealis/Data$tow_area)
Data$pandalus_montagui<-(Data$pandalus_montagui/Data$tow_area)
Data$chionoecetes_opilio<-(Data$chionoecetes_opilio/Data$tow_area)
Data$gadus_morhua<-(Data$gadus_morhua/Data$tow_area)
Data$reinhardtius_hippoglossoides<-(Data$reinhardtius_hippoglossoides/Data$tow_area)
Data$sebastes_mentella<-(Data$sebastes_mentella/Data$tow_area)
Data$hippoglossoides_platessoides<-(Data$hippoglossoides_platessoides/Data$tow_area)
Data$large_benthivores<-(Data$large_benthivores/Data$tow_area)
Data$medium_benthivores<-(Data$medium_benthivores/Data$tow_area)
Data$small_benthivores<-(Data$small_benthivores/Data$tow_area)
Data$piscivores<-(Data$piscivores/Data$tow_area)
Data$Planktivores<-(Data$Planktivores/Data$tow_area)
Data$plankpiscivores<-(Data$plankpiscivores/Data$tow_area)
Data$shellfish<-(Data$shellfish/Data$tow_area)
Data$total_biomass<-(Data$total_biomass/Data$tow_area)

# Load Predicted Data
drive_download(as_id("1UHxsy6a-F63dYlCzVkUHMaGeIQd3kLia"),path="code/temp/Projected_Spatial_Allregions.csv",overwrite=TRUE)
Predicted_Data<-fread('code/temp/Projected_Spatial_Allregions.csv')

rm(list=setdiff(ls(), c("Data","Predicted_Data")))


                                                                  ### Plot ----

Plot_LatLong_Newfoundland_Fall<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_fall"&year_surv==2017&pred_crab<1000),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2)&year_surv==2017)),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland Fall - 2017")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_LatLong_Newfoundland_Fall
Plot_LatLong_Newfoundland_Spring<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_spring"&year_surv==2018),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7)&year_surv==2018)),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland Spring - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_LatLong_Newfoundland_Spring
Plot_LatLong_Quebec<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Quebec"&year_surv==2018&pred_crab<6000),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,region=="Quebec"&year_surv==2018),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Quebec - 2019")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_LatLong_Quebec
Plot_LatLong_Maritimes<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Maritimes"&year_surv==2018),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,region=="Maritimes"&year_surv==2018),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Maritimes - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_LatLong_Maritimes
Plot_Cod_LatLong_Gulf<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Gulf"&year_surv==2017),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,region=="Gulf"&year_surv==2017),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Gulf - 2017")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Cod_LatLong_Gulf
Plot_Cod_LatLong_Arctic<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,fill=pred_shrimp))+
  geom_point(data=subset(Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,color=pandalus_borealis),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Arctic - 2018 - Northern shrimp")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Cod_LatLong_Arctic

ggarrange(Plot_Cod_LatLong_Quebec,Plot_Cod_LatLong_Newfoundland,Plot_Cod_LatLong_Arctic,Plot_Cod_LatLong_Gulf,Plot_Cod_LatLong_Maritimes,ncol=3,nrow=2)

#Turbot
Plot_Turbot_LatLong_Newfoundland_Fall<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_fall"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_turbot))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2)&year_surv==2005)),aes(x=longitude,y=latitude,color=reinhardtius_hippoglossoides),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Turbot_LatLong_Newfoundland_Fall
Plot_Turbot_LatLong_Newfoundland_Spring<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_spring"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_turbot))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7)&year_surv==2005)),aes(x=longitude,y=latitude,color=reinhardtius_hippoglossoides),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Turbot_LatLong_Newfoundland_Spring
Plot_Turbot_LatLong_Quebec<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_turbot+0.001))+
  geom_point(data=subset(Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,color=reinhardtius_hippoglossoides),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Quebec - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Turbot_LatLong_Quebec
Plot_Turbot_LatLong_Maritimes<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_turbot))+
  geom_point(data=subset(Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,color=log(reinhardtius_hippoglossoides)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Maritimes - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Turbot_LatLong_Maritimes
Plot_Turbot_LatLong_Gulf<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_turbot+0.001)))+
  geom_point(data=subset(Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,color=log(reinhardtius_hippoglossoides)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Gulf - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Turbot_LatLong_Gulf
Plot_Turbot_LatLong_Arctic<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,fill=pred_turbot))+
  geom_point(data=subset(Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,color=reinhardtius_hippoglossoides),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Arctic - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Turbot_LatLong_Arctic

ggarrange(Plot_Turbot_LatLong_Quebec,Plot_Turbot_LatLong_Newfoundland,Plot_Turbot_LatLong_Arctic,Plot_Turbot_LatLong_Gulf,Plot_Turbot_LatLong_Maritimes,ncol=3,nrow=2)

#Redfish
Plot_Redfish_LatLong_Newfoundland_Fall<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_fall"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_redfish))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2)&year_surv==2005)),aes(x=longitude,y=latitude,color=sebastes_mentella),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Redfish_LatLong_Newfoundland_Fall
Plot_Redfish_LatLong_Newfoundland_Spring<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_spring"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_redfish))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7)&year_surv==2005)),aes(x=longitude,y=latitude,color=sebastes_mentella),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Redfish_LatLong_Newfoundland_Spring
Plot_Redfish_LatLong_Quebec<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_redfish))+
  geom_point(data=subset(Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,color=sebastes_mentella),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Quebec - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Redfish_LatLong_Quebec
Plot_Redfish_LatLong_Maritimes<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_redfish))+
  geom_point(data=subset(Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,color=sebastes_mentella),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Maritimes - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Redfish_LatLong_Maritimes
Plot_Redfish_LatLong_Gulf<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_redfish))+
  geom_point(data=subset(Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,color=sebastes_mentella),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Gulf - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Redfish_LatLong_Gulf
Plot_Redfish_LatLong_Arctic<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,fill=pred_redfish))+
  geom_point(data=subset(Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,color=sebastes_mentella),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Arctic - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Redfish_LatLong_Arctic

ggarrange(Plot_Redfish_LatLong_Quebec,Plot_Redfish_LatLong_Newfoundland,Plot_Redfish_LatLong_Arctic,Plot_Redfish_LatLong_Gulf,Plot_Redfish_LatLong_Maritimes,ncol=3,nrow=2)

#Crab
Plot_Crab_LatLong_Newfoundland_Fall<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_fall"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2)&year_surv==2005)),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Crab_LatLong_Newfoundland_Fall
Plot_Crab_LatLong_Newfoundland_Spring<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_spring"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7)&year_surv==2005)),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Crab_LatLong_Newfoundland_Spring
Plot_Crab_LatLong_Quebec<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Quebec - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Crab_LatLong_Quebec
Plot_Crab_LatLong_Maritimes<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_crab+0.001))+
  geom_point(data=subset(Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Maritimes - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Crab_LatLong_Maritimes
Plot_Crab_LatLong_Gulf<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_crab))+
  geom_point(data=subset(Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,color=chionoecetes_opilio),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Gulf - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Crab_LatLong_Gulf
Plot_Crab_LatLong_Arctic<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,fill=log(pred_crab+0.001)))+
  geom_point(data=subset(Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,color=log(chionoecetes_opilio)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Arctic - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Crab_LatLong_Arctic

ggarrange(Plot_Crab_LatLong_Quebec,Plot_Crab_LatLong_Newfoundland,Plot_Crab_LatLong_Arctic,Plot_Crab_LatLong_Gulf,Plot_Crab_LatLong_Maritimes,ncol=3,nrow=2)

#Shrimp
Plot_Shrimp_LatLong_Newfoundland_Fall<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_fall"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_shrimp))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==9|month==10|month==11|month==12|month==1|month==2)&year_surv==2005)),aes(x=longitude,y=latitude,color=pandalus_borealis),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Shrimp_LatLong_Newfoundland_Fall
Plot_Shrimp_LatLong_Newfoundland_Spring<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland_spring"&year_surv==2005&pred_shrimp),aes(x=longitude,y=latitude,fill=pred_shrimp))+
  geom_point(data=subset(Data,(region=="Newfoundland"&(month==3|month==4|month==5|month==6|month==7)&year_surv==2005)),aes(x=longitude,y=latitude,color=pandalus_borealis),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Shrimp_LatLong_Newfoundland_Spring
Plot_Shrimp_LatLong_Quebec<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_shrimp))+
  geom_point(data=subset(Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,color=pandalus_borealis),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Quebec - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Shrimp_LatLong_Quebec
Plot_Shrimp_LatLong_Maritimes<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,fill=pred_shrimp))+
  geom_point(data=subset(Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,color=pandalus_borealis),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Maritimes - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Shrimp_LatLong_Maritimes
Plot_Shrimp_LatLong_Gulf<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Gulf"&year_surv==2008),aes(x=longitude,y=latitude,fill=pred_shrimp))+
  geom_point(data=subset(Data,region=="Gulf"&year_surv==2008),aes(x=longitude,y=latitude,color=pandalus_borealis),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Gulf - 2008")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Shrimp_LatLong_Gulf
Plot_Shrimp_LatLong_Arctic<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,fill=pred_shrimp))+
  geom_point(data=subset(Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,color=pandalus_borealis),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Arctic - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Shrimp_LatLong_Arctic

ggarrange(Plot_Shrimp_LatLong_Quebec,Plot_Shrimp_LatLong_Newfoundland,Plot_Shrimp_LatLong_Arctic,Plot_Shrimp_LatLong_Gulf,Plot_Shrimp_LatLong_Maritimes,ncol=3,nrow=2)

#Planktivore
Plot_Planktivore_LatLong_Newfoundland<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_planktivores+0.001)))+
  geom_point(data=subset(Data,(region=="Newfoundland"&year_surv==2005)),aes(x=longitude,y=latitude,color=log(planktivores)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Planktivore_LatLong_Newfoundland
Plot_Planktivore_LatLong_Quebec<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_planktivores+0.001)))+
  geom_point(data=subset(Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,color=log(planktivores)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Quebec - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Planktivore_LatLong_Quebec
Plot_Planktivore_LatLong_Maritimes<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_planktivores+0.001)))+
  geom_point(data=subset(Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,color=log(planktivores)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Maritimes - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Planktivore_LatLong_Maritimes
Plot_Planktivore_LatLong_Gulf<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_planktivores+0.001)))+
  geom_point(data=subset(Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,color=log(planktivores)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Gulf - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Planktivore_LatLong_Gulf
Plot_Planktivore_LatLong_Arctic<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,fill=log(pred_planktivores)))+
  geom_point(data=subset(Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,color=log(planktivores)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Arctic - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Planktivore_LatLong_Arctic

ggarrange(Plot_Planktivore_LatLong_Quebec,Plot_Planktivore_LatLong_Newfoundland,Plot_Planktivore_LatLong_Arctic,Plot_Planktivore_LatLong_Gulf,Plot_Planktivore_LatLong_Maritimes,ncol=3,nrow=2)

#Total biomass
Plot_Totalbiomass_LatLong_Newfoundland<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Newfoundland"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_totalbiomass+0.001)))+
  geom_point(data=subset(Data,(region=="Newfoundland"&year_surv==2005)),aes(x=longitude,y=latitude,color=log(total_biomass)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Newfoundland - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Totalbiomass_LatLong_Newfoundland
Plot_Totalbiomass_LatLong_Quebec<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_totalbiomass+0.001)))+
  geom_point(data=subset(Data,region=="Quebec"&year_surv==2005),aes(x=longitude,y=latitude,color=log(total_biomass)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Quebec - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Totalbiomass_LatLong_Quebec
Plot_Totalbiomass_LatLong_Maritimes<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_totalbiomass+0.001)))+
  geom_point(data=subset(Data,region=="Maritimes"&year_surv==2005),aes(x=longitude,y=latitude,color=log(total_biomass)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Maritimes - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Totalbiomass_LatLong_Maritimes
Plot_Totalbiomass_LatLong_Gulf<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,fill=log(pred_totalbiomass+0.001)))+
  geom_point(data=subset(Data,region=="Gulf"&year_surv==2005),aes(x=longitude,y=latitude,color=log(total_biomass)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Gulf - 2005")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Totalbiomass_LatLong_Gulf
Plot_Totalbiomass_LatLong_Arctic<-ggplot()+
  geom_tile(data=subset(Predicted_Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,fill=log(pred_totalbiomass+0.001)))+
  geom_point(data=subset(Data,region=="Arctic"&year_surv==2018),aes(x=longitude,y=latitude,color=log(total_biomass)),size=0.9)+
  scale_fill_viridis_c()+
  ggtitle("Arctic - 2018")+
  scale_colour_gradient(low = "red", high = "green")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(fill="Predicted", color="Measured")+
  coord_equal()
Plot_Totalbiomass_LatLong_Arctic

ggarrange(Plot_Totalbiomass_LatLong_Quebec,Plot_Totalbiomass_LatLong_Newfoundland,Plot_Totalbiomass_LatLong_Arctic,Plot_Totalbiomass_LatLong_Gulf,Plot_Totalbiomass_LatLong_Maritimes,ncol=3,nrow=2)

