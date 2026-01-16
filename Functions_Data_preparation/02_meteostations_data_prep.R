#-------------------------------------------------------------------------------
## Loading and initializating libraries ####
library("openxlsx")
library("reshape")
library("runner")
library("plyr")
library("tidyverse")
library("pvldcurve")
library("myClim")
#-------------------------------------------------------------------------------
## Loading and initializating functions ####
source("Functions_Data_preparation/01_base_functions.R")

## calc.VPD ####
#
# Calculate vapor pressure deficit (VPD).
#
# Computes vapor pressure deficit from air temperature and
# relative humidity using standard saturation vapor pressure
# equations.
#
# Arguments:
#   clim.data - data.frame containing climate data with columns:
#               T : air temperature (°C)
#               H : relative humidity (%)
#
# Output is a numeric vector of vapor pressure deficit (VPD) values (kPa).
#
calc.VPD<-function(clim.data){
  
  ## Testing arguments
  # clim.data=dataset.small$RN_ACCA_smallTrees
  
  es<-0.6108*exp((17.27*clim.data$T)/(clim.data$T+273.3))
  ea<-es*(clim.data$H/100)
  VPD<-es-ea
  
  return(VPD)
}
#-------------------------------------------------------------------------------
# Boubin ####
site_name<-"Boubin"
data_folder<-paste0("Datasets/Meteo_data/",site_name)

clim.data<-load.dendrometer.data(data_folder)
clim.data<-clim.data[which(clim.data$Minute==0),]
clim.data$VPD<-calc.VPD(clim.data)
clim.data$Time<-site_name

names(clim.data)<-c("Site","DOY","Year","Month","Day","Hour","Minute","R","T","H","P","VPD")
clim.data<-clim.data[,c("Site","Year","DOY","Hour","R","T","H","P","VPD")]

write.xlsx(clim.data,paste0("Datasets_recalculated/Recalculated_stationclimate/",site_name,"_stationclim.xlsx"))

#-------------------------------------------------------------------------------
# Eustaska ####
site_name<-"Eustaska"
data_folder<-paste0("Datasets/Meteo_data/",site_name)

clim.data<-load.dendrometer.data(data_folder)
clim.data<-clim.data[which(clim.data$Minute==0),]
clim.data$VPD<-calc.VPD(clim.data)
clim.data$Time<-site_name

names(clim.data)<-c("Site","DOY","Year","Month","Day","Hour","Minute","R","T","H","P","VPD")
clim.data<-clim.data[,c("Site","Year","DOY","Hour","R","T","H","P","VPD")]

write.xlsx(clim.data,paste0("Datasets_recalculated/Recalculated_stationclimate/",site_name,"_stationclim.xlsx"))

#-------------------------------------------------------------------------------
# Ranspurk ####
site_name<-"Ranspurk"
data_folder<-paste0("Datasets/Meteo_data/",site_name)

clim.data<-load.dendrometer.data(data_folder)
clim.data<-clim.data[which(clim.data$Minute==0),]
clim.data$VPD<-calc.VPD(clim.data)
clim.data$Time<-site_name

names(clim.data)<-c("Site","Year","DOY","Month","Day","Hour","Minute","R","T","H","P","VPD")
clim.data<-clim.data[,c("Site","Year","DOY","Hour","R","T","H","P","VPD")]

write.xlsx(clim.data,paste0("Datasets_recalculated/Recalculated_stationclimate/",site_name,"_stationclim.xlsx"))

#-------------------------------------------------------------------------------
# Zofin ####
site_name<-"Zofin"
data_folder<-paste0("Datasets/Meteo_data/",site_name)

clim.data<-load.dendrometer.data(data_folder)
clim.data<-clim.data[which(clim.data$Minute==0),]
clim.data$VPD<-calc.VPD(clim.data)
clim.data$Time<-site_name

names(clim.data)<-c("Site","DOY","Year","Month","Day","Hour","Minute","R","T","H","P","VPD")
clim.data<-clim.data[,c("Site","Year","DOY","Hour","R","T","H","P","VPD")]

write.xlsx(clim.data,paste0("Datasets_recalculated/Recalculated_stationclimate/",site_name,"_stationclim.xlsx"))