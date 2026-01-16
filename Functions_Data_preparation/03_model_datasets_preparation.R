#-------------------------------------------------------------------------------
## Loading and initializating libraries ####
library("tidyverse")
library("lme4")
library("nlme")
library("mgcv")
library("MuMIn")
library("brms")
library("MCMCglmm")
library("geosphere")
library("sjPlot")
library("lmerTest")
library("car")
library("broom.mixed")
library("effectsize")
library("ggeffects")
library("effects")
library("plyr")
## ---------------------------------------------------------------------------- ####
## ------------------------------ Data loading -------------------------------- ####
## Data loading ####
# source("Functions_Data_preparation/03_data_loading.R")

BB_metadata <- read.xlsx("Datasets/Metadata/Boubin_meta.xlsx")
ZF_metadata <- read.xlsx("Datasets/Metadata/Zofin_meta.xlsx")
RN_metadata <- read.xlsx("Datasets/Metadata/Ranspurk_meta.xlsx")
EU_metadata <- read.xlsx("Datasets/Metadata/Eustaska_meta.xlsx")

## Cut the data ####
# By uncomplete years
temp_dendrometer_data<-list()

cut.year<-2018
temp_dendrometer_data$BB_FASY_bigTrees<-subset(dendrometer_data$BB_FASY_bigTrees,Year>cut.year)
temp_dendrometer_data$BB_PCAB_bigTrees<-subset(dendrometer_data$BB_PCAB_bigTrees,Year>cut.year)
temp_dendrometer_data$BB_FASY_smallTrees<-subset(dendrometer_data$BB_FASY_smallTrees,Year>cut.year)
temp_dendrometer_data$BB_PCAB_smallTrees<-subset(dendrometer_data$BB_PCAB_smallTrees,Year>cut.year)

cut.year<-2019
temp_dendrometer_data$EU_PCAB_bigTrees<-subset(dendrometer_data$EU_PCAB_bigTrees,Year>cut.year)
temp_dendrometer_data$EU_PCAB_smallTrees<-subset(dendrometer_data$EU_PCAB_smallTrees,Year>cut.year)

cut.year<-2017
temp_dendrometer_data$ZF_FASY_bigTrees<-subset(dendrometer_data$ZF_FASY_bigTrees,Year>cut.year)
temp_dendrometer_data$ZF_PCAB_bigTrees<-subset(dendrometer_data$ZF_PCAB_bigTrees,Year>cut.year)
temp_dendrometer_data$ZF_FASY_smallTrees<-subset(dendrometer_data$ZF_FASY_smallTrees,Year>cut.year)
temp_dendrometer_data$ZF_PCAB_smallTrees<-subset(dendrometer_data$ZF_PCAB_smallTrees,Year>cut.year)

cut.year<-2018
temp_dendrometer_data$RN_ACCA_bigTrees<-subset(dendrometer_data$RN_ACCA_bigTrees,Year>cut.year)
temp_dendrometer_data$RN_ACCA_smallTrees<-subset(dendrometer_data$RN_ACCA_smallTrees,Year>cut.year)
temp_dendrometer_data$RN_CABE_bigTrees<-subset(dendrometer_data$RN_CABE_bigTrees,Year>cut.year)
temp_dendrometer_data$RN_CABE_smallTrees<-subset(dendrometer_data$RN_CABE_smallTrees,Year>cut.year)
temp_dendrometer_data$RN_ULSP_bigTrees<-subset(dendrometer_data$RN_ULSP_bigTrees,Year>cut.year)
temp_dendrometer_data$RN_ULSP_smallTrees<-subset(dendrometer_data$RN_ULSP_smallTrees,Year>cut.year)

## By CI
# CI<-0.90
CI<-0.95

low<-(1-CI)/2
high<-CI+low

dendrometer_data$BB_FASY_bigTrees    <- subset(dendrometer_data$BB_FASY_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$BB_PCAB_bigTrees    <- subset(dendrometer_data$BB_PCAB_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$EU_PCAB_bigTrees    <- subset(dendrometer_data$EU_PCAB_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_FASY_bigTrees    <- subset(dendrometer_data$ZF_FASY_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_PCAB_bigTrees    <- subset(dendrometer_data$ZF_PCAB_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ACCA_bigTrees    <- subset(dendrometer_data$RN_ACCA_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_CABE_bigTrees    <- subset(dendrometer_data$RN_CABE_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ULSP_bigTrees    <- subset(dendrometer_data$RN_ULSP_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)

dendrometer_data$BB_FASY_smallTrees  <- subset(dendrometer_data$BB_FASY_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$BB_PCAB_smallTrees  <- subset(dendrometer_data$BB_PCAB_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$EU_PCAB_smallTrees  <- subset(dendrometer_data$EU_PCAB_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_FASY_smallTrees  <- subset(dendrometer_data$ZF_FASY_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_PCAB_smallTrees  <- subset(dendrometer_data$ZF_PCAB_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ACCA_smallTrees  <- subset(dendrometer_data$RN_ACCA_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_CABE_smallTrees  <- subset(dendrometer_data$RN_CABE_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ULSP_smallTrees  <- subset(dendrometer_data$RN_ULSP_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)

# by size
DBH_limit <- 250

dendrometer_data$BB_PCAB_bigTrees[which(dendrometer_data$BB_PCAB_bigTrees$Tree %in% subset(BB_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]

dendrometer_data$BB_FASY_bigTrees <- dendrometer_data$BB_FASY_bigTrees[which(dendrometer_data$BB_FASY_bigTrees$Tree %in% subset(BB_metadata, Species == "FASY" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$BB_PCAB_bigTrees <- dendrometer_data$BB_PCAB_bigTrees[which(dendrometer_data$BB_PCAB_bigTrees$Tree %in% subset(BB_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$EU_PCAB_bigTrees <- dendrometer_data$EU_PCAB_bigTrees[which(dendrometer_data$EU_PCAB_bigTrees$Tree %in% subset(EU_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$ZF_FASY_bigTrees <- dendrometer_data$ZF_FASY_bigTrees[which(dendrometer_data$ZF_FASY_bigTrees$Tree %in% subset(ZF_metadata, Species == "FASY" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$ZF_PCAB_bigTrees <- dendrometer_data$ZF_PCAB_bigTrees[which(dendrometer_data$ZF_PCAB_bigTrees$Tree %in% subset(ZF_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$RN_ACCA_bigTrees <- dendrometer_data$RN_ACCA_bigTrees[which(dendrometer_data$RN_ACCA_bigTrees$Tree %in% subset(RN_metadata, Species == "ACCA" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$RN_CABE_bigTrees <- dendrometer_data$RN_CABE_bigTrees[which(dendrometer_data$RN_CABE_bigTrees$Tree %in% subset(RN_metadata, Species == "CABE" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$RN_ULSP_bigTrees <- dendrometer_data$RN_ULSP_bigTrees[which(dendrometer_data$RN_ULSP_bigTrees$Tree %in% subset(RN_metadata, Species == "ULSP" & DBH >= DBH_limit)$Device_ID),]

## ---------------------------------------------------------------------------- ####
## -------------------------------- Functions --------------------------------- ####
source("Functions_Data_preparation/01_base_functions.R")

## load.swp.data ####
#
# Load soil water potential (SWP) data from multiple files.
#
# Reads all files in a specified folder and loads them into R.
# All files are expected to have identical structure.
#
# Arguments:
#   data_folder   - path to the folder containing SWP data files
#   output_format - output format: "data.frame" (default) or "list"
#
# Returns:
#   A data.frame with merged SWP data or a list of data.frames.
#
load.swp.data<-function(data_folder,output_format="data.frame"){
  
  ## Testing arguments
  # data_folder="SWP_data/Boubin"
  # output_format="data.frame"
  
  # load file names of dataset for site
  files<-list.files(data_folder)
  
  # initialize output list
  output<-list()
  
  # for cycle for data loading
  for(i in files){
    # using file names to label list elements
    # print(i)
    output[[i]]<-read.xlsx(paste0(data_folder,"/",i))
  }
  
  # changing output file type if necessary
  if(output_format=="list") output<-output
  if(output_format=="data.frame"){ 
    output<-do.call(rbind.fill,output)
    rownames(output)<-c(1:nrow(output))  
  }
  
  return(output)
}

## prepare.swc ####
#
# Prepare soil water content (SWC) data for a selected soil depth.
#
# Extracts SWP measurements for a given depth and computes
# mean soil water content across sensors.
#
# Arguments:
#   swp.dta.input - data.frame with raw SWP data
#   depth         - soil depth in cm (default = 10)
#
# Returns:
#   A data.frame with Year, DOY, Hour, and averaged SWC values.
#
prepare.swc<-function(swp.dta.input, depth=10){
  
  ## Testing arguments
  # swp.dta.input=load.swp.data("SWP_data/Eustaska")
  # depth=10
  
  # swp.dta.input<-swp.dta.input[,c(2,3,6,which(substr(names(swp.dta.input),12,16)==paste0(depth,"cm")))]
  swp.dta.input<-swp.dta.input[,c(2,3,6,which(str_sub(names(swp.dta.input), -4)==paste0(depth,"cm")))]
  
  output<-swp.dta.input[,c("Year","DOY","Hour")]
  output$SWC<-rowMeans(swp.dta.input[,c(4:ncol(swp.dta.input))],na.rm=T)
  
  return(output)
}

## connect.swc ####
#
# Merge soil water content data with tree dendrometer data.
#
# Joins SWC data to tree-level data based on Year, DOY, and Hour.
#
# Arguments:
#   input.swc       - prepared SWC data.frame
#   input.tree.data - dendrometer data.frame
#
# Returns:
#   A merged data.frame containing tree and SWC data.
#
connect.swc<-function(input.swc, input.tree.data){
  
  ## Testing arguments
  # input.swp=prepare.swc(load.swp.data("SWP_data/Boubin"))
  # input.tree.data=dendrometer_data$BB_FASY_bigTrees
  
  to.merge.tree.data<-input.tree.data[,c("Tree", "Year", "DOY", "Hour")]
  
  merged<-merge(input.tree.data, input.swc, by = c("Year","DOY", "Hour"))
  
  # output<-input.tree.data
  # output$SWP<-merged$SWC
  return(merged)
}

## add.station.climate ####
#
# Add station-based climate data to tree dendrometer data.
#
# Joins tree-level data with climate station data using
# Year, DOY, and Hour.
#
# Arguments:
#   tree.data - dendrometer data.frame
#   clim.data - climate station data.frame
#
# Returns:
#   A data.frame with tree data extended by climate variables.
#
add.station.climate<-function(tree.data, clim.data){
  
  ## Testing arguments
  # tree.data=dendrometer_data$EU_PCAB_bigTrees
  # clim.data=climate_data$Eustaska_stationclim
  
  tree.data$time<-paste0(tree.data$Year,"_",tree.data$DOY,"_",tree.data$Hour)
  clim.data$time<-paste0(clim.data$Year,"_",clim.data$DOY,"_",clim.data$Hour)
  
  # joined.df<-inner.join(tree.data, clim.data, "time")
  
  joined.df<- tree.data %>% inner_join(clim.data, by='time')
  joined.df<-na.omit(joined.df)
  
  joined.df<-within(joined.df, rm("Time", "Year.y", "DOY.y", "Month.y", "Day.y", "Hour.y", "Minute.y"))
  
  names(joined.df)<-gsub(".x","",names(joined.df))
  
  return(joined.df)
}

## add.podrost.climate ####
#
# Add understory (forest interior) climate data to tree data.
#
# Computes mean understory temperature, humidity, and VPD,
# and merges them with tree-level dendrometer data.
#
# Arguments:
#   tree.data - dendrometer data.frame
#   clim.data - understory climate data.frame
#
# Returns:
#   A data.frame with tree data extended by understory climate variables.
#
add.podrost.climate<-function(tree.data, clim.data){
  
  ## Testing arguments
  tree.data=dendrometer_data$BB_PCAB_bigTrees
  clim.data=climate_data_forest$podrost_BB
  
  if(ncol(clim.data)==9){
    names(clim.data)<-c("Time","Year","DOY","Month","Day","Hour","Minute","T_forest", "H_forest")
  } else {
    clim.data$T_forest<-rowMeans(clim.data[,which(substr(names(clim.data),1,2)=="T_")],na.rm=T)
    clim.data$H_forest<-rowMeans(clim.data[,which(substr(names(clim.data),1,2)=="H_")],na.rm=T)
    clim.data<-clim.data[,c("Time","Year","DOY","Month","Day","Hour","Minute","T_forest", "H_forest")]
  }
  
  clim.data$VPD_forest<-NA
  es<-0.6108*exp((17.27*clim.data$T_forest)/(clim.data$T_forest+273.3))
  ea<-es*(clim.data$H_forest/100)
  clim.data$VPD_forest<-es-ea
  
  tree.data$time<-paste0(tree.data$Year,"_",tree.data$DOY,"_",tree.data$Hour)
  clim.data$time<-paste0(clim.data$Year,"_",clim.data$DOY,"_",clim.data$Hour)
  
  # joined.df<-inner.join(tree.data, clim.data, "time")
  
  joined.df<- tree.data %>% inner_join(clim.data, by='time')
  joined.df<-na.omit(joined.df)
  
  joined.df<-within(joined.df, rm("Time", "Year.y", "DOY.y", "Month.y", "Day.y", "Hour.y", "Minute.y"))
  
  names(joined.df)<-gsub(".x","",names(joined.df))
  
  return(joined.df)
}

## add.podrost.climate.simulated ####
#
# Add simulated understory climate variables to tree data.
#
# Uses linear models derived from station and understory climate
# data to simulate forest interior conditions.
#
# Arguments:
#   tree.data          - dendrometer data.frame
#   clim.data          - station climate data.frame
#   clim.data.podrost  - understory climate data.frame
#
# Returns:
#   A data.frame with tree data and simulated understory climate variables.
#
add.podrost.climate.simulated<-function(tree.data, clim.data, clim.data.podrost){
  
  ## Testing arguments
  # tree.data=dendrometer_data$ZF_PCAB_bigTrees
  # clim.data=climate_data$Zofin_stationclim
  # clim.data.podrost=climate_data_forest$podrost_ZF
  
  if(ncol(clim.data.podrost)==9){
    names(clim.data.podrost)<-c("Time","Year","DOY","Month","Day","Hour","Minute","T_forest", "H_forest")
  } else {
    clim.data.podrost$T_forest<-rowMeans(clim.data.podrost[,which(substr(names(clim.data.podrost),1,2)=="T_")],na.rm=T)
    clim.data.podrost$H_forest<-rowMeans(clim.data.podrost[,which(substr(names(clim.data.podrost),1,2)=="H_")],na.rm=T)
    clim.data.podrost<-clim.data.podrost[,c("Time","Year","DOY","Month","Day","Hour","Minute","T_forest", "H_forest")]
  }
  
  clim.data.podrost$VPD_forest<-NA
  es<-0.6108*exp((17.27*clim.data.podrost$T_forest)/(clim.data.podrost$T_forest+273.3))
  ea<-es*(clim.data.podrost$H_forest/100)
  clim.data.podrost$VPD_forest<-es-ea
  
  tree.data$time<-paste0(tree.data$Year,"_",tree.data$DOY,"_",tree.data$Hour)
  clim.data$time<-paste0(clim.data$Year,"_",clim.data$DOY,"_",clim.data$Hour)
  clim.data.podrost$time<-paste0(clim.data.podrost$Year,"_",clim.data.podrost$DOY,"_",clim.data.podrost$Hour)
  
  clim.data.model<-clim.data %>% inner_join(clim.data.podrost, by='time')
  clim.data.model<-within(clim.data.model, rm("Year.y","DOY.y","Hour.y"))
  names(clim.data.model)<-gsub(".x","",names(clim.data.model))
  
  lm.t<-lm(T_forest~T,data=clim.data.model)
  lm.h<-lm(H_forest~H,data=clim.data.model)
  lm.vpd<-lm(VPD_forest~VPD,data=clim.data.model)
  
  
  # joined.df<-inner.join(tree.data, clim.data, "time")
  
  joined.df<- tree.data %>% inner_join(clim.data, by='time')
  joined.df<-na.omit(joined.df)
  joined.df<-within(joined.df, rm("Time", "Year.y", "DOY.y", "Month.y", "Day.y", "Hour.y", "Minute.y"))
  names(joined.df)<-gsub(".x","",names(joined.df))
  
  joined.df$T_forest_sim<-lm.t[["coefficients"]][["(Intercept)"]]+joined.df$T*lm.t[["coefficients"]][["T"]]
  joined.df$H_forest_sim<-lm.h[["coefficients"]][["(Intercept)"]]+joined.df$H*lm.h[["coefficients"]][["H"]]
  joined.df$VPD_forest_sim<-lm.vpd[["coefficients"]][["(Intercept)"]]+joined.df$VPD*lm.vpd[["coefficients"]][["VPD"]]
  
  return(joined.df)
}

## add.data.simple ####
#
# Create a basic analysis dataset combining tree and climate data.
#
# Adds station climate data, daylength, and growth phase classification.
#
# Arguments:
#   tree.data - dendrometer data.frame
#   clim.data - station climate data.frame
#   lat       - site latitude (decimal degrees)
#
# Returns:
#   A data.frame ready for basic growth analysis.
#
add.data.simple<-function(tree.data, clim.data, lat){
  
  ## Testing arguments
  # tree.data=dendrometer_data$EU_PCAB_bigTrees
  # clim.data=climate_data$Eustaska_stationclim
  # lat=50.0592
  
  output<-add.station.climate(tree.data, clim.data)
  output$daylength<-daylength(lat, output$DOY)
  output<-determine.growth.phase(output)
  output$phase2<-0
  output$phase2[which(output$phase=="Growth")]<-1
  
  # output<-na.omit(output)
  
  return(output)
}

## add.data ####
#
# Create an extended analysis dataset including understory climate.
#
# Combines tree data with station climate, understory climate,
# daylength, and growth phase classification.
#
# Arguments:
#   tree.data        - dendrometer data.frame
#   clim.data        - station climate data.frame
#   clim.data.podrost- understory climate data.frame
#   lat              - site latitude (decimal degrees)
#
# Returns:
#   A data.frame ready for advanced growth analysis.
#
add.data<-function(tree.data, clim.data, clim.data.podrost, lat){
  
  ## Testing arguments
  # tree.data=dendrometer_data$BB_PCAB_bigTrees
  # clim.data=climate_data$Boubin_stationclim
  # clim.data.podrost=climate_data_forest$podrost_BB
  # lat=50
  
  output<-add.station.climate(tree.data, clim.data)
  output<-add.podrost.climate(output, clim.data.podrost)
  output$daylength<-daylength(lat, output$DOY)
  output<-determine.growth.phase(output)
  output$phase2<-0
  output$phase2[which(output$phase=="Growth")]<-1
  
  # output<-na.omit(output)
  
  return(output)
}

## complete.data ####
#
# Compile complete dendrometer datasets for all sites (station climate only).
#
# Applies basic data preparation to all sites and tree categories.
#
# Arguments:
#   in.tree.data - list of dendrometer datasets
#   in.clim.data - list of climate station datasets
#
# Returns:
#   A list of fully prepared data.frames for all sites.
#
complete.data<-function(in.tree.data, in.clim.data){
  
  ## Testing arguments
  # in.tree.data=dendrometer_data
  # in.clim.data=climate_data
  
  out<-list()
  
  ## Boubin
  out$BB_FASY_bigTrees<-add.data.simple(in.tree.data$BB_FASY_bigTrees, 
                                        climate_data$Boubin_stationclim, 
                                        48.9911)
  out$BB_FASY_smallTrees<-add.data.simple(in.tree.data$BB_FASY_smallTrees, 
                                          climate_data$Boubin_stationclim, 
                                          48.9911)
  out$BB_PCAB_bigTrees<-add.data.simple(in.tree.data$BB_PCAB_bigTrees, 
                                        climate_data$Boubin_stationclim, 
                                        48.9911)
  out$BB_PCAB_smallTrees<-add.data.simple(in.tree.data$BB_PCAB_smallTrees, 
                                          climate_data$Boubin_stationclim, 
                                          48.9911)
  
  ## Eustaska
  out$EU_PCAB_bigTrees<-add.data.simple(in.tree.data$EU_PCAB_bigTrees, 
                                        climate_data$Eustaska_stationclim, 
                                        50.0592)
  out$EU_PCAB_smallTrees<-add.data.simple(in.tree.data$EU_PCAB_smallTrees, 
                                          climate_data$Eustaska_stationclim, 
                                          50.0592)
  
  ## Ranspurk
  out$RN_ACCA_bigTrees<-add.data.simple(in.tree.data$RN_ACCA_bigTrees, 
                                        climate_data$Ranspurk_stationclim, 
                                        48.6783)
  out$RN_ACCA_smallTrees<-add.data.simple(in.tree.data$RN_ACCA_smallTrees, 
                                          climate_data$Ranspurk_stationclim, 
                                          48.6783)
  out$RN_CABE_bigTrees<-add.data.simple(in.tree.data$RN_CABE_bigTrees, 
                                        climate_data$Ranspurk_stationclim, 
                                        48.6783)
  out$RN_CABE_smallTrees<-add.data.simple(in.tree.data$RN_CABE_smallTrees, 
                                          climate_data$Ranspurk_stationclim, 
                                          48.6783)
  out$RN_ULSP_bigTrees<-add.data.simple(in.tree.data$RN_ULSP_bigTrees, 
                                        climate_data$Ranspurk_stationclim, 
                                        48.6783)
  out$RN_ULSP_smallTrees<-add.data.simple(in.tree.data$RN_ULSP_smallTrees, 
                                          climate_data$Ranspurk_stationclim, 
                                          48.6783)
  
  ## Zofin
  out$ZF_FASY_bigTrees<-add.data.simple(in.tree.data$ZF_FASY_bigTrees, 
                                        climate_data$Zofin_stationclim, 
                                        48.6666)
  out$ZF_FASY_smallTrees<-add.data.simple(in.tree.data$ZF_FASY_smallTrees, 
                                          climate_data$Zofin_stationclim, 
                                          48.6666)
  out$ZF_PCAB_bigTrees<-add.data.simple(in.tree.data$ZF_PCAB_bigTrees, 
                                        climate_data$Zofin_stationclim, 
                                        48.6666)
  out$ZF_PCAB_smallTrees<-add.data.simple(in.tree.data$ZF_PCAB_smallTrees, 
                                          climate_data$Zofin_stationclim, 
                                          48.6666)
  
  return(out)
  
}

## complete.data.podrost ####
#
# Compile complete dendrometer datasets including understory climate.
#
# Prepares datasets for all sites using measured forest interior
# climate data.
#
# Arguments:
#   in.tree.data          - list of dendrometer datasets
#   in.clim.data         - list of station climate datasets
#   in.clim.data.podrost - list of understory climate datasets
#
# Returns:
#   A list of fully prepared data.frames with understory climate.
#
complete.data.podrost<-function(in.tree.data, in.clim.data, in.clim.data.podrost){
  
  ## Testing arguments
  # in.tree.data=dendrometer_data
  # in.clim.data=climate_data
  # in.clim.data.podrost=climate_data_forest
  
  out<-list()
  
  ## Boubin
  out$BB_FASY_smallTrees<-add.data(in.tree.data$BB_FASY_smallTrees, 
                                   climate_data$Boubin_stationclim, 
                                   in.clim.data.podrost$understory_BB, 
                                   48.9911)
  out$BB_PCAB_smallTrees<-add.data(in.tree.data$BB_PCAB_smallTrees, 
                                   climate_data$Boubin_stationclim, 
                                   in.clim.data.podrost$understory_BB,
                                   48.9911)
  
  ## Eustaska
  out$EU_PCAB_smallTrees<-add.data(in.tree.data$EU_PCAB_smallTrees, 
                                   climate_data$Eustaska_stationclim, 
                                   in.clim.data.podrost$understory_EU,
                                   50.0592)
  
  ## Ranspurk
  out$RN_ACCA_smallTrees<-add.data(in.tree.data$RN_ACCA_smallTrees, 
                                   climate_data$Ranspurk_stationclim, 
                                   in.clim.data.podrost$understory_RN,
                                   48.6783)
  out$RN_CABE_smallTrees<-add.data(in.tree.data$RN_CABE_smallTrees, 
                                   climate_data$Ranspurk_stationclim, 
                                   in.clim.data.podrost$understory_RN,
                                   48.6783)
  out$RN_ULSP_smallTrees<-add.data(in.tree.data$RN_ULSP_smallTrees, 
                                   climate_data$Ranspurk_stationclim, 
                                   in.clim.data.podrost$understory_RN,
                                   48.6783)
  
  ## Zofin
  out$ZF_FASY_smallTrees<-add.data(in.tree.data$ZF_FASY_smallTrees, 
                                   climate_data$Zofin_stationclim, 
                                   in.clim.data.podrost$understory_ZF,
                                   48.6666)
  out$ZF_PCAB_smallTrees<-add.data(in.tree.data$ZF_PCAB_smallTrees, 
                                   climate_data$Zofin_stationclim, 
                                   in.clim.data.podrost$understory_ZF,
                                   48.6666)
  
  return(out)
  
}


## ---------------------------------------------------------------------------- ####
## -------------------------- Datasets preparations --------------------------- ####
## Prepare basic dataset ####
dendrometer_data_4model<-temp_dendrometer_data

dendrometer_data_4model$EU_PCAB_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Eustaska")),
                                                      dendrometer_data_4model$EU_PCAB_bigTrees)
dendrometer_data_4model$EU_PCAB_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Eustaska")),
                                                        dendrometer_data_4model$EU_PCAB_smallTrees)

dendrometer_data_4model$BB_PCAB_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Boubin")),
                                                      dendrometer_data_4model$BB_PCAB_bigTrees)
dendrometer_data_4model$BB_PCAB_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Boubin")),
                                                        dendrometer_data_4model$BB_PCAB_smallTrees)
dendrometer_data_4model$BB_FASY_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Boubin")),
                                                      dendrometer_data_4model$BB_FASY_bigTrees)
dendrometer_data_4model$BB_FASY_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Boubin")),
                                                        dendrometer_data_4model$BB_FASY_smallTrees)

dendrometer_data_4model$ZF_PCAB_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Zofin")),
                                                      dendrometer_data_4model$ZF_PCAB_bigTrees)
dendrometer_data_4model$ZF_PCAB_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Zofin")),
                                                        dendrometer_data_4model$ZF_PCAB_smallTrees)
dendrometer_data_4model$ZF_FASY_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Zofin")),
                                                      dendrometer_data_4model$ZF_FASY_bigTrees)
dendrometer_data_4model$ZF_FASY_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Zofin")),
                                                        dendrometer_data_4model$ZF_FASY_smallTrees)

dendrometer_data_4model$RN_ACCA_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                      dendrometer_data_4model$RN_ACCA_bigTrees)
dendrometer_data_4model$RN_ACCA_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                        dendrometer_data_4model$RN_ACCA_smallTrees)
dendrometer_data_4model$RN_CABE_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                      dendrometer_data_4model$RN_CABE_bigTrees)
dendrometer_data_4model$RN_CABE_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                        dendrometer_data_4model$RN_CABE_smallTrees)
dendrometer_data_4model$RN_ULSP_bigTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                      dendrometer_data_4model$RN_ULSP_bigTrees)
dendrometer_data_4model$RN_ULSP_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                        dendrometer_data_4model$RN_ULSP_smallTrees)

model_dataset<-complete.data(dendrometer_data_4model, climate_data)

## Prepare understory datasets ####
dendrometer_data_4model<-dendrometer_data[c("BB_FASY_smallTrees",
                                            "BB_PCAB_smallTrees",
                                            "EU_PCAB_smallTrees",
                                            "RN_ACCA_smallTrees",
                                            "RN_CABE_smallTrees",
                                            "RN_ULSP_smallTrees",  
                                            "ZF_FASY_smallTrees",
                                            "ZF_PCAB_smallTrees")]

dendrometer_data_4model$EU_PCAB_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Eustaska")),
                                                        dendrometer_data_4model$EU_PCAB_smallTrees)

dendrometer_data_4model$BB_PCAB_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Boubin")),
                                                        dendrometer_data_4model$BB_PCAB_smallTrees)
dendrometer_data_4model$BB_FASY_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Boubin")),
                                                        dendrometer_data_4model$BB_FASY_smallTrees)

dendrometer_data_4model$ZF_PCAB_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Zofin")),
                                                        dendrometer_data_4model$ZF_PCAB_smallTrees)
dendrometer_data_4model$ZF_FASY_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Zofin")),
                                                        dendrometer_data_4model$ZF_FASY_smallTrees)

dendrometer_data_4model$RN_ACCA_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                        dendrometer_data_4model$RN_ACCA_smallTrees)
dendrometer_data_4model$RN_CABE_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                        dendrometer_data_4model$RN_CABE_smallTrees)
dendrometer_data_4model$RN_ULSP_smallTrees<-connect.swc(prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk")),
                                                        dendrometer_data_4model$RN_ULSP_smallTrees)


model_dataset_podrost<-complete.data.podrost(dendrometer_data_4model, climate_data, climate_data_forest)
