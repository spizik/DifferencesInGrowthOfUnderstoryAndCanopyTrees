#-------------------------------------------------------------------------------
## Loading and initializating libraries ####
library("openxlsx")
library("reshape")
library("runner")
library("plyr")
library("ggplot2")
library("suncalc")

#-------------------------------------------------------------------------------
## Loading and initializating functions ####
source("Functions_Data_preparation/01_base_functions.R")

## update.site.meta ####
#
# Update DBH values in site metadata using dendrometer input data.
# Valid only for small trees
#
# For each device ID present both in the site metadata and in the input
# list, the function extracts the most recent (last non-NA) value and
# updates the DBH field in site_metadata.
#
# arguments:
#   input - named list of numeric vectors (or time series), where names
#           correspond to Device_ID values in site_metadata
#
# Output is updated site_metadata data.frame with modified DBH values.
#
update.site.meta<-function(input){
  
  ## Testing arguments
  # input=small_trees
  
  for(i in site_metadata$Device_ID[site_metadata$Device_ID %in% names(input)]){
    
    ind_meta <- which(site_metadata$Device_ID == i)
    
    x <- input[[i]]
    x <- x[!is.na(x)]      # keeps only valid numbers
    
    site_metadata$DBH[ind_meta] <- tail(x, 1)
  }
  
  return(site_metadata)
}
old.update.site.meta<-function(input){
  
  # input=small_trees
  
  last_day_of_small_trees<-input[((nrow(input)-23):nrow(input)),]
  
  for(i in site_metadata$Device_ID[which(site_metadata$Device_ID %in% names(last_day_of_small_trees))]){
    ind_meta<-which(site_metadata$Device_ID==i)  
    ind_file<-which(names(last_day_of_small_trees)==i)
    
    site_metadata$DBH[ind_meta]<-min(last_day_of_small_trees[,i],na.rm=T)
  }
  
  return(site_metadata)
}

## Commonv variables ####
ci_growing_season<-c(0.025,0.975)

#-------------------------------------------------------------------------------
## Eustaška ####
data_folder<-"Datasets/Dendrometer_data/Eustaska"
site_metadata<-read.xlsx("Datasets/Metadata/Eustaska_meta.xlsx")
site_junk_trees<-read.xlsx("Datasets/Junk_data/junk_Eustaska.xlsx")
DBH_measurement_year<-2021
DBH_measurement_DOY<-300
latitude<-50.068
longitude<-17.261

site_data<-load.dendrometer.data(data_folder=data_folder,
                                 output_format="data.frame")


names_trees<-subset(site_metadata,DendrometerType=="DLR26C")$Device_ID[which(subset(site_metadata,DendrometerType=="DLR26C")$Device_ID %in% names(site_data))]  
big_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

names_trees<-subset(site_metadata,DendrometerType=="PTi")$Device_ID[which(subset(site_metadata,DendrometerType=="PTi")$Device_ID %in% names(site_data))]
small_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

big_trees<-subset(big_trees, Minute==0)
small_trees<-subset(small_trees, Minute==0)

site_metadata<-update.site.meta(small_trees)


big_trees<-cut.wrong.trees(stem_data=big_trees,
                           meta_data=site_metadata,
                           junk_trees=site_junk_trees,
                           dendrometer_type="DLR26C")

small_trees_growing<-cut.wrong.trees(stem_data=small_trees,
                                     meta_data=site_metadata,
                                     junk_trees=site_junk_trees,
                                     dendrometer_type="PTi")

big_trees<-correct.curves(big_trees, min.diff=1.0)
small_trees_growing<-correct.curves(small_trees_growing, min.diff=0.25)

big_trees<-create.main.data.file(stem_data=big_trees,
                                 meta_data=site_metadata,
                                 DBH_year=DBH_measurement_year,
                                 DBH_DOY=DBH_measurement_DOY)

small_trees_growing<-create.main.data.file(stem_data=small_trees_growing,
                                           meta_data=site_metadata,
                                           DBH_year=small_trees$Year[nrow(small_trees)],
                                           DBH_DOY=small_trees$DOY[nrow(small_trees)])

## Creates the main data file
big_trees<-calculate_normalized_data(input=big_trees, gs_file=site_junk_trees)
small_trees_growing<-calculate_normalized_data(input=small_trees_growing, gs_file=site_junk_trees)

## cutting confidence interval
big_trees<-subset(big_trees, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))
small_trees_growing<-subset(small_trees_growing, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))

## deleting the first year
big_trees<-subset(big_trees,Year>=2020)
small_trees_growing<-subset(small_trees_growing,Year>=2020)

## rozliseni rustovych fazi
big_trees<-determine.growth.phase(big_trees)
small_trees_growing<-determine.growth.phase(small_trees_growing)

## rozliseni denniho cyklu
big_trees<-determine.day.phase(big_trees,latitude,longitude)
small_trees_growing<-determine.day.phase(small_trees_growing,latitude,longitude)

## sorting data
EU_PCAB_big<-big_trees
EU_PCAB_small_growing<-small_trees_growing

## cleaning the last mistakes
EU_PCAB_big<-na.omit(EU_PCAB_big)
EU_PCAB_small_growing<-na.omit(EU_PCAB_small_growing)

#-------------------------------------------------------------------------------
## Boubín ####
data_folder<-"Datasets/Dendrometer_data/Boubin"
site_metadata<-read.xlsx("Datasets/Metadata/Boubin_meta.xlsx")
site_junk_trees<-read.xlsx("Datasets/Junk_data/junk_Boubin.xlsx")
DBH_measurement_year<-2021
DBH_measurement_DOY<-307
ci_growing_season<-c(0.025,0.975)
latitude<-48.977
longitude<-13.812
  
site_data<-load.dendrometer.data(data_folder=data_folder,
                                 output_format="data.frame")

names_trees<-subset(site_metadata,DendrometerType=="DLR26C")$Device_ID[which(subset(site_metadata,DendrometerType=="DLR26C")$Device_ID %in% names(site_data))]  
big_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

names_trees<-subset(site_metadata,DendrometerType=="PTi")$Device_ID[which(subset(site_metadata,DendrometerType=="PTi")$Device_ID %in% names(site_data))]
small_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

big_trees<-subset(big_trees, Minute==0)
small_trees<-subset(small_trees, Minute==0)

site_metadata<-update.site.meta(small_trees)


big_trees<-cut.wrong.trees(stem_data=big_trees,
                           meta_data=site_metadata,
                           junk_trees=site_junk_trees,
                           dendrometer_type="DLR26C")

small_trees_growing<-cut.wrong.trees(stem_data=small_trees,
                                     meta_data=site_metadata,
                                     junk_trees=site_junk_trees,
                                     dendrometer_type="PTi")

big_trees<-correct.curves(big_trees, min.diff=1.0)
small_trees_growing<-correct.curves(small_trees_growing, min.diff=0.25)

big_trees<-create.main.data.file(stem_data=big_trees,
                                 meta_data=site_metadata,
                                 DBH_year=DBH_measurement_year,
                                 DBH_DOY=DBH_measurement_DOY)

small_trees_growing<-create.main.data.file(stem_data=small_trees_growing,
                                           meta_data=site_metadata,
                                           DBH_year=small_trees$Year[nrow(small_trees)],
                                           DBH_DOY=small_trees$DOY[nrow(small_trees)])

## Creates the main data file
big_trees<-calculate_normalized_data(input=big_trees, gs_file=site_junk_trees)
small_trees_growing<-calculate_normalized_data(input=small_trees_growing, gs_file=site_junk_trees)

## cutting confidence interval
big_trees<-subset(big_trees, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))
small_trees_growing<-subset(small_trees_growing, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))

## deleting the first year
big_trees<-subset(big_trees,Year>=2019)
small_trees_growing<-subset(small_trees_growing,Year>=2019)

## rozliseni rustovych fazi
big_trees<-determine.growth.phase(big_trees)
small_trees_growing<-determine.growth.phase(small_trees_growing)

## rozliseni denniho cyklu
big_trees<-determine.day.phase(big_trees,latitude,longitude)
small_trees_growing<-determine.day.phase(small_trees_growing,latitude,longitude)

## sorting data
BB_PCAB_big<-subset(big_trees,Species=="PCAB")
BB_PCAB_small_growing<-subset(small_trees_growing,Species=="PCAB")

BB_FASY_big<-subset(big_trees,Species=="FASY")
BB_FASY_small_growing<-subset(small_trees_growing,Species=="FASY")

## cleanig the last mistakes
BB_PCAB_big<-na.omit(BB_PCAB_big)
BB_PCAB_small_growing<-na.omit(BB_PCAB_small_growing)

BB_FASY_big<-na.omit(BB_FASY_big)
BB_FASY_small_growing<-na.omit(BB_FASY_small_growing)

#-------------------------------------------------------------------------------
# Žofín ####
data_folder<-"Datasets/Dendrometer_data/Zofin"
site_metadata<-read.xlsx("Datasets/Metadata/Zofin_meta.xlsx")
site_junk_trees<-read.xlsx("Datasets/Junk_data/junk_Zofin.xlsx")
DBH_measurement_year<-2021
DBH_measurement_DOY<-308 
latitude<-48.666
longitude<-14.706

site_data<-load.dendrometer.data(data_folder=data_folder,
                                 output_format="data.frame")

names_trees<-subset(site_metadata,DendrometerType=="DLR26C")$Device_ID[which(subset(site_metadata,DendrometerType=="DLR26C")$Device_ID %in% names(site_data))]  
big_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

names_trees<-subset(site_metadata,DendrometerType=="PTi")$Device_ID[which(subset(site_metadata,DendrometerType=="PTi")$Device_ID %in% names(site_data))]
small_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

big_trees<-subset(big_trees, Minute==0)
small_trees<-subset(small_trees, Minute==0)

site_metadata<-update.site.meta(small_trees)


big_trees<-cut.wrong.trees(stem_data=big_trees,
                           meta_data=site_metadata,
                           junk_trees=site_junk_trees,
                           dendrometer_type="DLR26C")

small_trees_growing<-cut.wrong.trees(stem_data=small_trees,
                                     meta_data=site_metadata,
                                     junk_trees=site_junk_trees,
                                     dendrometer_type="PTi")

big_trees<-correct.curves(big_trees, min.diff=1.0)
small_trees_growing<-correct.curves(small_trees_growing, min.diff=0.25)

big_trees<-create.main.data.file(stem_data=big_trees,
                                 meta_data=site_metadata,
                                 DBH_year=DBH_measurement_year,
                                 DBH_DOY=DBH_measurement_DOY)

small_trees_growing<-create.main.data.file(stem_data=small_trees_growing,
                                           meta_data=site_metadata,
                                           DBH_year=small_trees$Year[nrow(small_trees)],
                                           DBH_DOY=small_trees$DOY[nrow(small_trees)])

## Creates the main data file
big_trees<-calculate_normalized_data(input=big_trees, gs_file=site_junk_trees)
small_trees_growing<-calculate_normalized_data(input=small_trees_growing, gs_file=site_junk_trees)

## cutting confidence interval
big_trees<-subset(big_trees, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))
small_trees_growing<-subset(small_trees_growing, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))

# deleting the first year
big_trees<-subset(big_trees,Year>=2018)
small_trees_growing<-subset(small_trees_growing,Year>=2018)

## rozliseni rustovych fazi
big_trees<-determine.growth.phase(big_trees)
small_trees_growing<-determine.growth.phase(small_trees_growing)

## rozliseni denniho cyklu
big_trees<-determine.day.phase(big_trees,latitude,longitude)
small_trees_growing<-determine.day.phase(small_trees_growing,latitude,longitude)

## sorting data
ZF_PCAB_big<-subset(big_trees,Species=="PCAB")
ZF_PCAB_small_growing<-subset(small_trees_growing,Species=="PCAB")

ZF_FASY_big<-subset(big_trees,Species=="FASY")
ZF_FASY_small_growing<-subset(small_trees_growing,Species=="FASY")

## cleanig the last mistakes
ZF_PCAB_big<-na.omit(ZF_PCAB_big)
ZF_PCAB_small_growing<-na.omit(ZF_PCAB_small_growing)

ZF_FASY_big<-na.omit(ZF_FASY_big)
ZF_FASY_small_growing<-na.omit(ZF_FASY_small_growing)

#-------------------------------------------------------------------------------
# Ranšpurk ####
data_folder<-"Datasets/Dendrometer_data/Ranspurk"
site_metadata<-read.xlsx("Datasets/Metadata/Ranspurk_meta.xlsx")
site_junk_trees<-read.xlsx("Datasets/Junk_data/junk_Ranspurk.xlsx")
DBH_measurement_year<-2021
DBH_measurement_DOY<-313 
latitude<-48.678
longitude<-16.947

site_data<-load.dendrometer.data(data_folder=data_folder,
                                 output_format="data.frame")

site_junk_trees$Tree_ID<-paste0(substr(site_junk_trees$Tree_ID,1,4), substr(site_junk_trees$Tree_ID,8,20))
names(site_data)<-c(names(site_data)[1:7], paste0(substr(names(site_data)[8:length(names(site_data))],1,4), substr(names(site_data)[8:length(names(site_data))],8,20)))  # tady konec

names_trees<-subset(site_metadata,DendrometerType=="DLR26C")$Device_ID[which(subset(site_metadata,DendrometerType=="DLR26C")$Device_ID %in% names(site_data))] 
big_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

names_trees<-subset(site_metadata,DendrometerType=="PTi")$Device_ID[which(subset(site_metadata,DendrometerType=="PTi")$Device_ID %in% names(site_data))]
small_trees<-site_data[,which(names(site_data) %in% c("Time","DOY","Year","Month","Day","Hour","Minute",names_trees))]

big_trees<-subset(big_trees, Minute==0)
small_trees<-subset(small_trees, Minute==0)

# site_metadata<-update.site.meta(small_trees)


big_trees<-cut.wrong.trees(stem_data=big_trees,
                           meta_data=site_metadata,
                           junk_trees=site_junk_trees,
                           dendrometer_type="DLR26C")

small_trees_growing<-cut.wrong.trees(stem_data=small_trees,
                                     meta_data=site_metadata,
                                     junk_trees=site_junk_trees,
                                     dendrometer_type="PTi")

big_trees<-correct.curves(big_trees, min.diff=1.0)
small_trees_growing<-correct.curves(small_trees_growing, min.diff=0.25)

big_trees<-create.main.data.file(stem_data=big_trees,
                                 meta_data=site_metadata,
                                 DBH_year=DBH_measurement_year,
                                 DBH_DOY=DBH_measurement_DOY)

small_trees_growing<-create.main.data.file(stem_data=small_trees_growing,
                                           meta_data=site_metadata,
                                           DBH_year=small_trees$Year[nrow(small_trees)],
                                           DBH_DOY=small_trees$DOY[nrow(small_trees)])

## Creates the main data file
big_trees<-calculate_normalized_data(input=big_trees, gs_file=site_junk_trees)
small_trees_growing<-calculate_normalized_data(input=small_trees_growing, gs_file=site_junk_trees)

## cutting confidence interval
big_trees<-subset(big_trees, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))
small_trees_growing<-subset(small_trees_growing, Normalized_zero_growth>=min(ci_growing_season) & Normalized_zero_growth<=max(ci_growing_season))

# deleting the first year
big_trees<-subset(big_trees,Year>=2019)
small_trees_growing<-subset(small_trees_growing,Year>=2019)

## rozliseni rustovych fazi
big_trees<-determine.growth.phase(big_trees)
small_trees_growing<-determine.growth.phase(small_trees_growing)

## rozliseni denniho cyklu
big_trees<-determine.day.phase(big_trees,latitude,longitude)
small_trees_growing<-determine.day.phase(small_trees_growing,latitude,longitude)

## sorting data
RN_ACCA_big<-subset(big_trees,Species=="ACCA")
RN_ACCA_small_growing<-subset(small_trees_growing,Species=="ACCA")

RN_CABE_big<-subset(big_trees,Species=="CABE")
RN_CABE_small_growing<-subset(small_trees_growing,Species=="CABE")

RN_ULSP_big<-subset(big_trees,Species=="ULSP")
RN_ULSP_small_growing<-subset(small_trees_growing,Species=="ULSP")

## cleanig the last mistakes
RN_ACCA_big<-na.omit(RN_ACCA_big)
RN_ACCA_small_growing<-na.omit(RN_ACCA_small_growing)

RN_CABE_big<-na.omit(RN_CABE_big)
RN_CABE_small_growing<-na.omit(RN_CABE_small_growing)

RN_ULSP_big<-na.omit(RN_ULSP_big)
RN_ULSP_small_growing<-na.omit(RN_ULSP_small_growing)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## Data saving
#-------------------------------------------------------------------------------
folder<-"Datasets_recalculated/Recalculated_datasets"

# Eustaska
write.xlsx(EU_PCAB_big,paste0(folder,"/","EU_PCAB_bigTrees.xlsx"))
write.xlsx(EU_PCAB_small_growing,paste0(folder,"/","EU_PCAB_smallTrees.xlsx"))


# Boubin
write.xlsx(BB_PCAB_big,paste0(folder,"/","BB_PCAB_bigTrees.xlsx"))
write.xlsx(BB_PCAB_small_growing,paste0(folder,"/","BB_PCAB_smallTrees.xlsx"))

write.xlsx(BB_FASY_big,paste0(folder,"/","BB_FASY_bigTrees.xlsx"))
write.xlsx(BB_FASY_small_growing,paste0(folder,"/","BB_FASY_smallTrees.xlsx"))


# Zofin
write.xlsx(ZF_PCAB_big,paste0(folder,"/","ZF_PCAB_bigTrees.xlsx"))
write.xlsx(ZF_PCAB_small_growing,paste0(folder,"/","ZF_PCAB_smallTrees.xlsx"))

write.xlsx(ZF_FASY_big,paste0(folder,"/","ZF_FASY_bigTrees.xlsx"))
write.xlsx(ZF_FASY_small_growing,paste0(folder,"/","ZF_FASY_smallTrees.xlsx"))


# Ranspurk
write.xlsx(RN_ACCA_big,paste0(folder,"/","RN_ACCA_bigTrees.xlsx"))
write.xlsx(RN_ACCA_small_growing,paste0(folder,"/","RN_ACCA_smallTrees.xlsx"))

write.xlsx(RN_CABE_big,paste0(folder,"/","RN_CABE_bigTrees.xlsx"))
write.xlsx(RN_CABE_small_growing,paste0(folder,"/","RN_CABE_smallTrees.xlsx"))

write.xlsx(RN_ULSP_big,paste0(folder,"/","RN_ULSP_bigTrees.xlsx"))
write.xlsx(RN_ULSP_small_growing,paste0(folder,"/","RN_ULSP_smallTrees.xlsx"))