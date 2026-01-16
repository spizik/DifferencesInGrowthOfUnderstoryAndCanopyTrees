## Libraries
library("openxlsx")

## loads prepared datasets ####
#
# loading data prepared in script 02_data_preparation.R
#
# arguments:
#   - data_folder - folder with relaculated data
#   - data_type - if daily or hourly data will be loaded
#
# output is a list with all data
#
load.prepared.data<-function(data_folder){
  
  # testing files
  # data_folder="Recalculated_datasets"
  
  # load file names of dataset for site
  files<-list.files(data_folder)
  
  site_names<-gsub(".xlsx", "", files)
  
  # initialize output list
  output<-list()
  
  # for cycle for data loading
  for(i in 1:length(files)){
    
    # asign site and file name
    site_name<-site_names[i]
    file_name<-files[i]
    
    # load file
    output[[site_name]]<-read.xlsx(paste0(data_folder,"/",file_name))
  }
  
  return(output)
}

## SWC data preparation ####
load.swp.data<-function(data_folder,output_format="data.frame"){
  
  ## testing arguments
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
prepare.swc<-function(swp.dta.input, depth=10){
  ## testing arguments
  # swp.dta.input=load.swp.data("SWP_data/Eustaska")
  # depth=10
  
  # swp.dta.input<-swp.dta.input[,c(2,3,6,which(substr(names(swp.dta.input),12,16)==paste0(depth,"cm")))]
  swp.dta.input<-swp.dta.input[,c(2,3,6,which(str_sub(names(swp.dta.input), -4)==paste0(depth,"cm")))]
  
  output<-swp.dta.input[,c("Year","DOY","Hour")]
  output$SWC<-rowMeans(swp.dta.input[,c(4:ncol(swp.dta.input))],na.rm=T)
  
  return(output)
}
#-------------------------------------------------------------------------------
site_metadata<-read.xlsx("Datasets/Metadata/_Site_description.xlsx")

dendrometer_data<-load.prepared.data("Datasets_recalculated/Recalculated_datasets")
climate_data<-load.prepared.data("Datasets_recalculated/Recalculated_stationclimate")
climate_data_forest<-load.prepared.data("Datasets/Meteo_data_understory")

# SWP --------------------------------------------------------------------------
swp_data<-list()

swp_data[["Boubin"]]<-prepare.swc(load.swp.data("Datasets/SWP_data/Boubin"))
swp_data[["Eustaska"]]<-prepare.swc(load.swp.data("Datasets/SWP_data/Eustaska"))
swp_data[["Ranspurk"]]<-prepare.swc(load.swp.data("Datasets/SWP_data/Ranspurk"))
swp_data[["Zofin"]]<-prepare.swc(load.swp.data("Datasets/SWP_data/Zofin"))

# Hemifoto ---------------------------------------------------------------------
hemi_boubin<-read.xlsx("Datasets_recalculated/Hemiphoto_results/res_boubin.xlsx")
hemi_eustaska<-read.xlsx("Datasets_recalculated/Hemiphoto_results/res_eustaska.xlsx")
hemi_ranspurk<-read.xlsx("Datasets_recalculated/Hemiphoto_results/res_ranspurk.xlsx")
hemi_zofin<-read.xlsx("Datasets_recalculated/Hemiphoto_results/res_zofin.xlsx")