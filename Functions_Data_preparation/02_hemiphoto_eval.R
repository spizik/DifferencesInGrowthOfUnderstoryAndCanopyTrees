#-------------------------------------------------------------------------------
## Loading and initializating libraries ####
library("hemispheR")
library("openxlsx")
#-------------------------------------------------------------------------------
## Loading and initializating functions ####

## evaluate_me_hemiphotos ####
#
# Evaluate canopy openness from hemispherical photographs.
#
# Arguments:
#   input_folder - path to the folder containing hemispherical JPG images
#
# Output is a data.frame with one row per image containing:
#     - tree              : image name without file extension
#     - perc_canopy_open  : proportion of open canopy pixels (0–1)
#
evaluate_me_hemiphotos<-function(input_folder){
  
  ## Testing arguments
  # input_folder="Hemifoto/Ranspurk"
  
  files<-list.files(input_folder)
  
  output<-data.frame(tree=gsub(".JPG","",files), perc_canopy_open=NA)
  
  files<-paste0(input_folder,"/",files)
  
  for(i in 1:length(files)){
    
    print(files[i])
    
    fig<-import_fisheye(files[i])
    eval_fig<-binarize_fisheye(fig)
    nums<-as.data.frame(eval_fig)
    
    output[i,"perc_canopy_open"]<-length(which(nums[1]==1))/nrow(nums)
  }
  
  return(output)
}

#-------------------------------------------------------------------------------
# Data evaluation ####
hemi_boubin<-evaluate_me_hemiphotos("Datasets/Hemiphoto/Boubin")
hemi_eustaska<-evaluate_me_hemiphotos("Datasets/Hemiphoto/Eustaska")
hemi_ranspurk<-evaluate_me_hemiphotos("Datasets/Hemiphoto/Ranspurk")
hemi_zofin<-evaluate_me_hemiphotos("Datasets/Hemiphoto/Zofin")

# Data writing ####
write.xlsx(hemi_boubin, "Datasets_recalculated/Hemiphoto_results/res_boubin.xlsx")
write.xlsx(hemi_eustaska, "Datasets_recalculated/Hemiphoto_results/res_eustaska.xlsx")
write.xlsx(hemi_ranspurk, "Datasets_recalculated/Hemiphoto_results/res_ranspurk.xlsx")
write.xlsx(hemi_zofin, "Datasets_recalculated/Hemiphoto_results/res_zofin.xlsx")




