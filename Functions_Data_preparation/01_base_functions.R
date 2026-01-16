# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Data loading and preparation
# ------------------------------------------------------------------------------
## load.dendrometer.data ####
#
# Loading dendrometer data
# expects the same number of columns and columns names in all files
#
# argumets:
#   data_folder   - route to the data folder in the workspace
#   output_format - of the dataset (list of data.frame), implicit value is data.frame
#                 - this also defines the output format
#
load.dendrometer.data<-function(data_folder,output_format="data.frame"){
  
  ## Testing argumets
  # data_folder="Dendrometer_data/Eustaska"
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

## cut.wrong.trees ####
#
# select trees with specific dendrometer type
# 
# arguments:
#   - stem_data
#   - metadata - dendrometer type name should be in the column DendrometerType
#   - dendrometer_type - type of dendrometer to be selected
#
# output is data.frame with columns: "Year","DOY","Month","Day","Hour","Minute"
#                                     column with data of each tree
cut.wrong.trees<-function(stem_data,meta_data, junk_trees, dendrometer_type="DLR26C"){
  
  ## Testing argumets
  # stem_data=site_data
  # meta_data=site_metadata
  # junk_trees=site_junk_trees
  # dendrometer_type="DLR26C"
  
  junk_trees<-subset(junk_trees, Junk=="Junk" | Junk=="junk")
  
  # deleting data of junk tree/years
  for(i in 1:nrow(junk_trees)){
    # print(junk_trees$Tree_ID[i])
    stem_data[which(stem_data$Year==junk_trees$Year[i]),junk_trees$Tree_ID[i]]<-NA
  }
  
  # the main columns necessary for all the data
  main_columns<-c("Year","DOY","Month","Day","Hour","Minute")
  
  # selected trees with proper dendrometer
  selected_trees<-names(stem_data)[which(names(stem_data) %in% subset(meta_data,DendrometerType==dendrometer_type)$Device_ID)]
  
  # selecting right columns
  stem_data<-stem_data[,c(main_columns,selected_trees)]
  
  return(stem_data)
}

## correct.curves ####
# 
# correct all stem curves to delete steps in the data
#
# arguments:
#   - stem_data - data with stem tape values
#   - min.diff  - defines minimal value of the step to be corrected
#   - min.val   - minimal value to correct the ninitial value
#
correct.curves<-function(stem_data,min.diff=2,min.val=0){
  
  ## Testing argumets
  # stem_data=small_trees_growing
  # min.diff=2
  # min.val=0
  
  # inicializing only tree names
  tree_names<-names(stem_data)[-which(names(stem_data) %in% c("Year","DOY","Month","Day","Hour","Minute"))]
  
  # filling data of each tree
  for(i in tree_names){
    
    # print(i)
    
    # subset tree data
    sub<-stem_data[,i]
    
    # calculate differences in tape data
    differences<-sub[2:length(sub)]-sub[1:(length(sub)-1)]  
    differences<-which(abs(differences)>=min.diff)
    
    # dif there are differencess above given treshold this part is executed
    if(length(differences)>0){
      
      # for each difference the script will proceed with correction procedure
      for(j in c(1:length(differences))){
        
        # index where the difference had been found (the value before grop)
        diff_index<-differences[j]
        
        # index of first value after drop
        calculate_after_index<-diff_index+1
        
        # tis part will correct the data by adding value before drop minus the new initial value
        sub[calculate_after_index:length(sub)]<-sub[calculate_after_index:length(sub)]+sub[diff_index]-sub[calculate_after_index]
      }
    }
    
    # correct the data for the new minimal value
    sub<-sub-(min(sub,na.rm=T)-min.val)
    
    # add the data into right column
    stem_data[,i]<-sub
  }
  
  # returning corrected tape data
  return(stem_data)
}


## calculate.increment ####
#
# function for increment calculations
#
# arguments:
#   - x - vector to be calculated
#
# returns vector of the same length as data input (the first value is NA)
#
calculate.increment<-function(x){
  output<-c(NA,x[2:length(x)]-x[1:(length(x)-1)])
  return(output)
}

## calculate.continuous.dbh ####
#
# Recalculates tape data into continuous DBH data
#
# arguments:
#   - stem_data - data with stem tape values
#   - meta_data
#   - DBH_year  - year, when DBH was measured
#   - DBH_DOY   - DOY when DBH was measured
#
# output is tada.frame with recalculated dta
#
calculate.continuous.dbh<-function(stem_data,meta_data,DBH_year,DBH_DOY){
  
  ## Testing argumets
  # stem_data=small_trees_growing
  # meta_data=site_metadata
  # DBH_year=2024
  # DBH_DOY=283
  
  # inicializing only tree names
  tree_names<-names(stem_data)[-which(names(stem_data) %in% c("Year","DOY","Month","Day","Hour","Minute"))]
  
  # calculates continuous DBH of each tree
  for(i in tree_names){
    
    ## Testing argumets
    # i="57__3_0468"
    
    # subset of the data to be calculated
    sub<-stem_data[,i]
    
    if(subset(meta_data,Device_ID==i)$DendrometerType=="PTi"){
      
      # save measured DBH at a given time 
      DBH<-subset(meta_data,Device_ID==i)$DBH
      
      # calculate continuous stem DBH
      sub<-DBH-(max(sub,na.rm=T)-sub) # to data before circumference value needs to be deducted
      
    } else {
      # save measured DBH at a given time 
      DBH<-subset(meta_data,Device_ID==i)$DBH
      
      # calculate stem circumference at a given time
      circumference<-2*pi*(DBH/2)
      
      # calculate indexes before DBH measurements
      index_to<-c(1:max(which(stem_data$Year==DBH_year & stem_data$DOY==DBH_DOY)))
      
      # calculate indexes after DBH measurements
      index_from<-(max(which(stem_data$Year==DBH_year & stem_data$DOY==DBH_DOY))+1):length(sub)
      
      # value when the two parts of the dataset cross
      crossing.value<-sub[max(index_to)]
      if(is.na(crossing.value)) crossing.value<-na.omit(sub[index_from:length(sub)])[1]
      
      # calculate continuous stem circumference
      sub[index_to]<-circumference-(crossing.value-sub[index_to]) # to data before circumference value needs to be deducted
      sub[index_from]<-circumference+(sub[index_from]-crossing.value) # to data before circumference value needs to be added
      
      # recalculates continuous circumference to continuous DBH
      sub<-(sub/(2*pi))*2
    }
    
    # saving recalculated data into output data.frame
    stem_data[,i]<-sub
  }
  
  return(stem_data)
  
}

## calculate.increments.df ####
#
# calluculates increments (or opposit) from the tape and DBH data
#
# arguments:
#   - input - input data.frame
#
calculate.increments.df<-function(input){
  
  ## Testing argumets
  # input=corrected_stem
  
  # inicializing only tree names
  tree_names<-names(input)[-which(names(input) %in% c("Year","DOY","Month","Day","Hour","Minute"))]
  
  # calculates continuous DBH of each tree
  for(i in tree_names){
    
    # subset of the data to be calculated
    sub<-input[,i]
    
    # calculates incremet data
    increments<-calculate.increment(sub)
    
    input[,i]<-increments
  }
  
  return(input)
}

## calculate.increments.column ####
#
# calluculates increments (or opposit) from the tape, DBH and normalize growth data
#
# arguments:
#   - input   - input data.frame
#   - column  - name of the column to be calculated 
#
calculate.increments.column<-function(input,column){
  
  ## Testing argumets
  # input=output
  # column="Tape_value"
  
  # inicialize output vector
  output<-NULL
  # tro cycles for calculations one for TreeID and second for Year
  for(i in unique(input$Tree)){
    for(j in unique(input$Year)){
      
      # data subset based on treeId and year combination
      sub<-subset(input,Tree==i & Year==j)[,column]
      
      # when the length of subset is higher than 0
      if(length(sub)>0){
        # calculates increments
        temp<-calculate.increment(sub)
        
        # add calculated data to output
        output<-c(output,temp)
      }
    }
  }
  return(output)
}
## create.main.data.file ####
#
# create main data.frame ordered to colums
#
#   - stem_data - data with stam tape data
#   - meta_data - meta data for particular site
#   - DBH_year  - year whe DBH was measured
#   - DBH_DOY   - DOY when DBH was measured
#
create.main.data.file<-function(stem_data, 
                                meta_data,
                                DBH_year,
                                DBH_DOY){
  
  ## Testing argumets
  # stem_data=small_trees_growing
  # meta_data=site_metadata
  # DBH_year=2024
  # DBH_DOY=283
  
  # calls function that correct jumps in the data
  # corrected_stem<-correct.curves(stem_data)
  
  # calls function that recalculates DBH data
  continuous_DBH<-calculate.continuous.dbh(stem_data,meta_data,DBH_year,DBH_DOY)
  
  # calculates tape increments
  tape_increments<-calculate.increments.df(stem_data)
  
  # calculates DBH increments
  dbh_increments<-calculate.increments.df(continuous_DBH)
  
  ## here can be added site ID
  
  # the main columns necessary for all the data
  main_columns<-c("Year","DOY","Month","Day","Hour","Minute")
  
  # reshaping data.frame of stem Tape data
  output<-melt(stem_data,id=main_columns)
  
  # reordering data.frame
  output$Species<-NA
  output<-output[,c("variable","Species",main_columns,"value")]
  names(output)<-c("Tree","Species",main_columns,"Tape_value")
  
  # adding tree species
  output$Species<-add.species(output,meta_data)
  
  # add calculated continuous stem DBH 
  output$DBH_value<-melt(continuous_DBH,id=main_columns)$value
  
  # add stem tape increment
  output$Tape_increment<-melt(tape_increments,id=main_columns)$value
  
  # add stem DBH increment
  output$DBH_increment<-melt(dbh_increments,id=main_columns)$value
  
  return(output) 
}

## add.species ####
#
# add species into the data.frame
#
# arguments:
# input     - data.frame where species code will be added
# meta_data - file with meta_data, including species cordes
#
# output is a vector of the same length as input row number
#
add.species<-function(input,meta_data){
  
  ## Testing argumets
  # input=output
  # meta_data=Boubin_metadata
  
  all_species<-rep(NA,nrow(input))
  for(i in unique(input$Tree)){
    all_species[which(input$Tree==i)]<-subset(meta_data,Device_ID==i)$Species
  }
  return(all_species)
}

## calculate.daily.data ####
#
# aggregate hourly or sub hourly data into daily data
#
# arguments:
#   - input - input data.frame with hourly or sub-hourly data
#   - value - method for data aggregation (mean or max)
calculate.daily.data<-function(input,variable="max"){
  
  ## Testing argumets
  # input=site_data
  # variable="max"
  
  # aggregate tape and DBH data
  if(variable=="mean"){
    output<-aggregate(Tape_value~Tree+Species+Year+DOY+Month+Day,
                      data=input,
                      FUN=mean,
                      na.rm = TRUE, 
                      na.action=NULL)
    dbh<-aggregate(DBH_value~Tree+Species+Year+DOY+Month+Day,
                   data=input,
                   FUN=mean,
                   na.rm = TRUE, 
                   na.action=NULL)
  }
  if(variable=="max"){
    output<-aggregate(Tape_value~Tree+Species+Year+DOY+Month+Day,
                      data=input,
                      FUN=max,
                      na.rm = TRUE, 
                      na.action=NULL)
    dbh<-aggregate(DBH_value~Tree+Species+Year+DOY+Month+Day,
                   data=input,
                   FUN=max,
                   na.rm = TRUE, 
                   na.action=NULL)
  }
  
  # order output data.frame
  output<-output[order(output$Tree,output$Year,output$DOY),]
  
  # order and adds DBH data
  output$DBH_value<-dbh[order(dbh$Tree,dbh$Year,dbh$DOY),"DBH_value"]
  
  # Inf to NAs
  output$Tape_value[which(is.infinite(output$Tape_value))]<-NA
  output$DBH_value[which(is.infinite(output$DBH_value))]<-NA
  
  # calculate tape and DBH increments
  output$Tape_increment<-calculate.increments.column(output,"Tape_value")
  output$DBH_increment<-calculate.increments.column(output,"DBH_value")
  
  return(output)
}

## data normalization ####
#
# x - input vector with data to be normalized
#
min_max_norm <- function(x){
  out<-(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))
  out[1:which.max(out>=0)]<-0
  out[which.max(out==1):length(out)]<-1
  return(out)
}

## calculate.zero.growth  ####
#
# calculate zero-growth by Zweifel
#
calculate.zero.growth<-function(x){
  
  ## Testing argumets
  # x=sub$stem.gro
  
  out<-max_run(x)
  return(out)
}

## calculate_normalized_data ####
#
# add normalized growth data to the main data.frame
# add also zero growth incremet
#
# arguments:
#   - input         - input data.frame with daily and hourly data
#   - gs_beginning  - esimated growing seson beginning (to avoud frost drought)
#   - gs_enging     - estimated growing season ending (to cut the schrinkage in the end)
#
calculate_normalized_data <- function(input, gs_file){
  
  ## Testing argumets
  # input=small_trees_growing
  # gs_file=site_junk_trees
  
  gs_file<-subset(gs_file, is.na(Junk))
  output<-input
  
  rows_to_keep<-NULL
  
  for(i in 1:nrow(gs_file)){
    tr<-gs_file$Tree[i]
    yr<-gs_file$Year[i]
    sos<-gs_file$SOS[i]
    eos<-gs_file$EOS[i]
    
    temp<-which(output$Tree==tr & output$Year==yr & output$DOY>=sos & output$DOY<=eos)
    
    rows_to_keep<-c(rows_to_keep,temp)
  }
  
  # output<-subset(input,DOY>=gs_beginning & DOY<=gs_ending)
  output <- input[sort(rows_to_keep), ]
  
  output$Normalized_growth<-NA
  output$Normalized_zero_growth<-NA
  output$DBH_zero_growth<-NA
  # output$BA_zero_growth<-NA
  
  output$Normalized_increment<-NA
  output$Normalized_zero_increment<-NA
  output$DBH_zero_increment<-NA
  # output$BA_zero_increment<-NA
  
  output$twd_normalized<-NA
  output$twd_DBH<-NA
  # output$twd_BA<-NA
  
  for(i in unique(output$Tree)){
    for(j in unique(output$Year)){
      
      # i="57__3_0468"
      # j=2021
      
      # print(c(i,j))
      
      # index of data to be replaced
      indexes<-which(output$Tree==i & output$Year==j)
      
      # data subset based on treeId and year combination
      sub<-output[indexes,]
      
      
      # deleting missing data
      # inds.to.delete<-which()
      # sub<-na.omit(sub)
      
      # assigning tape values
      tape_data<-sub$Tape_value
      dbh_data<-sub$DBH_value
      # ba_data<-sub$BA_value 
      
      # if there are data the the script continue 
      if(length(na.omit(tape_data))>0 & 
         length(na.omit(dbh_data))>0 # &
         # length(na.omit(ba_data))>0
      ){
        
        # print(c(i,j))
        
        # adjust minimal and maximal stem tape data
        tape_data<-tape_data-min(tape_data,na.rm=T)
        tape_data[1:which.min(tape_data)]<-0
        tape_data[which.max(tape_data):length(tape_data)]<-max(tape_data,na.rm=T)
        
        dbh_data<-dbh_data-min(dbh_data,na.rm=T)
        dbh_data[1:which.min(dbh_data)]<-0
        dbh_data[which.max(dbh_data):length(dbh_data)]<-max(dbh_data,na.rm=T)
        
        # ba_data<-ba_data-min(ba_data,na.rm=T)
        # ba_data[1:which.min(ba_data)]<-0
        # ba_data[which.max(ba_data):length(ba_data)]<-max(ba_data,na.rm=T)
        
        # normalize data
        normalized_data<-min_max_norm(tape_data)
        
        # caluclate normalized data increment
        normalized_data_increment<-calculate.increment(normalized_data)
        
        # zero growth
        zero_growth_normalized<-calculate.zero.growth(normalized_data)
        zero_growth_dbh<-calculate.zero.growth(dbh_data)
        # zero_growth_ba<-calculate.zero.growth(ba_data)
        
        # zero growth increment
        zero_growth_increment_normalized<-calculate.increment(zero_growth_normalized)
        zero_growth_increment_dbh<-calculate.increment(zero_growth_dbh)
        # zero_growth_increment_ba<-calculate.increment(zero_growth_ba)
        
        # TWD
        twd_normalized<-zero_growth_normalized-normalized_data
        twd_dbh<-zero_growth_dbh-dbh_data
        # twd_ba<-zero_growth_ba-ba_data
        
        # add calculated data into output data.frame
        output$Normalized_growth[indexes]<-normalized_data
        output$Normalized_zero_growth[indexes]<-zero_growth_normalized
        output$DBH_zero_growth[indexes]<-zero_growth_dbh
        # output$BA_zero_growth[indexes]<-zero_growth_ba
        
        output$Normalized_increment[indexes]<-normalized_data_increment
        output$Normalized_zero_increment[indexes]<-zero_growth_increment_normalized
        output$DBH_zero_increment[indexes]<-zero_growth_increment_dbh
        # output$BA_zero_increment[indexes]<-zero_growth_increment_ba
        
        output$twd_normalized[indexes]<-twd_normalized
        output$twd_DBH[indexes]<-twd_dbh
        # output$twd_BA[indexes]<-twd_ba
      }
    }
  }
  
  return(output)
}
old1_calculate_normalized_data<-function(input,gs_beginning=91,gs_ending=304){
  
  ## Testing argumets
  # input=big_trees
  # input=site_data_daily
  # gs_beginning=121
  # gs_ending=304

  output<-subset(input,DOY>=gs_beginning & DOY<=gs_ending)
  
  output$zero_growth_DBH_value<-NA
  
  output$Normalized_growth<-NA
  output$Normalized_increment<-NA
  
  output$Normalized_zero_growth<-NA
  output$Normalized_zero_increment<-NA
  
  output$twd_absolute<-twd_absolute<-NA
  output$twd_normalized<-twd_normalized<-NA
  
  for(i in unique(output$Tree)){
    for(j in unique(output$Year)){

      # print(c(i,j))
      
      # index of data to be replaced
      indexes<-which(output$Tree==i & output$Year==j)
      
      # data subset based on treeId and year combination
      sub<-output[indexes,]
      
      # deleting missing data
      # inds.to.delete<-which()
      # sub<-na.omit(sub)
      
      # assigning tape values
      stem_data<-sub$Tape_value
      
      # if there are data the the script continue 
      if(length(na.omit(stem_data))>0){
        
        # adjust minimal and maximal stem tape data
        stem_data<-stem_data-min(stem_data,na.rm=T)
        stem_data[1:which.min(stem_data)]<-0
        stem_data[which.max(stem_data):length(stem_data)]<-max(stem_data,na.rm=T)
        
        # normalize data
        normalized_data<-min_max_norm(stem_data)
        
        # caluclate normalized data increment
        normalized_data_increment<-calculate.increment(normalized_data)
        
        # zero growth
        zero_growth_dbh<-calculate.zero.growth(stem_data)
        zero_growth<-calculate.zero.growth(normalized_data)
        
        # zero growth increment
        zero_growth_increment<-calculate.increment(zero_growth)
        
        # TWD
        twd_absolute<-zero_growth_dbh-stem_data
        twd_normalized<-zero_growth-normalized_data
        
        # add calculated data into output data.frame
        output$zero_growth_DBH_value[indexes]<-zero_growth_dbh
          
        output$Normalized_growth[indexes]<-normalized_data
        output$Normalized_increment[indexes]<-normalized_data_increment
        
        output$Normalized_zero_growth[indexes]<-zero_growth
        output$Normalized_zero_increment[indexes]<-zero_growth_increment
        
        output$twd_absolute[indexes]<-twd_absolute
        output$twd_normalized[indexes]<-twd_normalized
      }
    }
  }
  
  return(output)
}
old2_calculate_normalized_data<-function(input,gs_file){
  
  ## Testing argumets
  ## testing files
  # input=big_trees
  # gs_file=site_junk_trees
  
  gs_file<-subset(gs_file, is.na(Junk))
  output<-input
  
  rows_to_keep<-NULL
  
  for(i in 1:nrow(gs_file)){
    tr<-gs_file$Tree_ID[i]
    yr<-gs_file$Year[i]
    sos<-gs_file$SOS[i]
    eos<-gs_file$EOS[i]
    
    temp<-which(output$Tree==tr & output$Year==yr & output$DOY>=sos & output$DOY<=eos)
    
    rows_to_keep<-c(rows_to_keep,temp)
  }
  
  # output<-subset(input,DOY>=gs_beginning & DOY<=gs_ending)
  output<-input[rows_to_keep,]
  
  output$zero_growth_DBH_value<-NA
  
  output$Normalized_growth<-NA
  output$Normalized_increment<-NA
  
  output$Normalized_zero_growth<-NA
  output$Normalized_zero_increment<-NA
  
  output$twd_absolute<-twd_absolute<-NA
  output$twd_normalized<-twd_normalized<-NA
  
  for(i in unique(output$Tree)){
    for(j in unique(output$Year)){
      
      # print(c(i,j))
      
      # index of data to be replaced
      indexes<-which(output$Tree==i & output$Year==j)
      
      # data subset based on treeId and year combination
      sub<-output[indexes,]
      
      # deleting missing data
      # inds.to.delete<-which()
      # sub<-na.omit(sub)
      
      # assigning tape values
      stem_data<-sub$Tape_value
      
      # if there are data the the script continue 
      if(length(na.omit(stem_data))>0){
        
        # adjust minimal and maximal stem tape data
        stem_data<-stem_data-min(stem_data,na.rm=T)
        stem_data[1:which.min(stem_data)]<-0
        stem_data[which.max(stem_data):length(stem_data)]<-max(stem_data,na.rm=T)
        
        # normalize data
        normalized_data<-min_max_norm(stem_data)
        
        # caluclate normalized data increment
        normalized_data_increment<-calculate.increment(normalized_data)
        
        # zero growth
        zero_growth_dbh<-calculate.zero.growth(stem_data)
        zero_growth<-calculate.zero.growth(normalized_data)
        
        # zero growth increment
        zero_growth_increment<-calculate.increment(zero_growth)
        
        # TWD
        twd_absolute<-zero_growth_dbh-stem_data
        twd_normalized<-zero_growth-normalized_data
        
        # add calculated data into output data.frame
        output$zero_growth_DBH_value[indexes]<-zero_growth_dbh
        
        output$Normalized_growth[indexes]<-normalized_data
        output$Normalized_increment[indexes]<-normalized_data_increment
        
        output$Normalized_zero_growth[indexes]<-zero_growth
        output$Normalized_zero_increment[indexes]<-zero_growth_increment
        
        output$twd_absolute[indexes]<-twd_absolute
        output$twd_normalized[indexes]<-twd_normalized
      }
    }
  }
  
  return(output)
}

## determine.growth.status ####
#
# determine growth status of the tree to: 
#   - Growth (positive zero growth increment)
#   - Schrinkage (all negative increment values)
#   - Rehydratation (all positive increments except zero growth)
#   - No_change (0 difference betwee value in time t and t+1)
#   - NA (value without increment)
#
# output is complete data.frame
#
determine.growth.status<-function(input){
  
  ## Testing arguments
  # input=site_data_hourly
  
  input$State<-NA
  
  input$State[which(input$Normalized_increment>0)]      <- "Rehydratation"
  input$State[which(input$Normalized_increment<0)]      <- "Schrinkage"
  input$State[which(input$Normalized_increment==0)]     <- "No_change"
  input$State[which(is.na(input$DBH_value))]            <- NA
  input$State[which(input$Normalized_zero_increment>0)] <- "Growth"
  
  return(input)
}

## restrict.growing.season ####
#
# return indexes of the growing seson within given confidence interval
#
# arguments:
#   - input - data.frame with all the data
#   - ci    - confidence interval of the growing season (implicit is full growing seson (c(0,1)))
#
# returns vectors wit indexes of all relevant rows of the input data.frame
#
restrict.growing.season<-function(input,ci_growing_season=c(0,1)){
  
  ## Testing arguments
  # input=site_data_hourly
  # ci=c(0.025,0.975)
  
  # initializing output vector
  indexes<-NULL
  
  # for loops for data tree-year identification
  for(i in unique(input$Tree)){
    for(j in unique(input$Year)){
      
      # data subset to ensure if the tree was measured in a given year
      sub<-subset(input, input$Tree==i & input$Year==j)
      
      # if tree has data, this part is executed
      if(nrow(sub>0)){
        # identify all DOYs of the growing season
        doys<-input$DOY[which(input$Tree==i & 
                                input$Year==j & 
                                input$Normalized_growth>min(ci_growing_season) & 
                                input$Normalized_growth<max(ci_growing_season))]
        
        # return indexes of all relevant DOYs
        indexes<-c(indexes,which(input$Tree==i & 
                                   input$Year==j & 
                                   input$DOY %in% doys))
      }
    }
  }
  
  return(indexes)
}

## site.data.preparation ####
#
# prepare datasets of hourly and daily data for one site 
#
# arguments:
#   - data_folder           - folder with xlsx files with dendrometer data
#   - site_metadata         - file with site metadata
#   - site_junk_trees       - file with junk tree/years
#   - DBH_measurement_year  - Year when tree DBH was measured
#   - DBH_measurement_DOY   - DOY when tree DBH was measured 
#   - ci_growing_season     - confidence interval of the growing season
#
# returns list with hourly and daily data
#
bc.site.data.preparation<-function(data_folder,
                                   site_metadata,
                                   site_junk_trees,
                                   DBH_measurement_year,
                                   DBH_measurement_DOY,
                                   ci_growing_season=c(0.025,0.975)){
  
  ## Testing arguments
  # data_folder="Dendrometer_data/Boubin"
  # site_metadata<-read.xlsx("Metadata/Boubin_meta.xlsx")
  # site_junk_trees<-read.xlsx("Junk_data/junk_Boubin.xlsx")
  # DBH_measurement_year=2021
  # DBH_measurement_DOY=308
  # ci_growing_season=c(0.025,0.975)
  
  site_data<-load.dendrometer.data(data_folder=data_folder,
                                   output_format="data.frame")
  
  site_data<-cut.wrong.trees(stem_data=site_data,
                             meta_data=site_metadata,
                             junk_trees=site_junk_trees,
                             dendrometer_type="DLR26C")
  
  site_data<-create.main.data.file(stem_data=site_data,
                                   meta_data=site_metadata,
                                   DBH_year=DBH_measurement_year,
                                   DBH_DOY=DBH_measurement_DOY)
  
  site_data_hourly<-site_data
  site_data_daily<-calculate.daily.data(input=site_data,
                                        variable="max")
  
  site_data_hourly<-calculate_normalized_data(input=site_data_hourly)
  site_data_daily<-calculate_normalized_data(input=site_data_daily)
  
  site_data_hourly<-determine.growth.status(input=site_data_hourly)
  site_data_daily<-determine.growth.status(input=site_data_daily)
  
  site_data_hourly<-site_data_hourly[restrict.growing.season(site_data_hourly,ci_growing_season),]
  site_data_daily<-site_data_daily[restrict.growing.season(site_data_daily,ci_growing_season),]
  
  site_data_hourly<-na.omit(site_data_hourly)
  site_data_daily<-na.omit(site_data_daily)
  
  output<-list(hourly_data=site_data_hourly,
               daily_data =site_data_daily)
  
  return(output)
}


## determine.day.phase ####
#
# determines time of day (if at least 50 percent of the hour falls within a given phase)
# distinguishes night, dawn, day, and dusk
#
# input is a dataset with dendrometer data
# input must contain!!! year, month, and day (for calculation) and geographic coordinates of the site
#
# output is a data.frame with the same structure as the input,
# extended by a column indicating time of day
#
determine.day.phase <- function(date.input, latitude, longitude, tz = "Etc/GMT-1") {
  
  ## Testing arguments
  # date.input=big_trees
  # latitude=50.068
  # longitude=17.261
  # tz = "Etc/GMT-1"
  
  # Create 'dates' column in date.input
  date.input$dates <- as.Date(paste(date.input$Year,
                                    formatC(date.input$Month, format = "f", flag = "0", width = 2, digits = 0),
                                    formatC(date.input$Day, format = "f", flag = "0", width = 2, digits = 0),
                                    sep = "-"))
  
  # Initialize DayPhase column
  date.input$DayPhase <- NA
  
  # Get unique dates from input data
  unique_dates <- unique(date.input$dates)
  
  # Fetch sunlight times for the unique dates and location
  # sun_times <- getSunlightTimes(date = unique_dates, lat = latitude, lon = longitude, tz = tz, 
  #                               keep = c("nightEnd", "sunriseEnd", "sunsetStart", "night"))
  # sun_times <- getSunlightTimes(date = unique_dates, lat = latitude, lon = longitude, tz = tz, 
  #                               keep = c("dawn", "sunriseEnd", "sunsetStart", "dusk")) # bere hranici 6° pod horizontem
  sun_times <- getSunlightTimes(date = unique_dates, lat = latitude, lon = longitude, tz = tz, 
                                keep = c("nauticalDawn", "sunriseEnd", "sunsetStart", "nauticalDusk")) # bere hranici 12° pod horizontem
  
  # Convert sun_times columns to POSIXct and extract hours for easier comparison
  # sun_times$nightEnd_hour <- as.numeric(format(as.POSIXct(sun_times$nightEnd, tz = tz), "%H"))
  # sun_times$nightEnd_hour <- as.numeric(format(as.POSIXct(sun_times$dawn, tz = tz), "%H"))
  sun_times$nightEnd_hour <- as.numeric(format(as.POSIXct(sun_times$nauticalDawn, tz = tz), "%H"))
  sun_times$sunriseEnd_hour <- as.numeric(format(as.POSIXct(sun_times$sunriseEnd, tz = tz), "%H"))
  sun_times$sunsetStart_hour <- as.numeric(format(as.POSIXct(sun_times$sunsetStart, tz = tz), "%H"))
  # sun_times$night_hour <- as.numeric(format(as.POSIXct(sun_times$night, tz = tz), "%H"))
  # sun_times$night_hour <- as.numeric(format(as.POSIXct(sun_times$dusknauticalDusk, tz = tz), "%H"))
  sun_times$night_hour <- as.numeric(format(as.POSIXct(sun_times$nauticalDusk, tz = tz), "%H"))
  
  # Handle cases where night times are missing or invalid
  sun_times$nightEnd_hour[is.na(sun_times$nightEnd_hour) | sun_times$nightEnd_hour == 0] <- 0  # Fallback to midnight
  sun_times$night_hour[is.na(sun_times$night_hour) | sun_times$night_hour == 0] <- 24  # Fallback to the end of the day
  
  # Assign the date column to be consistent with date.input
  sun_times$date <- as.Date(sun_times$date)
  
  # Merge sun_times with date.input by the date column
  date.input <- merge(date.input, sun_times[, c("date", "nightEnd_hour", "sunriseEnd_hour", "sunsetStart_hour", "night_hour")], 
                      by.x = "dates", by.y = "date", all.x = TRUE)
  
  # Vectorized assignment of day phases based on the hour of the day
  date.input$DayPhase <- ifelse(date.input$Hour <= date.input$nightEnd_hour, "Night", 
                                ifelse(date.input$Hour <= date.input$sunriseEnd_hour, "Sunrise", 
                                       ifelse(date.input$Hour <= date.input$sunsetStart_hour, "Day", 
                                              ifelse(date.input$Hour <= date.input$night_hour, "Sunset", "Night"))))
  
  sub.no.night.days<-sun_times[is.na(sun_times$nightEnd),]
  
  date.input$DayPhase[which(date.input$dates %in% sub.no.night.days$date & date.input$DayPhase=="Night")]<-NA
  
  # Handling any remaining NA values
  date.input$DayPhase[is.na(date.input$DayPhase) & date.input$Hour < 12] <- "Sunrise"
  date.input$DayPhase[is.na(date.input$DayPhase) & date.input$Hour >= 12] <- "Sunset"
  
  # date.input<-subset(date.input, select = -c("dates" , "nightEnd_hour", "sunriseEnd_hour", "sunsetStart_hour", "night_hour"))
  date.input <- date.input[, !(names(date.input) %in% c("dates" , "nightEnd_hour", "sunriseEnd_hour", "sunsetStart_hour", "night_hour"))]
  
  return(date.input)
}

## determine.growth.phase ####
#
# determines growth phase
# distinguishes: Growth, TWD, stem tissue refilling, and no change
#
# input is a dataset with dendrometer data
#
# output is a data.frame with the same structure as the input,
# extended by a column indicating growth phase
#
determine.growth.phase<-function(tree.data){
  
  ## Testing arguments
  # tree.data=dendrometer_data$BB_FASY_smallTrees
  
  tree.data$GrowthPhase<-NA
  tree.data$GrowthPhase[which(tree.data$Normalized_zero_increment>0)]<-"Growth"
  tree.data$GrowthPhase[which(tree.data$Normalized_increment>0 & tree.data$Normalized_zero_increment==0)]<-"Refiling"
  tree.data$GrowthPhase[which(tree.data$Normalized_increment<0)]<-"TWD"
  tree.data$GrowthPhase[which(tree.data$Normalized_increment==0)]<-"NoStatus"
  
  return(tree.data)
}

