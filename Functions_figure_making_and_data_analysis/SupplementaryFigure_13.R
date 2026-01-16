## Functions ####

## calculate.data ####
# Computes daily (DOY) stem growth, soil water potential (SWC),
# and temperature summaries using bootstrap confidence intervals.
# Output contains min / median / max estimates for each variable
# across all years and trees.
calculate.data<-function(input_tree_data,
                         input_swp_data,
                         input_temperature_data){
  
  ## testing arguments
  # input_tree_data=dendrometer_data$BB_FASY_bigTrees
  # input_swp_data=swp_data$Boubin
  # input_temperature_data=climate_data$Boubin_stationclim
  
  output<-data.frame(DOY=c(1:365),
                     min_stem=NA,
                     mid_stem=NA,
                     max_stem=NA,
                     min_temp=NA,
                     mid_temp=NA,
                     max_temp=NA,
                     min_swp=NA,
                     mid_swp=NA,
                     max_swp=NA)
  
  temp_tree_data<-aggregate(Normalized_growth~Tree+Year+DOY, data=input_tree_data, FUN=max)
  temp_swp_data<-aggregate(SWC~Year+DOY, data=input_swp_data, FUN=mean)
  temp_temperature_data<-aggregate(T~Year+DOY, data=input_temperature_data, FUN=mean)
  
  
  for(i in c(1:365)){
    sub_tree_data<-subset(temp_tree_data, DOY==i)
    
    if(nrow(sub_tree_data)>1){
      output[i,c("min_stem","mid_stem","max_stem")]<-quantile(apply(replicate(1000,sample(sub_tree_data$Normalized_growth,nrow(sub_tree_data),T)),2,mean),probs=c(0.025, 0.500, 0.975))
    }
    
    sub_swp_data<-subset(temp_swp_data, DOY==i)
    if(nrow(sub_swp_data)>1){
      output[i,c("min_swp","mid_swp","max_swp")]<-quantile(apply(replicate(1000,sample(sub_swp_data$SWC,nrow(sub_swp_data),T)),2,mean),probs=c(0.025, 0.500, 0.975))
    }
    
    sub_temperature_data<-subset(temp_temperature_data, DOY==i)
    if(nrow(sub_temperature_data)>1){
      output[i,c("min_temp","mid_temp","max_temp")]<-quantile(apply(replicate(1000,sample(sub_temperature_data$T,nrow(sub_temperature_data),T)),2,mean),probs=c(0.025, 0.500, 0.975))
    }
    
  }
  
  return(output)
  
}

## draw.graph ####
# Plots seasonal dynamics of stem growth together with temperature
# and soil water potential.
# Compares big vs. small trees using ribbons (CI) and median lines,
# with climate variables rescaled to a common axis.
draw.graph<-function(input_big, input_small, species){
  ## testing arguments
  # input_big=calculate.data(dendrometer_data$RN_ACCA_bigTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim)
  # input_small=calculate.data(dendrometer_data$RN_ACCA_smallTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim)
  # species="PCAB"
  
  
  input_big$Size="big"
  input_small$Size="small"
  input<-rbind(input_big, input_small)
  input$Species=species
  
  input$min_temp<-(input$min_temp+5)/25
  input$mid_temp<-(input$mid_temp+5)/25
  input$max_temp<-(input$max_temp+5)/25
  
  input$min_swp<-input$min_swp/10
  input$mid_swp<-input$mid_swp/10
  input$max_swp<-input$max_swp/10
  
  input$max_temp[which(input$max_temp>1.2)]<-1.2
  input$max_swp[which(input$max_swp>1.2)]<-1.2
  
  
  g<-ggplot(input)
  g<-g+geom_ribbon(aes(x=DOY,ymin=min_stem,ymax=max_stem, group=Size),fill=species.colors[species], alpha=0.1)
  g<-g+geom_line(aes(x=DOY,y=min_stem, linetype=Size, group=Size),colour=species.colors[species], linewidth=0.25)
  g<-g+geom_line(aes(x=DOY,y=max_stem, linetype=Size, group=Size),colour=species.colors[species], linewidth=0.25)
  g<-g+geom_line(aes(x=DOY,y=mid_stem, linetype=Size, group=Size),colour=species.colors[species], linewidth=0.75)
  g<-g+geom_ribbon(aes(x=DOY,ymin=min_temp,ymax=max_temp),fill="#E74C3C", alpha=0.25)
  g<-g+geom_line(aes(x=DOY,y=mid_temp),colour="#E74C3C")
  g<-g+geom_ribbon(aes(x=DOY,ymin=min_swp,ymax=max_swp),fill="#3498DB", alpha=0.25)
  g<-g+geom_line(aes(x=DOY,y=mid_swp),colour="#3498DB")
  g<-g+scale_x_continuous("Day of the Year",limits=c(100,275),breaks=seq(0,365,25))
  g<-g+scale_y_continuous("Stem increment (%)",limits=c(0,1.2),breaks=seq(0,1.2,0.2),labels=formatC(seq(0,1.2,0.2), format="f", digits=1))
  g<-my.theme(g,"none")
  g
}


## Figure finalization ####
figure<-ggarrange(draw.graph(calculate.data(dendrometer_data$RN_ACCA_bigTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim),
                             calculate.data(dendrometer_data$RN_ACCA_smallTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim),
                             "ACCA"),
                  draw.graph(calculate.data(dendrometer_data$RN_CABE_bigTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim),
                             calculate.data(dendrometer_data$RN_CABE_smallTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim),
                             "CABE"),
                  draw.graph(calculate.data(dendrometer_data$RN_ULSP_bigTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim),
                             calculate.data(dendrometer_data$RN_ULSP_smallTrees, swp_data$Ranspurk, climate_data$Ranspurk_stationclim),
                             "ULSP"),
                  draw.graph(calculate.data(dendrometer_data$ZF_PCAB_bigTrees, swp_data$Zofin, climate_data$Zofin_stationclim),
                             calculate.data(dendrometer_data$ZF_PCAB_smallTrees, swp_data$Zofin, climate_data$Zofin_stationclim),
                             "PCAB"),
                  draw.graph(calculate.data(dendrometer_data$ZF_FASY_bigTrees, swp_data$Zofin, climate_data$Zofin_stationclim),
                             calculate.data(dendrometer_data$ZF_FASY_smallTrees, swp_data$Zofin, climate_data$Zofin_stationclim),
                             "FASY"),
                  draw.graph(calculate.data(dendrometer_data$BB_PCAB_bigTrees, swp_data$Boubin, climate_data$Boubin_stationclim),
                             calculate.data(dendrometer_data$BB_PCAB_smallTrees, swp_data$Boubin, climate_data$Boubin_stationclim),
                             "PCAB"),
                  draw.graph(calculate.data(dendrometer_data$BB_FASY_bigTrees, swp_data$Boubin, climate_data$Boubin_stationclim),
                             calculate.data(dendrometer_data$BB_FASY_smallTrees, swp_data$Boubin, climate_data$Boubin_stationclim),
                             "FASY"),
                  draw.graph(calculate.data(dendrometer_data$EU_PCAB_bigTrees, swp_data$Eustaska, climate_data$Eustaska_stationclim),
                             calculate.data(dendrometer_data$EU_PCAB_smallTrees, swp_data$Eustaska, climate_data$Eustaska_stationclim),
                             "PCAB"),
                  nrow=4, ncol=2,align="hv",labels=LETTERS[1:10])


