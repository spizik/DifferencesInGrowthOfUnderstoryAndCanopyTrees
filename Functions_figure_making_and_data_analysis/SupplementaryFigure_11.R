## Functions ####

## my.theme ####
# Apply a consistent ggplot2 theme with black axes and configurable legend position.
my.theme<-function(graph,legend.pos="bottom"){
  graph<-graph+theme_classic()
  graph<-graph+theme(axis.line.x = element_line(colour="black"),
                     axis.text.x = element_text(colour="black"),
                     axis.line.y = element_line(colour="black"),
                     axis.text.y = element_text(colour="black"),
                     axis.ticks = element_line(color = "black"),
                     legend.position = legend.pos)
  graph
}

## add.x ####
# Assign numeric x-positions for site × species × size combinations
# to standardize ordering in plots (all trees).
add.x<-function(input){
  
  ## testing arguments
  # input=gro.periods.data
  
  input$grp<-paste0(input$Site,"_",input$Species,"_",input$Size)
  
  input$x<-NA
  input$x[which(input$grp=="Ranspurk_ACCA_big")]<-1
  input$x[which(input$grp=="Ranspurk_ACCA_small")]<-2
  input$x[which(input$grp=="Ranspurk_CABE_big")]<-3
  input$x[which(input$grp=="Ranspurk_CABE_small")]<-4
  input$x[which(input$grp=="Ranspurk_ULSP_big")]<-5
  input$x[which(input$grp=="Ranspurk_ULSP_small")]<-6
  
  input$x[which(input$grp=="Zofin_PCAB_big")]<-7
  input$x[which(input$grp=="Zofin_PCAB_small")]<-8
  input$x[which(input$grp=="Zofin_FASY_big")]<-9
  input$x[which(input$grp=="Zofin_FASY_small")]<-10
  
  input$x[which(input$grp=="Boubin_PCAB_big")]<-11
  input$x[which(input$grp=="Boubin_PCAB_small")]<-12
  input$x[which(input$grp=="Boubin_FASY_big")]<-13
  input$x[which(input$grp=="Boubin_FASY_small")]<-14
  
  input$x[which(input$grp=="Eustaska_PCAB_big")]<-15
  input$x[which(input$grp=="Eustaska_PCAB_small")]<-16
  
  return(input)
}

## add.x.podrost ####
# Assign numeric x-positions for understory (small trees only) combinations.
add.x.podrost<-function(input){
  
  ## testing arguments
  # input=gro.periods.data
  
  input$grp<-paste0(input$Site,"_",input$Species,"_",input$Size)
  
  input$x<-NA
  input$x[which(input$grp=="Ranspurk_ACCA_small")]<-1
  input$x[which(input$grp=="Ranspurk_CABE_small")]<-2
  input$x[which(input$grp=="Ranspurk_ULSP_small")]<-3
  
  input$x[which(input$grp=="Zofin_PCAB_small")]<-4
  input$x[which(input$grp=="Zofin_FASY_small")]<-5
  
  input$x[which(input$grp=="Boubin_PCAB_small")]<-6
  input$x[which(input$grp=="Boubin_FASY_small")]<-7
  
  input$x[which(input$grp=="Eustaska_PCAB_small")]<-8
  
  return(input)
}

## add.station.climate ####
# Join tree dendrometer data with station climate data by Year–DOY–Hour.
add.station.climate<-function(tree.data, clim.data){
  
  ## testing arguments
  # tree.data=dendrometer_data$RN_ACCA_bigTrees
  # clim.data=climate_data$Eustaska_stationclim
  
  tree.data$time<-paste0(tree.data$Year,"_",tree.data$DOY,"_",tree.data$Hour)
  clim.data$time<-paste0(clim.data$Year,"_",clim.data$DOY,"_",clim.data$Hour)
  
  joined.df<- tree.data %>% inner_join(clim.data, by='time')
  joined.df<-na.omit(joined.df)
  
  joined.df<-within(joined.df, rm("Year.y","DOY.y","Hour.y"))
  names(joined.df)<-gsub(".x","",names(joined.df))
  
  return(joined.df)
}

## add.podrost.climate ####
# Join understory tree data with forest (podrost) climate data by time.
add.podrost.climate<-function(tree.data, clim.data){
  
  ## testing arguments
  # tree.data=dendrometer_data$BB_FASY_smallTrees
  # clim.data=climate_data_forest$podrost_BB
  
  tree.data$time<-paste0(tree.data$Year,"_",tree.data$DOY,"_",tree.data$Hour)
  clim.data$time<-paste0(clim.data$Year,"_",clim.data$DOY,"_",clim.data$Hour)
  
  clim.data<-clim.data[,!names(clim.data) %in% c("Time", "Year", "DOY", "Month", "Day", "Hour", "Minute")]
  
  joined.df<- tree.data %>% inner_join(clim.data, by='time')
  joined.df<-na.omit(joined.df)
  
  return(joined.df)
}

## calc.VPD ####
# Calculate vapor pressure deficit (VPD) from temperature and humidity.
calc.VPD<-function(clim.data){
  
  ## testing arguments
  # clim.data=dataset.2$RN_ACCA_smallTrees
  
  es<-0.6108*exp((17.27*clim.data$T)/(clim.data$T+273.3))
  ea<-es*(clim.data$H/100)
  VPD<-es-ea
  
  return(VPD)
}

## determine.growth.phase ####
# Classify growth phase (Growth, Refiling, TWD, NoStatus)
# based on normalized increments.
determine.growth.phase<-function(tree.data){
  
  ## testing arguments
  # tree.data=dendrometer_data$BB_FASY_smallTrees
  
  tree.data$phase<-NA
  tree.data$phase[which(tree.data$Normalized_zero_increment>0)]<-"Growth"
  tree.data$phase[which(tree.data$Normalized_increment>0 & tree.data$Normalized_zero_increment==0)]<-"Refiling"
  tree.data$phase[which(tree.data$Normalized_increment<0)]<-"TWD"
  tree.data$phase[which(tree.data$Normalized_increment==0)]<-"NoStatus"
  
  return(tree.data)
}

## add.data ####
# Combine tree data with station climate and assign growth phase.
add.data<-function(tree.data, clim.data){
  
  ## testing arguments
  # tree.data=dendrometer_data$BB_FASY_smallTrees
  # clim.data=climate_data$Boubin_stationclim
  
  output<-add.station.climate(tree.data, clim.data)
  output<-determine.growth.phase(output)
  
  return(output)
}

## add.data.podrost ####
# Combine understory tree data with forest climate and assign growth phase.
add.data.podrost<-function(tree.data, clim.data){
  
  ## testing arguments
  # tree.data=dendrometer_data$BB_FASY_smallTrees
  # clim.data=climate_data_forest$podrost_BB
  
  output<-add.podrost.climate(tree.data, clim.data)
  output<-determine.growth.phase(output)
  
  return(output)
}

## complete.data.station ####
# Prepare complete datasets (tree + station climate) for all sites,
# species, and size classes.
complete.data.station<-function(in.tree.data){
  
  ## testing arguments
  # in.tree.data=dendrometer_data
  # in.clim.data=climate_data
  
  out<-list()
  
  # Boubin
  out$BB_FASY_bigTrees<-add.data(in.tree.data$BB_FASY_bigTrees, climate_data$Boubin_stationclim)
  out$BB_FASY_smallTrees<-add.data(in.tree.data$BB_FASY_smallTrees, climate_data$Boubin_stationclim)
  out$BB_PCAB_bigTrees<-add.data(in.tree.data$BB_PCAB_bigTrees, climate_data$Boubin_stationclim)
  out$BB_PCAB_smallTrees<-add.data(in.tree.data$BB_PCAB_smallTrees, climate_data$Boubin_stationclim)
  
  # Eustaska
  out$EU_PCAB_bigTrees<-add.data(in.tree.data$EU_PCAB_bigTrees, climate_data$Eustaska_stationclim)
  out$EU_PCAB_smallTrees<-add.data(in.tree.data$EU_PCAB_smallTrees, climate_data$Eustaska_stationclim)
  
  # Zofin
  out$ZF_FASY_bigTrees<-add.data(in.tree.data$ZF_FASY_bigTrees, climate_data$Zofin_stationclim)
  out$ZF_FASY_smallTrees<-add.data(in.tree.data$ZF_FASY_smallTrees, climate_data$Zofin_stationclim)
  out$ZF_PCAB_bigTrees<-add.data(in.tree.data$ZF_PCAB_bigTrees, climate_data$Zofin_stationclim)
  out$ZF_PCAB_smallTrees<-add.data(in.tree.data$ZF_PCAB_smallTrees, climate_data$Zofin_stationclim)
  
  # Zofin
  out$RN_ACCA_bigTrees<-add.data(in.tree.data$RN_ACCA_bigTrees, climate_data$Ranspurk_stationclim)
  out$RN_ACCA_smallTrees<-add.data(in.tree.data$RN_ACCA_smallTrees, climate_data$Ranspurk_stationclim)
  out$RN_CABE_bigTrees<-add.data(in.tree.data$RN_CABE_bigTrees, climate_data$Ranspurk_stationclim)
  out$RN_CABE_smallTrees<-add.data(in.tree.data$RN_CABE_smallTrees, climate_data$Ranspurk_stationclim)
  out$RN_ULSP_bigTrees<-add.data(in.tree.data$RN_ULSP_bigTrees, climate_data$Ranspurk_stationclim)
  out$RN_ULSP_smallTrees<-add.data(in.tree.data$RN_ULSP_smallTrees, climate_data$Ranspurk_stationclim)
  
  return(out)
  
}

## complete.data.small ####
# Prepare complete datasets for small trees using forest (podrost) climate.
complete.data.small<-function(in.tree.data){
  
  ## testing arguments
  # in.tree.data=dendrometer_data
  # in.clim.data=climate_data_forest
  
  out<-list()
  
  # Boubin
  out$BB_FASY_smallTrees<-add.data(in.tree.data$BB_FASY_smallTrees, climate_data_forest$podrost_BB)
  out$BB_PCAB_smallTrees<-add.data(in.tree.data$BB_PCAB_smallTrees, climate_data_forest$podrost_BB)
  
  # Eustaska
  out$EU_PCAB_smallTrees<-add.data(in.tree.data$EU_PCAB_smallTrees, climate_data_forest$podrost_EU)
  
  # Zofin
  out$ZF_FASY_smallTrees<-add.data(in.tree.data$ZF_FASY_smallTrees, climate_data_forest$podrost_ZF)
  out$ZF_PCAB_smallTrees<-add.data(in.tree.data$ZF_PCAB_smallTrees, climate_data_forest$podrost_ZF)
  
  # Zofin
  out$RN_ACCA_smallTrees<-add.data(in.tree.data$RN_ACCA_smallTrees, climate_data_forest$podrost_RN)
  out$RN_CABE_smallTrees<-add.data(in.tree.data$RN_CABE_smallTrees, climate_data_forest$podrost_RN)
  out$RN_ULSP_smallTrees<-add.data(in.tree.data$RN_ULSP_smallTrees, climate_data_forest$podrost_RN)
  
  return(out)
  
}

## boxplot.climvars ####
# Plot climate variables by growth phase and tree size
# with significance testing between size classes.
boxplot.climvars<-function(input.big, input.small, variable){
  
  ## testing arguments
  # input.big=dataset$BB_FASY_bigTrees
  # input.small=dataset$BB_FASY_smallTrees
  # variable="T"
  
  input.big$size="big"
  input.small$size="small"
  
  show.data<-rbind(input.big, input.small)
  
  show.data$x<-NA
  
  show.data$x[which(show.data$phase=="Growth")]<-1
  show.data$x[which(show.data$phase=="Refiling")]<-2
  show.data$x[which(show.data$phase=="TWD")]<-3
  show.data$x[which(show.data$phase=="NoStatus")]<-4
  
  show.data$x[which(show.data$size=="big")]<-show.data$x[which(show.data$size=="big")]-0.2
  show.data$x[which(show.data$size=="small")]<-show.data$x[which(show.data$size=="small")]+0.2
  
  show.data<-show.data[,c("phase", "size", "x", variable)]
  names(show.data)<-c("phase", "size", "x", "variable")
  
  if(variable=="VPD") sigg.y<-1.25
  if(variable=="T") sigg.y<-27.5
  if(variable=="R") sigg.y<-275
  
  g<-ggplot(show.data)
  g<-g+geom_boxplot(aes(x=x, y=variable, alpha=size, fill=phase))
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_fill_manual(breaks=names(cols.phases), values=cols.phases)
  g<-g+scale_x_continuous("", limits=c(0.5,4.5),breaks=c(1:4),labels=c("Growth", "Refiling", "TWD", "No change"))
  if(kruskal.test(variable~size, data=subset(show.data,phase=="Growth"))$p.value<0.05) g<-g+annotate("text",x=1,y=sigg.y, label="*", size=10, colour=cols.phases["Growth"])
  if(kruskal.test(variable~size, data=subset(show.data,phase=="Refiling"))$p.value<0.05) g<-g+annotate("text",x=2,y=sigg.y, label="*", size=10, colour=cols.phases["Refiling"])
  if(kruskal.test(variable~size, data=subset(show.data,phase=="TWD"))$p.value<0.05) g<-g+annotate("text",x=3,y=sigg.y, label="*", size=10, colour=cols.phases["TWD"])
  if(kruskal.test(variable~size, data=subset(show.data,phase=="NoStatus"))$p.value<0.05) g<-g+annotate("text",x=4,y=sigg.y, label="*", size=10, colour=cols.phases["NoStatus"])
  if(variable=="VPD") g<-g+scale_y_continuous("VPD (kPa)",limits=c(0,1.5), breaks=seq(0,3,0.25),labels=formatC(seq(0,3,0.25), format="f", digits=2))
  if(variable=="T") g<-g+scale_y_continuous("Temperature (°C)",limits=c(0,30), breaks=seq(0,50,5))
  if(variable=="R") g<-g+scale_y_continuous("Solar irradiance",limits=c(0,300), breaks=seq(0,300,50))
  g<-my.theme(g)
  g
  
}

## plot.temp ####
# Boxplot temperature during Growth vs. TWD across sites and species.
plot.temp<-function(input){
  
  ## testing arguments
  # input=dataset
  
  input<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  
  input$x[which(input$GrowthPhase=="Growth")]<-input$x[which(input$GrowthPhase=="Growth")]-0.2
  input$x[which(input$GrowthPhase=="TWD")]<-input$x[which(input$GrowthPhase=="TWD")]+0.2
  
  input$grp<-paste0(input$grp,"_",input$GrowthPhase)
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x, y=T, alpha=Size, fill=Species, group=grp, linetype=GrowthPhase), outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_linetype_manual(breaks=c("Growth","TWD"), values=c("solid","dotted"))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_x_continuous("",
                          limits=c(0.5,16.5),
                          breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                          labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
  
  g<-g+scale_y_continuous("Temperature (°C)",limits=c(0,30), breaks=seq(0,50,5))
  g<-my.theme(g)
  g
  
}

## plot.temp.podrost ####
# Same as plot.temp, but for understory trees only.
plot.temp.podrost<-function(input){
  
  ## testing arguments
  # input=dataset.2
  
  input<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  
  input$x[which(input$GrowthPhase=="Growth")]<-input$x[which(input$GrowthPhase=="Growth")]-0.2
  input$x[which(input$GrowthPhase=="TWD")]<-input$x[which(input$GrowthPhase=="TWD")]+0.2
  
  input$grp<-paste0(input$grp,"_",input$GrowthPhase)
  
  sigg.y<-27.5
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x, y=T, alpha=Size, fill=Species, group=grp, linetype=GrowthPhase), outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_linetype_manual(breaks=c("Growth","TWD"), values=c("solid","dotted"))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  g<-g+geom_vline(xintercept = c(3.5, 5.5, 7.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_x_continuous("",
                          limits=c(0.5,8.5),
                          breaks=c(1:8),
                          labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
  g<-g+scale_y_continuous("Temperature (°C)",limits=c(0,30), breaks=seq(0,50,5))
  g<-my.theme(g)
  g
  
}

## plot.vpd ####
# Boxplot VPD during Growth vs. TWD across sites and species.
plot.vpd<-function(input){
  
  ## testing arguments
  # input=dataset
  
  input<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  
  input$x[which(input$GrowthPhase=="Growth")]<-input$x[which(input$GrowthPhase=="Growth")]-0.2
  input$x[which(input$GrowthPhase=="TWD")]<-input$x[which(input$GrowthPhase=="TWD")]+0.2
  
  input$grp<-paste0(input$grp,"_",input$GrowthPhase)
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x, y=VPD, alpha=Size, fill=Species, group=grp, linetype=GrowthPhase), outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_linetype_manual(breaks=c("Growth","TWD"), values=c("solid","dotted"))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_x_continuous("",
                          limits=c(0.5,16.5),
                          breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                          labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
  g<-g+scale_y_continuous("VPD (kPa)",limits=c(0,1.5), breaks=seq(0,3,0.25),labels=formatC(seq(0,3,0.25), format="f", digits=2))
  g<-my.theme(g)
  g
  
}

## plot.vpd.podrost ####
# Same as plot.vpd, but for understory trees only.
plot.vpd.podrost<-function(input){
  
  ## testing arguments
  # input=dataset
  
  input<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  
  input$x[which(input$GrowthPhase=="Growth")]<-input$x[which(input$GrowthPhase=="Growth")]-0.2
  input$x[which(input$GrowthPhase=="TWD")]<-input$x[which(input$GrowthPhase=="TWD")]+0.2
  
  input$grp<-paste0(input$grp,"_",input$GrowthPhase)
  
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x, y=VPD, alpha=Size, fill=Species, group=grp, linetype=GrowthPhase), outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_linetype_manual(breaks=c("Growth","TWD"), values=c("solid","dotted"))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  g<-g+geom_vline(xintercept = c(3.5, 5.5, 7.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_x_continuous("",
                          limits=c(0.5,8.5),
                          breaks=c(1:8),
                          labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
  g<-g+scale_y_continuous("VPD (kPa)",limits=c(0,1.5), breaks=seq(0,3,0.25),labels=formatC(seq(0,3,0.25), format="f", digits=2))
  g<-my.theme(g)
  g
  
}

## plot.rad ####
# Boxplot solar irradiance during Growth vs. TWD across sites and species.
plot.rad<-function(input){
  
  ## testing arguments
  # input=dataset
  
  input<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  
  input$x[which(input$GrowthPhase=="Growth")]<-input$x[which(input$GrowthPhase=="Growth")]-0.2
  input$x[which(input$GrowthPhase=="TWD")]<-input$x[which(input$GrowthPhase=="TWD")]+0.2
  
  input$grp<-paste0(input$grp,"_",input$GrowthPhase)
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x, y=R, alpha=Size, fill=Species, group=grp, linetype=GrowthPhase), outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_linetype_manual(breaks=c("Growth","TWD"), values=c("solid","dotted"))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_x_continuous("",
                          limits=c(0.5,16.5),
                          breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                          labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
  g<-g+scale_y_continuous("Solar irradiance",limits=c(0,300), breaks=seq(0,300,50))
  g<-my.theme(g)
  g
  
}

## plot.P ####
# Boxplot precipitation during Growth vs. TWD across sites and species.
plot.P<-function(input){
  
  ## testing arguments
  # input=dataset
  
  input<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  
  input$x[which(input$GrowthPhase=="Growth")]<-input$x[which(input$GrowthPhase=="Growth")]-0.2
  input$x[which(input$GrowthPhase=="TWD")]<-input$x[which(input$GrowthPhase=="TWD")]+0.2
  
  input$grp<-paste0(input$grp,"_",input$GrowthPhase)
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x, y=P, alpha=Size, fill=Species, group=grp, linetype=GrowthPhase), outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_linetype_manual(breaks=c("Growth","TWD"), values=c("solid","dotted"))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_x_continuous("",
                          limits=c(0.5,16.5),
                          breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                          labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
  g<-my.theme(g)
  g
  
}

## test.differences ####
# Perform Dunn post-hoc tests comparing size × growth-phase groups
# for radiation and VPD.
test.differences<-function(input){
  
  ## testing arguments
  # input=subset(dataset,Site=="Eustaska" & Species=="PCAB")
  
  sub<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  sub$grp<-paste0(sub$Size,"_",sub$GrowthPhase)
  test<-dunnTest(R~grp, data=sub)
  test<-test$res[,c(1,4)]
  test[,2]<-round(test[,2],3)
  output<-test
  
  sub<-subset(input, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  sub$grp<-paste0(sub$Size,"_",sub$GrowthPhase)
  test<-dunnTest(R~grp, data=sub)
  test<-test$res[,c(1,4)]
  test[,2]<-round(test[,2],3)
  output<-rbind(output,test)
  
  sub<-subset(sub, GrowthPhase=="Growth" | GrowthPhase=="TWD")
  sub$grp<-paste0(sub$Size,"_",sub$GrowthPhase)
  test<-dunnTest(VPD~grp, data=sub)
  test<-test$res[,c(1,4)]
  test[,2]<-round(test[,2],3)
  output<-rbind(output,test)
  
  return(output)
}

# Data preparation - statin data ####
dataset<-complete.data.station(dendrometer_data)
dataset$RN_ACCA_bigTrees$Size<-"big"
dataset$RN_CABE_bigTrees$Size<-"big"
dataset$RN_ULSP_bigTrees$Size<-"big"
dataset$ZF_PCAB_bigTrees$Size<-"big"
dataset$ZF_FASY_bigTrees$Size<-"big"
dataset$BB_PCAB_bigTrees$Size<-"big"
dataset$BB_FASY_bigTrees$Size<-"big"
dataset$EU_PCAB_bigTrees$Size<-"big"

dataset$RN_ACCA_smallTrees$Size<-"small"
dataset$RN_CABE_smallTrees$Size<-"small"
dataset$RN_ULSP_smallTrees$Size<-"small"
dataset$ZF_PCAB_smallTrees$Size<-"small"
dataset$ZF_FASY_smallTrees$Size<-"small"
dataset$BB_PCAB_smallTrees$Size<-"small"
dataset$BB_FASY_smallTrees$Size<-"small"
dataset$EU_PCAB_smallTrees$Size<-"small"

dataset$RN_ACCA_bigTrees$Site<-"Ranspurk"
dataset$RN_CABE_bigTrees$Site<-"Ranspurk"
dataset$RN_ULSP_bigTrees$Site<-"Ranspurk"
dataset$RN_ACCA_smallTrees$Site<-"Ranspurk"
dataset$RN_CABE_smallTrees$Site<-"Ranspurk"
dataset$RN_ULSP_smallTrees$Site<-"Ranspurk"

dataset<-do.call(rbind, dataset)

# Data preparation - understory data ####
to.keep<-c("Species", "Site", "Size", "GrowthPhase")
new.names<-c("Species", "Site", "Size", "GrowthPhase", "T", "H")

dataset.2<-complete.data.small(dendrometer_data)
dataset.2$RN_ACCA_smallTrees$Size<-"small"
dataset.2$RN_CABE_smallTrees$Size<-"small"
dataset.2$RN_ULSP_smallTrees$Size<-"small"
dataset.2$ZF_PCAB_smallTrees$Size<-"small"
dataset.2$ZF_FASY_smallTrees$Size<-"small"
dataset.2$BB_PCAB_smallTrees$Size<-"small"
dataset.2$BB_FASY_smallTrees$Size<-"small"
dataset.2$EU_PCAB_smallTrees$Size<-"small"

dataset.2$RN_ACCA_smallTrees$Site<-"Ranspurk" ;dataset.2$RN_ACCA_smallTrees<-dataset.2$RN_ACCA_smallTrees[,c(to.keep, "T_log49", "H_log49")] ; names(dataset.2$RN_ACCA_smallTrees)<-new.names
dataset.2$RN_CABE_smallTrees$Site<-"Ranspurk" ;dataset.2$RN_CABE_smallTrees<-dataset.2$RN_CABE_smallTrees[,c(to.keep, "T_log49", "H_log49")] ; names(dataset.2$RN_CABE_smallTrees)<-new.names
dataset.2$RN_ULSP_smallTrees$Site<-"Ranspurk" ;dataset.2$RN_ULSP_smallTrees<-dataset.2$RN_ULSP_smallTrees[,c(to.keep, "T_log49", "H_log49")] ; names(dataset.2$RN_ULSP_smallTrees)<-new.names
dataset.2$BB_PCAB_smallTrees$Site<-"Boubin" ;dataset.2$BB_PCAB_smallTrees<-dataset.2$BB_PCAB_smallTrees[,c(to.keep, "T_log64", "H_log64")] ; names(dataset.2$BB_PCAB_smallTrees)<-new.names
dataset.2$BB_FASY_smallTrees$Site<-"Boubin" ;dataset.2$BB_FASY_smallTrees<-dataset.2$BB_FASY_smallTrees[,c(to.keep, "T_log64", "H_log64")] ; names(dataset.2$BB_FASY_smallTrees)<-new.names
dataset.2$EU_PCAB_smallTrees$Site<-"Eustaska" ;dataset.2$EU_PCAB_smallTrees<-dataset.2$EU_PCAB_smallTrees[,c(to.keep, "T_log20", "H_log20")] ; names(dataset.2$EU_PCAB_smallTrees)<-new.names

dataset.2$ZF_PCAB_smallTrees$Site<-"Zofin" ;dataset.2$ZF_PCAB_smallTrees<-dataset.2$ZF_PCAB_smallTrees[,c(to.keep,"T_log13", "H_log13","T_log10", "H_log10")] 
dataset.2$ZF_FASY_smallTrees$Site<-"Zofin" ;dataset.2$ZF_FASY_smallTrees<-dataset.2$ZF_FASY_smallTrees[,c(to.keep,"T_log13", "H_log13","T_log10", "H_log10")] 
dataset.2$ZF_PCAB_smallTrees$T<-rowMeans(dataset.2$ZF_PCAB_smallTrees[,c("T_log13","T_log10")], na.rm=T)
dataset.2$ZF_FASY_smallTrees$T<-rowMeans(dataset.2$ZF_FASY_smallTrees[,c("T_log13","T_log10")], na.rm=T)
dataset.2$ZF_PCAB_smallTrees$H<-rowMeans(dataset.2$ZF_PCAB_smallTrees[,c("H_log13","H_log10")], na.rm=T)
dataset.2$ZF_FASY_smallTrees$H<-rowMeans(dataset.2$ZF_FASY_smallTrees[,c("H_log13","H_log10")], na.rm=T)
dataset.2$ZF_PCAB_smallTrees<-dataset.2$ZF_PCAB_smallTrees[,!(colnames(dataset.2$ZF_PCAB_smallTrees) %in% c("T_log13", "H_log13","T_log10", "H_log10"))]
dataset.2$ZF_FASY_smallTrees<-dataset.2$ZF_FASY_smallTrees[,!(colnames(dataset.2$ZF_FASY_smallTrees) %in% c("T_log13", "H_log13","T_log10", "H_log10"))]

dataset.2<-do.call(rbind, dataset.2)

## VPD calculations
dataset$VPD<-calc.VPD(dataset) 
dataset.2$VPD<-calc.VPD(dataset.2) 

# Figures finalization ####
dataset<-add.x(dataset)
dataset.2<-add.x.podrost(dataset.2)

dataset$GrowthPhase[which(dataset$GrowthPhase!="Growth")]<-"TWD"
dataset.2$GrowthPhase[which(dataset.2$GrowthPhase!="Growth")]<-"TWD"


figure<-ggarrange(plot.temp(dataset),
                  plot.temp.podrost(dataset.2),
                  plot.vpd(dataset),
                  plot.vpd.podrost(dataset.2),
                  plot.rad(dataset),
                  nrow=3,ncol=2,align="hv",labels=LETTERS[1:5],common.legend=T,legend="bottom",widths=c(0.66,0.34))


















