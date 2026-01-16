## Functions ####

## my.theme ####
# Apply a classic ggplot2 theme with black axes and configurable legend position.
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

## calculate.GS.length.data ####
# Calculate mean or median growing season start, end, and length per year.
calculate.GS.length.data<-function(input, site, size, variable="mean"){
  
  ## testing arguments
  # input=dendrometer_data$EU_PCAB_bigTrees
  # site="Eustaska"
  # size="big"
  # variable="mean"
  
  # input<-subset(input,Year==season)
  input$Time<-input$DOY + input$Hour/24
  
  temp<-aggregate(Time~Tree+Year, data=input,FUN=min)
  names(temp)<-c("Tree","Year","beginning")
  temp$ending<-aggregate(Time~Tree+Year, data=input,FUN=max)$Time
  temp$length<-temp$ending-temp$beginning
  
  if(variable=="mean"){
    output<-aggregate(beginning~Year,temp,mean)
    output$ending<-aggregate(ending~Year,temp,mean)$ending
    all<-data.frame(Year="All",beginning=mean(output$beginning),ending=mean(output$ending))
    output<-rbind(all,output)
  }
  if(variable=="median"){
    output<-aggregate(beginning~Year,temp,median)
    output$ending<-aggregate(ending~Year,temp,median)$ending
    all<-data.frame(Year="All",beginning=median(output$beginning),ending=median(output$ending))
    output<-rbind(all,output)
  }
  
  output$Site<-site
  output$Species<-unique(input$Species)
  output$Size<-size
  
  return(output)
}

## plot.GS.one.species ####
# Plot growing season timing across years for one species and size classes.
plot.GS.one.species<-function(show.data, since=2018){
  
  ## testing arguments
  # show.data=subset(dataset,Site=="Eustaska")
  # since=2019
  
  years<-na.omit(as.numeric(as.character(show.data$Year)))
  show.data$y<-show.data$Year
  show.data$y[which(show.data$y=="All")]<-max(years)+1
  show.data$y<-as.numeric(as.character(show.data$y))
  show.data$y<-show.data$y-since+1
  
  show.data$grp<-paste0(show.data$Year,"_",show.data$Species,"_",show.data$Size)
  
  show.data$y[which(show.data$Size=="big")]<-show.data$y[which(show.data$Size=="big")]+0.1
  show.data$y[which(show.data$Size=="small")]<-show.data$y[which(show.data$Size=="small")]-0.1
  
  g<-ggplot(show.data)
  g<-g+geom_linerange(aes(xmin=beginning, xmax=ending, y=y, group=grp, colour=Species, alpha=Size), linewidth=1.1)
  g<-g+geom_point(aes(x=beginning, y=y, group=grp, colour=Species, alpha=Size), size=2.5)
  g<-g+geom_point(aes(x=ending, y=y, group=grp, colour=Species, alpha=Size), size=2.5)
  g<-g+geom_hline(yintercept=seq(0.5,10.5,1),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_colour_manual(breaks=names(species.colors), values=species.colors)
  g<-g+scale_x_continuous("DOY",limits=c(100,275), breaks=seq(0,365,25))
  g<-g+scale_y_continuous("Growing season",limits=c(0,9), breaks=c(1:8), labels=c(since:c(max(years)),"All"))
  g<-my.theme(g)
  g
}

## plot.GS.two.species ####
# Plot growing season timing across years for two species and size classes.
plot.GS.two.species<-function(show.data, since=2018){
  
  ## testing arguments
  # show.data=subset(dataset,Site=="Boubin")
  # since=2019
  
  years<-na.omit(as.numeric(as.character(show.data$Year)))
  show.data$y<-show.data$Year
  show.data$y[which(show.data$y=="All")]<-max(years)+1
  show.data$y<-as.numeric(as.character(show.data$y))
  show.data$y<-show.data$y-since+1
  
  show.data<-subset(show.data,y>0)
  
  show.data$grp<-paste0(show.data$Year,"_",show.data$Species,"_",show.data$Size)
  
  spp<-unique(show.data$Species)
  show.data$y[which(show.data$Species==spp[1])]<-show.data$y[which(show.data$Species==spp[1])]+0.2
  show.data$y[which(show.data$Species==spp[2])]<-show.data$y[which(show.data$Species==spp[2])]-0.2
  show.data$y[which(show.data$Size=="big")]<-show.data$y[which(show.data$Size=="big")]+0.1
  show.data$y[which(show.data$Size=="small")]<-show.data$y[which(show.data$Size=="small")]-0.1
  
  g<-ggplot(show.data)
  g<-g+geom_linerange(aes(xmin=beginning, xmax=ending, y=y, group=grp, colour=Species, alpha=Size), linewidth=1.1)
  g<-g+geom_point(aes(x=beginning, y=y, group=grp, colour=Species, alpha=Size), size=2.5)
  g<-g+geom_point(aes(x=ending, y=y, group=grp, colour=Species, alpha=Size), size=2.5)
  g<-g+geom_hline(yintercept=seq(0.5,10.5,1),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_colour_manual(breaks=names(species.colors), values=species.colors)
  g<-g+scale_x_continuous("DOY",limits=c(100,275), breaks=seq(0,365,25))
  g<-g+scale_y_continuous("Growing season",limits=c(0,9), breaks=c(1:8), labels=c(since:c(max(years)),"All"))
  g<-my.theme(g)
  g
}

## plot.GS.three.species ####
# Plot growing season timing across years for three species and size classes.
plot.GS.three.species<-function(show.data, since=2018){
  
  ## testing arguments
  # show.data=subset(dataset,Site=="Ranspurk")
  # since=2018
  
  years<-na.omit(as.numeric(as.character(show.data$Year)))
  show.data$y<-show.data$Year
  show.data$y[which(show.data$y=="All")]<-max(years)+1
  show.data$y<-as.numeric(as.character(show.data$y))
  show.data$y<-show.data$y-since+1
  
  show.data<-subset(show.data,y>0)
  
  show.data$grp<-paste0(show.data$Year,"_",show.data$Species,"_",show.data$Size)
  
  spp<-unique(show.data$Species)
  show.data$y[which(show.data$Species==spp[1])]<-show.data$y[which(show.data$Species==spp[1])]+0.3
  show.data$y[which(show.data$Species==spp[2])]<-show.data$y[which(show.data$Species==spp[2])]-0.0
  show.data$y[which(show.data$Species==spp[2])]<-show.data$y[which(show.data$Species==spp[3])]-0.3
  show.data$y[which(show.data$Size=="big")]<-show.data$y[which(show.data$Size=="big")]+0.075
  show.data$y[which(show.data$Size=="small")]<-show.data$y[which(show.data$Size=="small")]-0.075
  
  g<-ggplot(show.data)
  g<-g+geom_linerange(aes(xmin=beginning, xmax=ending, y=y, group=grp, colour=Species, alpha=Size), linewidth=1.1)
  g<-g+geom_point(aes(x=beginning, y=y, group=grp, colour=Species, alpha=Size), size=2.5)
  g<-g+geom_point(aes(x=ending, y=y, group=grp, colour=Species, alpha=Size), size=2.5)
  g<-g+geom_hline(yintercept=seq(0.5,10.5,1),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_colour_manual(breaks=names(species.colors), values=species.colors)
  g<-g+scale_x_continuous("DOY",limits=c(100,275), breaks=seq(0,365,25))
  g<-g+scale_y_continuous("Growing season",limits=c(0,9), breaks=c(1:8), labels=c(since:c(max(years)),"All"))
  g<-my.theme(g)
  g
}

## summ.trees ####
# Summarize the number of sampled big and small trees per year.
summ.trees<-function(input_big, input_small){
  
  ## testing arguments
  # input_big=dendrometer_data$RN_ACCA_bigTrees
  # input_small=dendrometer_data$RN_ACCA_smallTrees
  
  big<-aggregate(Tree~Year,input_big,function(x){length(unique(x))})
  small<-aggregate(Tree~Year,input_small,function(x){length(unique(x))})
  
  output<-left_join(big, small, by = "Year")
  names(output)<-c("Year","Big","Small")
  return(output)
}

## Calculations ####
dataset<-rbind(calculate.GS.length.data(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
               calculate.GS.length.data(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small"),
               calculate.GS.length.data(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
               calculate.GS.length.data(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small"),
               calculate.GS.length.data(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
               calculate.GS.length.data(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small"),
               
               calculate.GS.length.data(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
               calculate.GS.length.data(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small"),
               calculate.GS.length.data(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
               calculate.GS.length.data(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small"),
               
               calculate.GS.length.data(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
               calculate.GS.length.data(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small"),
               calculate.GS.length.data(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
               calculate.GS.length.data(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small"),
               
               calculate.GS.length.data(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
               calculate.GS.length.data(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"))


## Printing results ####
print("RN - ACCA")
print(summ.trees(dendrometer_data$RN_ACCA_bigTrees, dendrometer_data$RN_ACCA_smallTrees))
print("RN - CABE")
print(summ.trees(dendrometer_data$RN_CABE_bigTrees, dendrometer_data$RN_CABE_smallTrees))
print("RN - ULSP")
print(summ.trees(dendrometer_data$RN_ULSP_bigTrees, dendrometer_data$RN_ULSP_smallTrees))

print("ZF - PCAB")
print(summ.trees(dendrometer_data$ZF_PCAB_bigTrees, dendrometer_data$ZF_PCAB_smallTrees))
print("ZF - FASY")
print(summ.trees(dendrometer_data$ZF_FASY_bigTrees, dendrometer_data$ZF_FASY_smallTrees))

print("BB - PCAB")
print(summ.trees(dendrometer_data$BB_PCAB_bigTrees, dendrometer_data$BB_PCAB_smallTrees))
print("BB - FASY")
print(summ.trees(dendrometer_data$BB_FASY_bigTrees, dendrometer_data$BB_FASY_smallTrees))

print("EU - PCAB")
print(summ.trees(dendrometer_data$EU_PCAB_bigTrees, dendrometer_data$EU_PCAB_smallTrees))

## Figure finalization ####
figure<-ggarrange(plot.GS.three.species(subset(dataset,Site=="Ranspurk")),
                  plot.GS.two.species(subset(dataset,Site=="Zofin")),
                  plot.GS.two.species(subset(dataset,Site=="Boubin")),
                  plot.GS.one.species(subset(dataset,Site=="Eustaska")),
                  nrow=1,ncol=4,labels=LETTERS[1:5],legend="bottom", common.legend=T)





