## Functions ####

## my.theme ####
# Apply a classic ggplot2 theme with black axes and a configurable legend position.
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
# Assign fixed numeric x-axis positions based on site, species, and tree size for plotting.
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

## calculate.growing.periods ####
# Calculate the proportion of time spent in active growth (positive increment) per tree and year.
calculate.growing.periods<-function(input,site,size){
  
  ## testing arguments
  # input=dendrometer_data$EU_PCAB_smallTrees
  # site="Eustaska"
  # size="small"

  growing.periods<-aggregate(Normalized_increment~Tree + Year , data=input, FUN=length)
  names(growing.periods)<-c("Tree","Year","NoGrPeriods")
  temp<-aggregate(Normalized_increment~Tree + Year , data=subset(input, Normalized_zero_increment>0), FUN=length)
  growing.periods<-merge(growing.periods,temp, by = c("Tree", "Year"), all = TRUE)
  names(growing.periods)<-c("Tree", "Year", "NoGrPeriods", "NoGrHours")
  
  
  growing.periods$PercGrPeriods<-growing.periods$NoGrHours/growing.periods$NoGrPeriods
  
  growing.periods$Site<-site
  growing.periods$Species<-unique(input$Species)
  growing.periods$Size<-size
  
  growing.periods<-na.omit(growing.periods)
  
  return(growing.periods)
}

## test.differences ####
# Test size-related differences in the proportion of growing periods between big and small trees for a given site and species.
test.differences<-function(site, species, test="lmer"){
  
  ## testing arguments
  # site="Eustaska"
  # species="PCAB"
  # test="lmer"
  
  input=subset(dataset,Site==site & Species==species)
  
  if(test=="anova"){
    gr.hours<-summary(aov(PercGrPeriods~Size,data=input))
    
    gr.hours<-round(gr[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    gr.hours<-kruskal.test(PercGrPeriods~Size,data=input)
    
    gr.hours<-round(gr.hours$p.value,3)
  }
  if(test=="wilcox"){
    gr.hours<-wilcox.test(PercGrPeriods~Size,data=input)
    
    gr.hours<-round(gr$p.value,3)
  }
  if(test=="lmer"){
    gr.hours <- summary(lmer(PercGrPeriods   ~ Size + (1|Tree) + (1|Year), data = input))
    
    gr.hours<-round(gr.hours$coefficients[,"Pr(>|t|)"][2],3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     gr=gr.hours)
  return(output)
}

## Calculations ####
dataset<-rbind(calculate.growing.periods(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
               calculate.growing.periods(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small"),
               calculate.growing.periods(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
               calculate.growing.periods(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small"),
               calculate.growing.periods(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
               calculate.growing.periods(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small"),
               
               calculate.growing.periods(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
               calculate.growing.periods(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small"),
               calculate.growing.periods(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
               calculate.growing.periods(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small"),
               
               calculate.growing.periods(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
               calculate.growing.periods(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small"),
               calculate.growing.periods(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
               calculate.growing.periods(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small"),
               
               calculate.growing.periods(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
               calculate.growing.periods(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"))

## Testing differences ####
differences<-rbind(test.differences("Ranspurk","ACCA"),
                   test.differences("Ranspurk","CABE"),
                   test.differences("Ranspurk","ULSP"),
                   test.differences("Zofin","PCAB"),
                   test.differences("Zofin","FASY"),
                   test.differences("Boubin","PCAB"),
                   test.differences("Boubin","FASY"),
                   test.differences("Eustaska","PCAB"))


print("------------- Differences -------------")
print(differences)

per<-aggregate(PercGrPeriods ~ Site + Species + Size, dataset, mean)
per$PercGrPeriods<-round(per$PercGrPeriods*100,1)
print(per)

aggregate(PercGrPeriods ~ Size + Species, subset(dataset, Site=="Boubin"), mean)
aggregate(PercGrPeriods ~ Size + Species, subset(dataset, Site=="Zofin"), mean)

aggregate(PercGrPeriods ~ Size + Species, subset(dataset, Site=="Ranspurk"), mean)

print("---------------------------------------")

## Figure ####
# dataset<-add.x.old(dataset)
dataset<-add.x(dataset)


g<-ggplot(dataset)
g<-g+geom_boxplot(aes(x=x, y=PercGrPeriods, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)

# g<-g+geom_vline(xintercept = c(4.5, 8.5, 12.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
# g<-g+scale_x_continuous("",
#                         limits=c(0.5,14.5),
#                         breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5),
#                         labels=c("Ranspurk ACCA", "Ranspurk CABE", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Number of growing periods (%)",limits=c(0,0.75),breaks=seq(0,1,0.15),labels=seq(0,100,15))
g<-my.theme(g)
figure<-g
