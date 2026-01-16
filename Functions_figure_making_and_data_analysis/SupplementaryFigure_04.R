## Functions ####

## my.theme ####
# Apply a clean classic ggplot2 theme with black axes and adjustable legend position.
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
# Assign fixed numeric x positions based on site, species, and size combinations for plotting.
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
# Calculate yearly proportions of growth-related phases (growth, TWD, refilling, no change) for each tree.
calculate.growing.periods<-function(input,site,size){
  
  ## testing arguments
  # input=dendrometer_data$EU_PCAB_smallTrees
  # site="Eustaska"
  # size="small"
  
  growing.periods<-aggregate(Normalized_increment~Tree + Year , data=input, FUN=length)
  names(growing.periods)<-c("Tree","Year","NoGrPeriods")
  
  temp<-aggregate(Normalized_increment~Tree + Year , data=subset(input, GrowthPhase=="Growth"), FUN=length)
  growing.periods<-merge(growing.periods,temp, by = c("Tree", "Year"), all = TRUE)
  names(growing.periods)<-c("Tree", "Year", "NoPeriods", "Growth")
  
  growing.periods$NoGroHours<-growing.periods$NoPeriods-growing.periods$Growth
  
  temp<-aggregate(Normalized_increment~Tree + Year , data=subset(input, GrowthPhase=="TWD"), FUN=length)
  growing.periods<-merge(growing.periods,temp, by = c("Tree", "Year"), all = TRUE)
  names(growing.periods)<-c("Tree", "Year", "NoPeriods", "Growth","NoGro","TWD")
  
  temp<-aggregate(Normalized_increment~Tree + Year , data=subset(input, GrowthPhase=="Refiling"), FUN=length)
  growing.periods<-merge(growing.periods,temp, by = c("Tree", "Year"), all = TRUE)
  names(growing.periods)<-c("Tree", "Year", "NoPeriods", "Growth","NoGro","TWD","Refiling")
  
  
  growing.periods$Rate_NoGRO_GRO<-growing.periods$NoGro/growing.periods$NoPeriods
  growing.periods$Rate_TWD_GRO<-growing.periods$TWD/growing.periods$NoPeriods
  growing.periods$Rate_Refiling_GRO<-growing.periods$Refiling/growing.periods$NoPeriods
  growing.periods$Rate_noChange<-(growing.periods$NoGro-growing.periods$TWD-growing.periods$Refiling)/growing.periods$NoPeriods
  
  
  growing.periods$Site<-site
  growing.periods$Species<-unique(input$Species)
  growing.periods$Size<-size
  
  growing.periods<-na.omit(growing.periods)
  
  return(growing.periods)
}

## test.differences_nogro_gro ####
# Test size-related differences in the proportion of non-growth periods relative to all periods.
test.differences_nogro_gro<-function(site, species, test="kw-test"){
  
  ## testing arguments
  # site="Eustaska"
  # species="PCAB"
  # test="kw-test"
  
  input=subset(dataset,Site==site & Species==species)
  
  if(test=="anova"){
    gr.hours<-summary(aov(Rate_NoGRO_GRO~Size,data=input))
    
    gr.hours<-round(gr[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    gr.hours<-kruskal.test(Rate_NoGRO_GRO~Size,data=input)
    
    gr.hours<-round(gr.hours$p.value,3)
  }
  if(test=="wilcox"){
    gr.hours<-wilcox.test(Rate_NoGRO_GRO~Size,data=input)
    
    gr.hours<-round(gr$p.value,3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     gr=gr.hours)
  return(output)
}

## test.differences_twd_gro ####
# Test size-related differences in the proportion of TWD periods relative to all periods.
test.differences_twd_gro<-function(site, species, test="kw-test"){
  
  ## testing arguments
  # site="Eustaska"
  # species="PCAB"
  # test="kw-test"
  
  input=subset(dataset,Site==site & Species==species)
  
  if(test=="anova"){
    gr.hours<-summary(aov(Rate_TWD_GRO~Size,data=input))
    
    gr.hours<-round(gr[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    gr.hours<-kruskal.test(Rate_TWD_GRO~Size,data=input)
    
    gr.hours<-round(gr.hours$p.value,3)
  }
  if(test=="wilcox"){
    gr.hours<-wilcox.test(Rate_TWD_GRO~Size,data=input)
    
    gr.hours<-round(gr$p.value,3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     gr=gr.hours)
  return(output)
}

## test.differences_refiling_gro ####
# Test size-related differences in the proportion of refilling periods relative to all periods.
test.differences_refiling_gro<-function(site, species, test="kw-test"){
  
  ## testing arguments
  # site="Eustaska"
  # species="PCAB"
  # test="kw-test"
  
  input=subset(dataset,Site==site & Species==species)
  
  if(test=="anova"){
    gr.hours<-summary(aov(Rate_Refiling_GRO~Size,data=input))
    
    gr.hours<-round(gr[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    gr.hours<-kruskal.test(Rate_Refiling_GRO~Size,data=input)
    
    gr.hours<-round(gr.hours$p.value,3)
  }
  if(test=="wilcox"){
    gr.hours<-wilcox.test(Rate_Refiling_GRO~Size,data=input)
    
    gr.hours<-round(gr$p.value,3)
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
differences_nogro_gro<-rbind(test.differences_nogro_gro("Ranspurk","ACCA"),
                             test.differences_nogro_gro("Ranspurk","CABE"),
                             test.differences_nogro_gro("Ranspurk","ULSP"),
                             test.differences_nogro_gro("Zofin","PCAB"),
                             test.differences_nogro_gro("Zofin","FASY"),
                             test.differences_nogro_gro("Boubin","PCAB"),
                             test.differences_nogro_gro("Boubin","FASY"),
                             test.differences_nogro_gro("Eustaska","PCAB"))

differences_twd_gro<-rbind(test.differences_twd_gro("Ranspurk","ACCA"),
                           test.differences_twd_gro("Ranspurk","CABE"),
                           test.differences_twd_gro("Ranspurk","ULSP"),
                           test.differences_twd_gro("Zofin","PCAB"),
                           test.differences_twd_gro("Zofin","FASY"),
                           test.differences_twd_gro("Boubin","PCAB"),
                           test.differences_twd_gro("Boubin","FASY"),
                           test.differences_twd_gro("Eustaska","PCAB"))

differences_refiling_gro<-rbind(test.differences_refiling_gro("Ranspurk","ACCA"),
                                test.differences_refiling_gro("Ranspurk","CABE"),
                                test.differences_refiling_gro("Ranspurk","ULSP"),
                                test.differences_refiling_gro("Zofin","PCAB"),
                                test.differences_refiling_gro("Zofin","FASY"),
                                test.differences_refiling_gro("Boubin","PCAB"),
                                test.differences_refiling_gro("Boubin","FASY"),
                                test.differences_refiling_gro("Eustaska","PCAB"))

## Printing results ####
print("------------- Differences -------------")
print("Nerustove vs rustove periody")
print(differences_nogro_gro)

print("TWD vs rustove periody")
print(differences_twd_gro)

print("Nerustove vs rustove periody")
print(differences_refiling_gro)

prc<-aggregate(Rate_NoGRO_GRO ~ Site + Species + Size, data=dataset, FUN=mean)
prc$Rate_NoGRO_GRO<-round(prc$Rate_NoGRO_GRO*100,1)
print("All non-growing")
print(prc)

prc<-aggregate(Rate_TWD_GRO ~ Site + Species + Size, data=dataset, FUN=mean)
prc$Rate_TWD_GRO<-round(prc$Rate_TWD_GRO*100,1)
print("TWD")
print(prc)

prc<-aggregate(Rate_Refiling_GRO ~ Site + Species + Size, data=dataset, FUN=mean)
prc$Rate_Refiling_GRO<-round(prc$Rate_Refiling_GRO*100,1)
print("Refiling")
print(prc)

prc<-aggregate(Rate_noChange ~ Site + Species + Size, data=dataset, FUN=mean)
prc$Rate_noChange<-round(prc$Rate_noChange*100,1)
print("All no-change")
print(prc)


print("---------------------------------------")

## Figure - panels ####
dataset<-add.x(dataset)

g<-ggplot(dataset)
g<-g+geom_boxplot(aes(x=x, y=Rate_NoGRO_GRO, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Non growing periods (%)",limits=c(0,1),breaks=seq(0,1,0.2), labels=seq(0,100,20))
g<-my.theme(g, "none")
figure_a<-g


g<-ggplot(dataset)
g<-g+geom_boxplot(aes(x=x, y=Rate_TWD_GRO, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("TWD (%)",limits=c(0,0.5),breaks=seq(0,1,0.1), labels=seq(0,100,10))
g<-my.theme(g, "none")
figure_b<-g


g<-ggplot(dataset)
g<-g+geom_boxplot(aes(x=x, y=Rate_Refiling_GRO, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Stem tissue refiling (%)",limits=c(0,0.5),breaks=seq(0,1,0.1), labels=seq(0,100,10))
g<-my.theme(g, "none")
figure_c<-g

g<-ggplot(dataset)
g<-g+geom_boxplot(aes(x=x, y=Rate_noChange, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("No change (%)",limits=c(0,0.5),breaks=seq(0,1,0.1), labels=seq(0,100,10))
g<-my.theme(g, "none")
figure_d<-g

## Figure - compilation ####
figure<-ggarrange(figure_a, figure_b, figure_c, figure_d,ncol=1,nrow=4,align="hv",labels=LETTERS[1:4])


