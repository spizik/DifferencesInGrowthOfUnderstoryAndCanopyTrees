## Functions ####

## my.theme ####
# Apply a simple classic ggplot2 theme with black axes and configurable legend position.
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
# Assign fixed numeric x positions based on site, species, and tree size for plotting.
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

## calculate.data ####
# Extract growth- or TWD-phase records and calculate absolute and relative DBH increments.
calculate.data<-function(input, phase, site, size){
  
  ## testing arguments
  # input=dendrometer_data$EU_PCAB_bigTrees
  # phase="Growth"
  
  sub_input<-subset(input,GrowthPhase==phase)
  
  output<-sub_input[,c("Tree", "Species", "Year", "DOY", "Month", "Day", "Hour", "Minute", "DBH_value", "DBH_increment")]
  output$DBH_increment_relative<-(output$DBH_increment/output$DBH_value)*100
  
  output$Site<-site
  output$Size<-size
  
  return(output)
}

## test.differences.growth ####
# Test size-related differences in absolute and relative DBH increments during growth phase.
test.differences.growth<-function(site, species, test="lmer"){
  
  ## testing arguments
  # site="Ranspurk"
  # species="ACCA"
  # test="lmer"

  input<-subset(dataset_growth,Site==site & Species==species)
  
  if(test=="anova"){
    absolute<-summary(aov(DBH_increment~Size,data=input))
    relative<-summary(aov(DBH_increment_relative~Size,data=input))
    
    absolute<-round(absolute[[1]][["Pr(>F)"]][1],3)
    relative<-round(relative[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    absolute<-kruskal.test(DBH_increment~Size,data=input)
    relative<-kruskal.test(DBH_increment_relative~Size,data=input)
    
    absolute<-round(absolute$p.value,3)
    relative<-round(relative$p.value,3)
  }
  if(test=="wilcox"){
    absolute<-wilcox.test(DBH_increment~Size,data=input)
    relative<-wilcox.test(DBH_increment_relative~Size,data=input)
    
    absolute<-round(absolute$p.value,3)
    relative<-round(relative$p.value,3)
  }
  if(test=="lmer"){
    absolute<-summary(lmer(DBH_increment   ~ Size + (1|Tree) + (1|Year), data = input))
    relative<-summary(lmer(DBH_increment_relative   ~ Size + (1|Tree) + (1|Year), data = input))
    
    absolute<-round(absolute$coefficients[,"Pr(>|t|)"][2],3)
    relative<-round(relative$coefficients[,"Pr(>|t|)"][2],3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     absolute_increments=absolute,
                     relative_increments=relative)
  return(output)
}

## test.differences.twd ####
# Test size-related differences in absolute and relative DBH increments during TWD phase.
test.differences.twd<-function(site, species, test="lmer"){
  
  ## testing arguments
  # site="Ranspurk"
  # species="ACCA"
  # test="kw-test"
  
  input<-subset(dataset_twd,Site==site & Species==species)
  
  if(test=="anova"){
    absolute<-summary(aov(DBH_increment~Size,data=input))
    relative<-summary(aov(DBH_increment_relative~Size,data=input))
    
    absolute<-round(absolute[[1]][["Pr(>F)"]][1],3)
    relative<-round(relative[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    absolute<-kruskal.test(DBH_increment~Size,data=input)
    relative<-kruskal.test(DBH_increment_relative~Size,data=input)
    
    absolute<-round(absolute$p.value,3)
    relative<-round(relative$p.value,3)
  }
  if(test=="wilcox"){
    absolute<-wilcox.test(DBH_increment~Size,data=input)
    relative<-wilcox.test(DBH_increment_relative~Size,data=input)
    
    absolute<-round(absolute$p.value,3)
    relative<-round(relative$p.value,3)
  }
  if(test=="lmer"){
    absolute<-summary(lmer(DBH_increment   ~ Size + (1|Tree) + (1|Year), data = input))
    relative<-summary(lmer(DBH_increment_relative   ~ Size + (1|Tree) + (1|Year), data = input))
    
    absolute<-round(absolute$coefficients[,"Pr(>|t|)"][2],3)
    relative<-round(relative$coefficients[,"Pr(>|t|)"][2],3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     absolute_increments=absolute,
                     relative_increments=relative)
  return(output)
}

## Dataset calculations ####
dataset_growth<-rbind(calculate.data(dendrometer_data$RN_ACCA_bigTrees, "Growth", "Ranspurk", "big"),
                      calculate.data(dendrometer_data$RN_ACCA_smallTrees, "Growth", "Ranspurk", "small"),
                      calculate.data(dendrometer_data$RN_CABE_bigTrees, "Growth", "Ranspurk", "big"),
                      calculate.data(dendrometer_data$RN_CABE_smallTrees, "Growth", "Ranspurk", "small"),
                      calculate.data(dendrometer_data$RN_ULSP_bigTrees, "Growth", "Ranspurk", "big"),
                      calculate.data(dendrometer_data$RN_ULSP_smallTrees, "Growth", "Ranspurk", "small"),
                      
                      calculate.data(dendrometer_data$ZF_PCAB_bigTrees, "Growth", "Zofin", "big"),
                      calculate.data(dendrometer_data$ZF_PCAB_smallTrees, "Growth", "Zofin", "small"),
                      calculate.data(dendrometer_data$ZF_FASY_bigTrees, "Growth", "Zofin", "big"),
                      calculate.data(dendrometer_data$ZF_FASY_smallTrees, "Growth", "Zofin", "small"),
                      
                      calculate.data(dendrometer_data$BB_PCAB_bigTrees, "Growth", "Boubin", "big"),
                      calculate.data(dendrometer_data$BB_PCAB_smallTrees, "Growth", "Boubin", "small"),
                      calculate.data(dendrometer_data$BB_FASY_bigTrees, "Growth", "Boubin", "big"),
                      calculate.data(dendrometer_data$BB_FASY_smallTrees, "Growth", "Boubin", "small"),
                      
                      calculate.data(dendrometer_data$EU_PCAB_bigTrees, "Growth", "Eustaska", "big"),
                      calculate.data(dendrometer_data$EU_PCAB_smallTrees, "Growth", "Eustaska", "small"))

dataset_twd<-rbind(calculate.data(dendrometer_data$RN_ACCA_bigTrees, "TWD", "Ranspurk", "big"),
                   calculate.data(dendrometer_data$RN_ACCA_smallTrees, "TWD", "Ranspurk", "small"),
                   calculate.data(dendrometer_data$RN_CABE_bigTrees, "TWD", "Ranspurk", "big"),
                   calculate.data(dendrometer_data$RN_CABE_smallTrees, "TWD", "Ranspurk", "small"),
                   calculate.data(dendrometer_data$RN_ULSP_bigTrees, "TWD", "Ranspurk", "big"),
                   calculate.data(dendrometer_data$RN_ULSP_smallTrees, "TWD", "Ranspurk", "small"),
                   
                   calculate.data(dendrometer_data$ZF_PCAB_bigTrees, "TWD", "Zofin", "big"),
                   calculate.data(dendrometer_data$ZF_PCAB_smallTrees, "TWD", "Zofin", "small"),
                   calculate.data(dendrometer_data$ZF_FASY_bigTrees, "TWD", "Zofin", "big"),
                   calculate.data(dendrometer_data$ZF_FASY_smallTrees, "TWD", "Zofin", "small"),
                   
                   calculate.data(dendrometer_data$BB_PCAB_bigTrees, "TWD", "Boubin", "big"),
                   calculate.data(dendrometer_data$BB_PCAB_smallTrees, "TWD", "Boubin", "small"),
                   calculate.data(dendrometer_data$BB_FASY_bigTrees, "TWD", "Boubin", "big"),
                   calculate.data(dendrometer_data$BB_FASY_smallTrees, "TWD", "Boubin", "small"),
                   
                   calculate.data(dendrometer_data$EU_PCAB_bigTrees, "TWD", "Eustaska", "big"),
                   calculate.data(dendrometer_data$EU_PCAB_smallTrees, "TWD", "Eustaska", "small"))

## Testing differences ####
differences<-rbind(test.differences.growth("Ranspurk","ACCA"),
                   test.differences.growth("Ranspurk","CABE"),
                   test.differences.growth("Ranspurk","ULSP"),
                   test.differences.growth("Zofin","PCAB"),
                   test.differences.growth("Zofin","FASY"),
                   test.differences.growth("Boubin","PCAB"),
                   test.differences.growth("Boubin","FASY"),
                   test.differences.growth("Eustaska","PCAB"))

## Printing results ####
print("--------- Differences Growth ----------")
print(differences)
print("---------------------------------------")

differences<-rbind(test.differences.twd("Ranspurk","ACCA"),
                   test.differences.twd("Ranspurk","CABE"),
                   test.differences.twd("Ranspurk","ULSP"),
                   test.differences.twd("Zofin","PCAB"),
                   test.differences.twd("Zofin","FASY"),
                   test.differences.twd("Boubin","PCAB"),
                   test.differences.twd("Boubin","FASY"),
                   test.differences.twd("Eustaska","PCAB"))


print("--------- Differences Growth ----------")
print(differences)
print("---------------------------------------")

## Figure - individual parts ####
dataset_growth<-add.x(dataset_growth)
dataset_twd<-add.x(dataset_twd)

# Figure absolute increment
g<-ggplot(dataset_growth)
g<-g+geom_boxplot(aes(x=x, y=DBH_increment, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Stem increment (mm)",limits=c(0,0.025),breaks=seq(0,1,0.005), labels=formatC(seq(0,1,0.005),format="f",digits=3))
g<-my.theme(g)
figure_a<-g

# Figure absolute trw
g<-ggplot(dataset_twd)
g<-g+geom_boxplot(aes(x=x, y=DBH_increment, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Stem TWD (mm)",limits=c(-0.025,0),breaks=seq(-1,0,0.005), labels=formatC(seq(-1,0,0.005),format="f",digits=3))
g<-my.theme(g)
figure_b<-g

# Figure relative increment
g<-ggplot(dataset_growth)
g<-g+geom_boxplot(aes(x=x, y=DBH_increment_relative, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Stem increment relative to DBH (%)",limits=c(0,0.1),breaks=seq(0,1,0.025), labels=formatC(seq(0,1,0.025),format="f",digits=3))
g<-my.theme(g)
figure_c<-g

# Figure relative TWD
g<-ggplot(dataset_twd)
g<-g+geom_boxplot(aes(x=x, y=DBH_increment_relative, fill=Species, alpha=Size, group=grp), colour="black", outliers = F)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Stem TWD relative to DBH (%)",limits=c(-0.1,0),breaks=seq(-1,0,0.025), labels=formatC(seq(-1,0,0.025),format="f",digits=3))
g<-my.theme(g)
figure_d<-g

## Figure - compilation ####
figure<-ggarrange(figure_a,figure_b,figure_c,figure_d,nrow=2,ncol=2,align="hv",common.legend=T,legend="bottom",labels=LETTERS[1:6])