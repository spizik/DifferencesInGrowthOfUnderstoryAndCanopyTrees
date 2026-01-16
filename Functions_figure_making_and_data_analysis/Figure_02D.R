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

## calculate.data ####
# Calculate relative annual DBH increment (normalized by minimum DBH) for each tree and year.
calculate.data<-function(input,site,size){
  
  ## testing arguments
  # input=dendrometer_data$BB_FASY_bigTrees
  # site="BB"
  # species="FASY"
  # size="big"
  
  dbh.min<-aggregate(DBH_value~Tree+Year,data=input, FUN=min)
  dbh.max<-aggregate(DBH_value~Tree+Year,data=input, FUN=max)
  
  output<-dbh.max
  output$DBH_value<-(dbh.max$DBH_value-dbh.min$DBH_value)/dbh.min$DBH_value
  output$Site<-site
  output$Species<-unique(input$Species)
  output$Size<-size

  return(output)
}

## calculate.data.2 ####
# Calculate absolute annual DBH increment for each tree and year.
calculate.data.2<-function(input,site,size){
  
  ## testing arguments
  # input=dendrometer_data$BB_FASY_bigTrees
  # site="BB"
  # species="FASY"
  # size="big"
  
  dbh.min<-aggregate(DBH_value~Tree+Year,data=input, FUN=min)
  dbh.max<-aggregate(DBH_value~Tree+Year,data=input, FUN=max)
  
  output<-dbh.max
  output$DBH_value<-dbh.max$DBH_value-dbh.min$DBH_value
  output$Site<-site
  output$Species<-unique(input$Species)
  output$Size<-size
  
  return(output)
}

## test.differences ####
# Test size-related differences in relative DBH increment between big and small trees for a given site and species.
test.differences<-function(site, species, test="kw-test"){
  
  ## testing arguments
  # site="Ranspurk"
  # species="ACCA"
  # test="kw-test"
  
  input=subset(dataset,Site==site & Species==species)
  
  if(test=="anova"){
    gr<-summary(aov(DBH_value~Size,data=input))
    
    gr<-round(gr[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    gr<-kruskal.test(DBH_value~Size,data=input)
    
    gr<-round(gr$p.value,3)
  }
  if(test=="wilcox"){
    gr<-wilcox.test(DBH_value~Size,data=input)
    
    gr<-round(gr$p.value,3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     gr=gr)
  return(output)
}

## test.differences.2 ####
# Test size-related differences in absolute DBH increment between big and small trees for a given site and species.
test.differences.2<-function(site, species, test="kw-test"){
  
  ## testing arguments
  # site="Ranspurk"
  # species="ACCA"
  # test="kw-test"
  
  input=subset(dataset.2,Site==site & Species==species)
  
  if(test=="anova"){
    gr<-summary(aov(DBH_value~Size,data=input))
    
    gr<-round(gr[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    gr<-kruskal.test(DBH_value~Size,data=input)
    
    gr<-round(gr$p.value,3)
  }
  if(test=="wilcox"){
    gr<-wilcox.test(DBH_value~Size,data=input)
    
    gr<-round(gr$p.value,3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     gr=gr)
  return(output)
}


## Calculations ####
dataset<-rbind(calculate.data(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
               calculate.data(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small"),
               calculate.data(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
               calculate.data(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small"),
               calculate.data(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
               calculate.data(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small"),
               
               calculate.data(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
               calculate.data(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small"),
               calculate.data(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
               calculate.data(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small"),
               
               calculate.data(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
               calculate.data(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small"),
               calculate.data(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
               calculate.data(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small"),
               
               calculate.data(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
               calculate.data(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"))


dataset.2<-rbind(calculate.data.2(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
                 calculate.data.2(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small"),
                 calculate.data.2(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
                 calculate.data.2(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small"),
                 calculate.data.2(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
                 calculate.data.2(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small"),
                 
                 calculate.data.2(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
                 calculate.data.2(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small"),
                 calculate.data.2(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
                 calculate.data.2(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small"),
                 
                 calculate.data.2(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
                 calculate.data.2(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small"),
                 calculate.data.2(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
                 calculate.data.2(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small"),
                 
                 calculate.data.2(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
                 calculate.data.2(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"))

## Testing differences ####
differences<-rbind(test.differences("Ranspurk","ACCA"),
                   test.differences("Ranspurk","CABE"),
                   test.differences("Ranspurk","ULSP"),
                   test.differences("Zofin","PCAB"),
                   test.differences("Zofin","FASY"),
                   test.differences("Boubin","PCAB"),
                   test.differences("Boubin","FASY"),
                   test.differences("Eustaska","PCAB"))

differences.2<-rbind(test.differences.2("Ranspurk","ACCA"),
                   test.differences.2("Ranspurk","CABE"),
                   test.differences.2("Ranspurk","ULSP"),
                   test.differences.2("Zofin","PCAB"),
                   test.differences.2("Zofin","FASY"),
                   test.differences.2("Boubin","PCAB"),
                   test.differences.2("Boubin","FASY"),
                   test.differences.2("Eustaska","PCAB"))


print("------------- Differences -------------")
print(differences)

print("Absolute increment")
print(c(round(mean(aggregate(DBH_value ~ Site + Species, data = subset(dataset.2, Size == "big"), FUN = median)$DBH_value-
                   aggregate(DBH_value ~ Site + Species, data = subset(dataset.2, Size == "small"), FUN = median)$DBH_value),
              2),
        
        round(sd(aggregate(DBH_value ~ Site + Species, data = subset(dataset.2, Size == "big"), FUN = median)$DBH_value-
             aggregate(DBH_value ~ Site + Species, data = subset(dataset.2, Size == "small"), FUN = median)$DBH_value),
             2)))

print("Absolute increment")
print(c(round(mean(aggregate(DBH_value ~ Site + Species, data = subset(dataset, Size == "small"), FUN = median)$DBH_value-
               aggregate(DBH_value ~ Site + Species, data = subset(dataset, Size == "big"), FUN = median)$DBH_value),
              3),
        
        round(sd(aggregate(DBH_value ~ Site + Species, data = subset(dataset, Size == "small"), FUN = median)$DBH_value-
             aggregate(DBH_value ~ Site + Species, data = subset(dataset, Size == "big"), FUN = median)$DBH_value),
             3)))

print("---------------------------------------")

## Figure ####
dataset<-add.x(dataset)


g<-ggplot(dataset)
g<-g+geom_boxplot(aes(x=x, y=DBH_value, fill=Species, alpha=Size, group=grp, colour=Size), outliers = F)
g<-g+scale_colour_manual(breaks=boxcols, values=boxcols)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Annual SDI (% of DBH)",limits=c(0,0.05),breaks=seq(0,1,0.01),labels=seq(0,100,1))
g<-my.theme(g)
figure<-g
