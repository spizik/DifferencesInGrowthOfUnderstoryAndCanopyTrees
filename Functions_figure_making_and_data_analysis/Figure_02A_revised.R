## Functions ####

## my.theme ####
# Apply a classic ggplot2 theme with black axes and customizable legend position.
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
# Add a numeric x-position variable based on site, species, and size combinations for plotting.
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

## calculate.GS.length.data ####
# Calculate the growing season beginning, end, and length for each tree and year from dendrometer data.
calculate.GS.length.data<-function(input, site, size){
  
  ## testing arguments
  # input=dendrometer_data$RN_ACCA_bigTrees
  # site="EU"
  # size="big"

  input$Time<-input$DOY + input$Hour/24
  
  output<-aggregate(Time~Tree+Year, data=input,FUN=min)
  names(output)<-c("Tree","Year","beginning")
  output$ending<-aggregate(Time~Tree+Year, data=input,FUN=max)$Time
  output$length<-output$ending-output$beginning
  
  output$Site<-site
  output$Species<-unique(input$Species)
  output$Size<-size
  
  return(output)
}

## test.differences ####
# Test size-related differences in growing season metrics (start, end, duration) within a site and species.
test.differences<-function(site, species, test="lmer"){
  
  ## testing arguments
  # site="Ranspurk"
  # species="CABE"
  # test="lmer"
  
  input<-subset(dataset,Site==site & Species==species)
  
  if(test=="anova"){
    sos<-summary(aov(beginning~Size,data=input))
    eos<-summary(aov(ending~Size,data=input))
    dos<-summary(aov(length~Size,data=input))
    
    sos<-round(sos[[1]][["Pr(>F)"]][1],3)
    eos<-round(eos[[1]][["Pr(>F)"]][1],3)
    dos<-round(dos[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    sos<-kruskal.test(beginning~Size,data=input)
    eos<-kruskal.test(ending~Size,data=input)
    dos<-kruskal.test(length~Size,data=input)
    
    sos<-round(sos$p.value,3)
    eos<-round(eos$p.value,3)
    dos<-round(dos$p.value,3)
  }
  if(test=="wilcox"){
    sos<-wilcox.test(beginning~Size,data=input)
    eos<-wilcox.test(ending~Size,data=input)
    dos<-wilcox.test(length~Size,data=input)
    
    sos<-round(sos$p.value,3)
    eos<-round(eos$p.value,3)
    dos<-round(dos$p.value,3)
  }
  if(test == "lmer"){
    sos <- summary(lmer(beginning   ~ Size + (1|Tree) + (1|Year), data = input))
    dos <- summary(lmer(length   ~ Size + (1|Tree) + (1|Year), data = input))
    eos <- summary(lmer(ending   ~ Size + (1|Tree) + (1|Year), data = input))
    
    sos<-round(sos$coefficients[,"Pr(>|t|)"][2],3)
    eos<-round(eos$coefficients[,"Pr(>|t|)"][2],3)
    dos<-round(dos$coefficients[,"Pr(>|t|)"][2],3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     sos=sos,
                     eos=eos,
                     dos=dos)
  return(output)
}

## testing.sos.diff.small.big.indiv ####
# Perform a paired test comparing start of season between canopy and juvenile trees within matched groups.
testing.sos.diff.small.big.indiv<-function(input){
  ## testing arguments
  # input=subset(dataset_sos, Site=="Boubin" & Size=="big")
  
  wide_data <- spread(input, Size, beginning)
  colnames(wide_data) <- c("Site", "Year", "Species", "Canopy", "Juvenile")
  wide_data<-na.omit(wide_data)
  
  wide_data$diff <- wide_data$Canopy - wide_data$Juvenile
  
  # test
  output<-t.test(wide_data$Canopy, wide_data$Juvenile, paired = TRUE)
  return(output)
}

## testing.sos.diff.small.big ####
# Summarize paired-test p-values for start-of-season differences between small and big trees across sites and species.
testing.sos.diff.small.big<-function(input){
  
  ## testing arguments
  # input = dataset_sos
  
  output<-data.frame(Site=c("Eustaska","Boubin","Boubin","Zofin","Zofin","Ranspurk","Ranspurk"),
                     Species=c("PCAB","PCAB","FASY","PCAB","FASY","ACCA","CABE"),
                     p.vals=c(round(testing.sos.diff.small.big.indiv(subset(input, Site=="Eustaska" & Species=="PCAB"))$p.value,3),
                              round(testing.sos.diff.small.big.indiv(subset(input, Site=="Boubin" & Species=="PCAB"))$p.value,3),
                              round(testing.sos.diff.small.big.indiv(subset(input, Site=="Boubin" & Species=="FASY"))$p.value,3),
                              round(testing.sos.diff.small.big.indiv(subset(input, Site=="Zofin" & Species=="PCAB"))$p.value,3),
                              round(testing.sos.diff.small.big.indiv(subset(input, Site=="Zofin" & Species=="FASY"))$p.value,3),
                              round(testing.sos.diff.small.big.indiv(subset(input, Site=="Ranspurk" & Species=="ACCA"))$p.value,3),
                              round(testing.sos.diff.small.big.indiv(subset(input, Site=="Ranspurk" & Species=="CABE"))$p.value,3)))
  
  
  return(output)
}

## testing.eos.diff.small.big.indiv ####
# Perform a paired test comparing end of season between canopy and juvenile trees within matched groups.
testing.eos.diff.small.big.indiv<-function(input){
  
  ## testing arguments
  # input=subset(dataset_sos, Site=="Boubin" & Size=="big")
  
  wide_data <- spread(input, Size, ending)
  colnames(wide_data) <- c("Site", "Year", "Species", "Canopy", "Juvenile")
  wide_data<-na.omit(wide_data)
  
  wide_data$diff <- wide_data$Canopy - wide_data$Juvenile
  
  output<-t.test(wide_data$Canopy, wide_data$Juvenile, paired = TRUE)
  return(output)
}

## testing.eos.diff.small.big ####
# Summarize paired-test p-values for end-of-season differences between small and big trees across sites and species.
testing.eos.diff.small.big<-function(input){
  
  ## testing arguments
  # input = dataset_sos
  
  output<-data.frame(Site=c("Eustaska","Boubin","Boubin","Zofin","Zofin","Ranspurk","Ranspurk"),
                     Species=c("PCAB","PCAB","FASY","PCAB","FASY","ACCA","CABE"),
                     p.vals=c(round(testing.eos.diff.small.big.indiv(subset(input, Site=="Eustaska" & Species=="PCAB"))$p.value,3),
                              round(testing.eos.diff.small.big.indiv(subset(input, Site=="Boubin" & Species=="PCAB"))$p.value,3),
                              round(testing.eos.diff.small.big.indiv(subset(input, Site=="Boubin" & Species=="FASY"))$p.value,3),
                              round(testing.eos.diff.small.big.indiv(subset(input, Site=="Zofin" & Species=="PCAB"))$p.value,3),
                              round(testing.eos.diff.small.big.indiv(subset(input, Site=="Zofin" & Species=="FASY"))$p.value,3),
                              round(testing.eos.diff.small.big.indiv(subset(input, Site=="Ranspurk" & Species=="ACCA"))$p.value,3),
                              round(testing.eos.diff.small.big.indiv(subset(input, Site=="Ranspurk" & Species=="CABE"))$p.value,3)))
  
  
  return(output)
}

## testing.dos.diff.small.big.indiv ####
# Perform a paired test comparing growing season duration between canopy and juvenile trees within matched groups.
testing.dos.diff.small.big.indiv<-function(input){
  
  ## testing arguments
  # input=subset(dataset_sos, Site=="Boubin" & Size=="big")
  
  wide_data <- spread(input, Size, duration)
  colnames(wide_data) <- c("Site", "Year", "Species", "Canopy", "Juvenile")
  wide_data<-na.omit(wide_data)
  
  wide_data$diff <- wide_data$Canopy - wide_data$Juvenile
  
  output<-t.test(wide_data$Canopy, wide_data$Juvenile, paired = TRUE)
  return(output)
}

## testing.dos.diff.small.big ####
# Summarize paired-test p-values for growing season duration differences between small and big trees across sites and species.
testing.dos.diff.small.big<-function(input){
  
  ## testing arguments
  # input = dataset_sos
  
  output<-data.frame(Site=c("Eustaska","Boubin","Boubin","Zofin","Zofin","Ranspurk","Ranspurk"),
                     Species=c("PCAB","PCAB","FASY","PCAB","FASY","ACCA","CABE"),
                     p.vals=c(round(testing.dos.diff.small.big.indiv(subset(input, Site=="Eustaska" & Species=="PCAB"))$p.value,3),
                              round(testing.dos.diff.small.big.indiv(subset(input, Site=="Boubin" & Species=="PCAB"))$p.value,3),
                              round(testing.dos.diff.small.big.indiv(subset(input, Site=="Boubin" & Species=="FASY"))$p.value,3),
                              round(testing.dos.diff.small.big.indiv(subset(input, Site=="Zofin" & Species=="PCAB"))$p.value,3),
                              round(testing.dos.diff.small.big.indiv(subset(input, Site=="Zofin" & Species=="FASY"))$p.value,3),
                              round(testing.dos.diff.small.big.indiv(subset(input, Site=="Ranspurk" & Species=="ACCA"))$p.value,3),
                              round(testing.dos.diff.small.big.indiv(subset(input, Site=="Ranspurk" & Species=="CABE"))$p.value,3)))
  
  
  return(output)
}

## testing.sos.same.size.sppdiff.indiv ####
# Perform a paired test comparing start of season between species within the same site and size class.
testing.sos.same.size.sppdiff.indiv<-function(input){
  
  ## testing arguments
  # input=subset(dataset_sos, Site=="Boubin" & Size=="big")
  
  wide_data <- spread(input, Species, beginning)
  wide_data<-na.omit(wide_data)
  
  wide_data$diff <- wide_data[,4] - wide_data[,5]
  
  # test
  output<-t.test(wide_data[,4], wide_data[,5], paired = TRUE)
  return(output)
}

## testing.sos.same.size.sppdiff ####
# Summarize paired-test p-values for species differences in start of season within the same size class across sites.
testing.sos.same.size.sppdiff<-function(input){
  
  ## testing arguments
  # input = dataset_sos
  
  output<-data.frame(Site=c("Boubin","Boubin","Zofin","Zofin","Ranspurk","Ranspurk"),
                     Species=c("big","small","big","small","big","small"),
                     p.vals=c(round(testing.sos.same.size.sppdiff.indiv(subset(input, Site=="Boubin" & Size=="big"))$p.value,3),
                              round(testing.sos.same.size.sppdiff.indiv(subset(input, Site=="Boubin" & Size=="small"))$p.value,3),
                              round(testing.sos.same.size.sppdiff.indiv(subset(input, Site=="Zofin" & Size=="big"))$p.value,3),
                              round(testing.sos.same.size.sppdiff.indiv(subset(input, Site=="Zofin" & Size=="small"))$p.value,3),
                              round(testing.sos.same.size.sppdiff.indiv(subset(input, Site=="Ranspurk" & Size=="big"))$p.value,3),
                              round(testing.sos.same.size.sppdiff.indiv(subset(input, Site=="Ranspurk" & Size=="small"))$p.value,3)))
  
  
  return(output)
}

## testing.eos.same.size.sppdiff.indiv ####
# Perform a paired test comparing end of season between species within the same site and size class.
testing.eos.same.size.sppdiff.indiv<-function(input){
  
  ## testing arguments
  # input=subset(dataset_sos, Site=="Boubin" & Size=="big")
  
  wide_data <- spread(input, Species, ending)
  wide_data<-na.omit(wide_data)
  
  wide_data$diff <- wide_data[,4] - wide_data[,5]
  
  output<-t.test(wide_data[,4], wide_data[,5], paired = TRUE)
  return(output)
}

## testing.eos.same.size.sppdiff ####
# Summarize paired-test p-values for species differences in end of season within the same size class across sites.
testing.eos.same.size.sppdiff<-function(input){
  
  ## testing arguments
  # input = dataset_sos
  
  output<-data.frame(Site=c("Boubin","Boubin","Zofin","Zofin","Ranspurk","Ranspurk"),
                     Species=c("big","small","big","small","big","small"),
                     p.vals=c(round(testing.eos.same.size.sppdiff.indiv(subset(input, Site=="Boubin" & Size=="big"))$p.value,3),
                              round(testing.eos.same.size.sppdiff.indiv(subset(input, Site=="Boubin" & Size=="small"))$p.value,3),
                              round(testing.eos.same.size.sppdiff.indiv(subset(input, Site=="Zofin" & Size=="big"))$p.value,3),
                              round(testing.eos.same.size.sppdiff.indiv(subset(input, Site=="Zofin" & Size=="small"))$p.value,3),
                              round(testing.eos.same.size.sppdiff.indiv(subset(input, Site=="Ranspurk" & Size=="big"))$p.value,3),
                              round(testing.eos.same.size.sppdiff.indiv(subset(input, Site=="Ranspurk" & Size=="small"))$p.value,3)))
  
  
  return(output)
}

## testing.dos.same.size.sppdiff.indiv ####
# Perform a paired test comparing growing season duration between species within the same site and size class.
testing.dos.same.size.sppdiff.indiv<-function(input){
  
  ## testing arguments
  # input=subset(dataset_sos, Site=="Boubin" & Size=="big")
  
  wide_data <- spread(input, Species, duration)
  wide_data<-na.omit(wide_data)
  
  wide_data$diff <- wide_data[,4] - wide_data[,5]
  
  # test
  output<-t.test(wide_data[,4], wide_data[,5], paired = TRUE)
  return(output)
}

## testing.dos.same.size.sppdiff ####
# Summarize paired-test p-values for species differences in growing season duration within the same size class across sites.
testing.dos.same.size.sppdiff<-function(input){
  
  ## testing arguments
  # input = dataset_sos
  
  output<-data.frame(Site=c("Boubin","Boubin","Zofin","Zofin","Ranspurk","Ranspurk"),
                     Species=c("big","small","big","small","big","small"),
                     p.vals=c(round(testing.dos.same.size.sppdiff.indiv(subset(input, Site=="Boubin" & Size=="big"))$p.value,3),
                              round(testing.dos.same.size.sppdiff.indiv(subset(input, Site=="Boubin" & Size=="small"))$p.value,3),
                              round(testing.dos.same.size.sppdiff.indiv(subset(input, Site=="Zofin" & Size=="big"))$p.value,3),
                              round(testing.dos.same.size.sppdiff.indiv(subset(input, Site=="Zofin" & Size=="small"))$p.value,3),
                              round(testing.dos.same.size.sppdiff.indiv(subset(input, Site=="Ranspurk" & Size=="big"))$p.value,3),
                              round(testing.dos.same.size.sppdiff.indiv(subset(input, Site=="Ranspurk" & Size=="small"))$p.value,3)))
  
  
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

## Testing differences ####
differences<-rbind(test.differences("Ranspurk","ACCA"),
                   test.differences("Ranspurk","CABE"),
                   test.differences("Ranspurk","ULSP"),
                   test.differences("Zofin","PCAB"),
                   test.differences("Zofin","FASY"),
                   test.differences("Boubin","PCAB"),
                   test.differences("Boubin","FASY"),
                   test.differences("Eustaska","PCAB"))

dataset_sos <- aggregate(beginning~ Site + Year + Species + Size, data = dataset, FUN=median)
dataset_eos <- aggregate(ending~ Site + Year + Species + Size, data = dataset, FUN=median)
dataset_dos <- dataset_sos[,c("Site", "Year", "Species", "Size")]
dataset_dos$duration <- dataset_eos$ending - dataset_sos$beginning




mean(aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "big"), FUN = median)$beginning-
       aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "small"), FUN = median)$beginning)

sd(aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "big"), FUN = median)$beginning-
     aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "small"), FUN = median)$beginning)

print("------------- Differences -------------")
print(differences)
print("General - SOS")
print(c(round(mean(aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "big"), FUN = median)$beginning-
                   aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "small"), FUN = median)$beginning),
              1),
      
      round(sd(aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "big"), FUN = median)$beginning-
               aggregate(beginning ~ Site + Species, data = subset(dataset_sos, Size == "small"), FUN = median)$beginning),
            1)))


print("PCAB ZF")
print(c(mean(median(subset(dataset_sos, Size == "big" & Site == "Zofin" & Species == "PCAB")$beginning)-
               median(subset(dataset_sos, Size == "small" & Site == "Zofin" & Species == "PCAB")$beginning)),
        
        sd(subset(dataset_sos, Size == "big" & Site == "Zofin" & Species == "PCAB")$beginning-
             subset(dataset_sos, Size == "small" & Site == "Zofin" & Species == "PCAB")$beginning)))

print("PCAB BB")
print(c(mean(median(subset(dataset_sos, Size == "big" & Site == "Boubin" & Species == "PCAB")$beginning)-
             median(subset(dataset_sos, Size == "small" & Site == "Boubin" & Species == "PCAB")$beginning)),
      
      sd(subset(dataset_sos, Size == "big" & Site == "Boubin" & Species == "PCAB")$beginning-
             subset(dataset_sos, Size == "small" & Site == "Boubin" & Species == "PCAB")$beginning)))

print("PCAB EU")
print(c(mean(median(subset(dataset_sos, Size == "big" & Site == "Eustaska" & Species == "PCAB")$beginning)-
               median(subset(dataset_sos, Size == "small" & Site == "Eustaska" & Species == "PCAB")$beginning)),
        
        sd(subset(dataset_sos, Size == "big" & Site == "Eustaska" & Species == "PCAB")$beginning-
             subset(dataset_sos, Size == "small" & Site == "Eustaska" & Species == "PCAB")$beginning)))


print("FASY ZF")
print(c(mean(median(subset(dataset_sos, Size == "big" & Site == "Zofin" & Species == "FASY")$beginning)-
               median(subset(dataset_sos, Size == "small" & Site == "Zofin" & Species == "FASY")$beginning)),
        
        sd(subset(dataset_sos, Size == "big" & Site == "Zofin" & Species == "FASY")$beginning-
             subset(dataset_sos, Size == "small" & Site == "Zofin" & Species == "FASY")$beginning)))

print("FASY BB")
print(c(mean(median(subset(dataset_sos, Size == "big" & Site == "Boubin" & Species == "FASY")$beginning)-
               median(subset(dataset_sos, Size == "small" & Site == "Boubin" & Species == "FASY")$beginning)),
        
        sd(subset(dataset_sos, Size == "big" & Site == "Boubin" & Species == "FASY")$beginning-
             subset(dataset_sos, Size == "small" & Site == "Boubin" & Species == "FASY")$beginning)))

print("Ranspurk BB")
print(c(mean(median(subset(dataset_sos, Size == "big" & Site == "Ranspurk")$beginning)-
               median(subset(dataset_sos, Size == "small" & Site == "Ranspurk")$beginning)),
        
        sd(subset(dataset_sos, Size == "big" & Site == "Ranspurk")$beginning-
             subset(dataset_sos, Size == "small" & Site == "Ranspurk")$beginning)))

print("General - EOS")
print(c(round(mean(aggregate(ending ~ Site + Species, data = subset(dataset_eos, Size == "big"), FUN = median)$ending-
               aggregate(ending ~ Site + Species, data = subset(dataset_eos, Size == "small"), FUN = median)$ending),
              1),
        
        round(sd(aggregate(ending ~ Site + Species, data = subset(dataset_eos, Size == "big"), FUN = median)$ending-
             aggregate(ending ~ Site + Species, data = subset(dataset_eos, Size == "small"), FUN = median)$ending),
        1)))

print("General - DOS")
print(c(round(mean(aggregate(duration ~ Site + Species, data = subset(dataset_dos, Size == "big"), FUN = median)$duration-
               aggregate(duration ~ Site + Species, data = subset(dataset_dos, Size == "small"), FUN = median)$duration),
              1),
        
        round(sd(aggregate(duration ~ Site + Species, data = subset(dataset_dos, Size == "big"), FUN = median)$duration-
             aggregate(duration ~ Site + Species, data = subset(dataset_dos, Size == "small"), FUN = median)$duration),
             1)))

print("---------------------------------------")


print("------------ Differences 2 ------------")

print("SOS - male vs velke")
print(testing.sos.diff.small.big(dataset_sos))

print("EOS - male vs velke")
print(testing.eos.diff.small.big(dataset_eos))

print("DOS - male vs velke")
print(testing.dos.diff.small.big(dataset_dos))

print("SOS - mezi druhy")
print(testing.sos.same.size.sppdiff(dataset_sos))

print("EOS - mezi druhy")
print(testing.eos.same.size.sppdiff(dataset_eos))

print("DOS - mezi druhy")
print(testing.dos.same.size.sppdiff(dataset_dos))

print("---------------------------------------")

## Figure making ####
dataset<-add.x(dataset)

# Calculates medians for each x a Size
median_data <- aggregate(beginning ~ Site + Species + Size, dataset, median)
median_data$ending <- aggregate(ending ~ Site + Species + Size, dataset, median)$ending
median_data <- add.x(median_data)

# LOESS models pro "big" a "small"
loess_begin_big <- loess(beginning ~ x, data = median_data[median_data$Size == "big", ], span = 0.75)
loess_begin_small <- loess(beginning ~ x, data = median_data[median_data$Size == "small", ], span = 0.75)

loess_end_big <- loess(ending ~ x, data = median_data[median_data$Size == "big", ], span = 0.75)
loess_end_small <- loess(ending ~ x, data = median_data[median_data$Size == "small", ], span = 0.75)

# adding predicted values back to the dataset
x_seq <- seq(min(median_data$x), max(median_data$x), length.out = 100)  # Jemnější škála

pred_big_begin <- predict(loess_begin_big, newdata = data.frame(x = x_seq))
pred_small_begin <- predict(loess_begin_small, newdata = data.frame(x = x_seq))

pred_big_end <- predict(loess_end_big, newdata = data.frame(x = x_seq))
pred_small_end <- predict(loess_end_small, newdata = data.frame(x = x_seq))

# Making dataset for LOESS models (ommitted after revision)
loess_df <- data.frame(
  x = rep(x_seq, 4),
  y = c(pred_big_begin, pred_small_begin, pred_big_end, pred_small_end),
  Type = rep(c("Beginning Big", "Beginning Small", "Ending Big", "Ending Small"), each = length(x_seq))
)

g<-ggplot(dataset)
g<-g+geom_boxplot(aes(x=x, y=beginning, fill=Species, alpha=Size, group=grp, colour = Size), outliers = F)
g<-g+geom_boxplot(aes(x=x, y=ending, fill=Species, alpha=Size, group=grp, colour = Size), outliers = F)
g<-g+scale_colour_manual(breaks=c("Beginning Big", "Beginning Small", "Ending Big", "Ending Small", names(boxcols)), 
                         values=c("#FA0E00", "#FAAE7B", "#FA0E00", "#FAAE7B", boxcols))
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("DOY",limits=c(100,300),breaks=seq(100,300,25))
g<-my.theme(g)
figure<-g

figure













