## Functions ####

## my.theme ####
# Apply a classic ggplot2 theme with black axes and adjustable legend position.
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

## prepare.rel.gr.toDBH.dataset ####
# Prepare a dataset of absolute and relative DBH increments by site, size, tree, year, and day phase
# for either Growth or TWD periods.
prepare.rel.gr.toDBH.dataset<-function(input,type="Growth"){
  
  ## testing arguments
  # input=dendrometer_data
  
  input$EU_PCAB_bigTrees$site<-"Eustaska"
  input$EU_PCAB_smallTrees$site<-"Eustaska"
  input$BB_PCAB_bigTrees$site<-"Boubin"
  input$BB_PCAB_smallTrees$site<-"Boubin"
  input$ZF_PCAB_bigTrees$site<-"Zofin"
  input$ZF_PCAB_smallTrees$site<-"Zofin"
  input$BB_FASY_bigTrees$site<-"Boubin"
  input$BB_FASY_smallTrees$site<-"Boubin"
  input$ZF_FASY_bigTrees$site<-"Zofin"
  input$ZF_FASY_smallTrees$site<-"Zofin"
  input$RN_ACCA_bigTrees$site<-"Ranspurk"
  input$RN_ACCA_smallTrees$site<-"Ranspurk"
  input$RN_CABE_bigTrees$site<-"Ranspurk"
  input$RN_CABE_smallTrees$site<-"Ranspurk"
  input$RN_ULSP_bigTrees$site<-"Ranspurk"
  input$RN_ULSP_smallTrees$site<-"Ranspurk"
  
  input$EU_PCAB_bigTrees$size<-"big"
  input$EU_PCAB_smallTrees$size<-"small"
  input$BB_PCAB_bigTrees$size<-"big"
  input$BB_PCAB_smallTrees$size<-"small"
  input$ZF_PCAB_bigTrees$size<-"big"
  input$ZF_PCAB_smallTrees$size<-"small"
  input$BB_FASY_bigTrees$size<-"big"
  input$BB_FASY_smallTrees$size<-"small"
  input$ZF_FASY_bigTrees$size<-"big"
  input$ZF_FASY_smallTrees$size<-"small"
  input$RN_ACCA_bigTrees$size<-"big"
  input$RN_ACCA_smallTrees$size<-"small"
  input$RN_CABE_bigTrees$size<-"big"
  input$RN_CABE_smallTrees$size<-"small"
  input$RN_ULSP_bigTrees$size<-"big"
  input$RN_ULSP_smallTrees$size<-"small"
  
  if(type=="Growth"){
    output<-data.frame(Site=character(), Size=character(), Tree=character(), Species=character(), 
                       Year=numeric(), DOY=numeric(), Hour=numeric(), DayPhase=character(), 
                       DBH_value=numeric())
    
    for(i in names(input)){
      sub.site<-input[[i]]
      for(j in unique(sub.site$Tree)){
        sub.tree<-subset(sub.site, Tree==j & GrowthPhase=="Growth")
        
        for(k in unique(sub.tree$Year)){
          
          # print(c(i,j,k))
          
          sub.tree.year<-subset(sub.tree, Year==k)
          # sub.tree.year<-sub.tree.year[order(sub.tree.year$DOY, sub.tree.year$Hour),]
          
          sub.tree.year <- sub.tree.year %>%
            arrange(DOY, Hour)
          
          sub.tree.year$incr<-NA
          sub.tree.year$rel.incr<-NA
          # sub.tree.year$incr[2:nrow(sub.tree.year)]<-sub.tree.year$DBH_zero_increment[2:nrow(sub.tree.year)]-sub.tree.year$DBH_zero_increment[1:(nrow(sub.tree.year)-1)]
          sub.tree.year$incr<-sub.tree.year$DBH_zero_increment
          sub.tree.year$rel.incr<-sub.tree.year$incr/sub.tree.year$DBH_value
          
          temp<-sub.tree.year[c(2:nrow(sub.tree.year)),c("site", "size", "Tree", "Species", "Year", "DOY", "Hour", "DayPhase","DBH_value","incr","rel.incr")]
          names(temp) <-c("Site", "Size", "Tree", "Species", "Year", "DOY", "Hour", "DayPhase","DBH_value","incr","rel.incr")
          
          output<-rbind(output,temp)
        }
      }
    }
    dataset<-output
  }
  
  if(type=="TWD"){
    
    dataset<-do.call(rbind,input)[,c("site", "size", "Tree", "Species", "Year", "DOY", "Hour", "GrowthPhase", "DayPhase","DBH_value","DBH_increment")]
    dataset<-subset(dataset,GrowthPhase=="TWD")
    dataset <- dataset %>% select(-"GrowthPhase")
    names(dataset) <-c("Site", "Size", "Tree", "Species", "Year", "DOY", "Hour", "DayPhase","DBH_value","incr")
    dataset$rel.incr<-dataset$incr/dataset$DBH_value
  }
  
  dataset$x<-1
  dataset$x[which(dataset$site=="Ranspurk_ACCA_big")]<-1
  dataset$x[which(dataset$site=="Ranspurk_ACCA_small")]<-2
  dataset$x[which(dataset$site=="Ranspurk_CABE_big")]<-3
  dataset$x[which(dataset$site=="Ranspurk_CABE_small")]<-4
  dataset$x[which(dataset$site=="Zofin_PCAB_big")]<-5
  dataset$x[which(dataset$site=="Zofin_PCAB_small")]<-6
  dataset$x[which(dataset$site=="Zofin_FASY_big")]<-7
  dataset$x[which(dataset$site=="Zofin_FASY_small")]<-8
  dataset$x[which(dataset$site=="Boubin_PCAB_big")]<-9
  dataset$x[which(dataset$site=="Boubin_PCAB_small")]<-10
  dataset$x[which(dataset$site=="Boubin_FASY_big")]<-11
  dataset$x[which(dataset$site=="Boubin_FASY_small")]<-12
  dataset$x[which(dataset$site=="Eustaska_PCAB_big")]<-13
  dataset$x[which(dataset$site=="Eustaska_PCAB_small")]<-14
  
  return(dataset)
}

## add.x ####
# Assign numeric x positions based on tree size and day phase for plotting.
add.x<-function(input){
  
  ## testing arguments
  # input=dataset
  
  input$grp<-paste0(input$Size,"_",input$DayPhase)
  
  input$x<-NA
  input$x[which(input$grp=="big_All")]<-1
  input$x[which(input$grp=="small_All")]<-2
  input$x[which(input$grp=="big_Sunrise")]<-3
  input$x[which(input$grp=="small_Sunrise")]<-4
  input$x[which(input$grp=="big_Day")]<-5
  input$x[which(input$grp=="small_Day")]<-6
  input$x[which(input$grp=="big_Sunset")]<-7
  input$x[which(input$grp=="small_Sunset")]<-8
  input$x[which(input$grp=="big_Night")]<-9
  input$x[which(input$grp=="small_Night")]<-10
  
  return(input)
}

## plot.data ####
# Plot relative stem increments by day phase and tree size using boxplots.
plot.data<-function(input){
  
  ## testing arguments
  # input=subset(dataset,Site=="Boubin" & Species=="FASY")
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x,y=rel.incr,group=grp,alpha=Size),fill=species.colors[unique(input$Species)], outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_x_continuous("",
                          limits=c(0.5,10.5),
                          breaks=seq(1.5,9.5,2),
                          labels=c("All day","Sunrise", "Day", "Sunset", "Night"))
  g<-g+scale_y_continuous("Stem increment (% of DBH)",limits=c(0,0.00020),breaks=seq(0,1,0.00005),labels=formatC(seq(0,100,0.005),format="f",digits=3))
  g<-g+geom_vline(xintercept = c(2.5, 4.5, 6.5, 8.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-my.theme(g)
  g
}

## test.differences.size.category ####
# Test species differences in relative increments within a site separately for big and small trees.
test.differences.size.category<-function(site){
  
  ## testing arguments
  # site="Ranspurk"
  
  desired_comparisons <- c(
    "All_ACCA - All_CABE", 
    "Sunrise_ACCA - Sunrise_CABE", 
    "Day_ACCA - Day_CABE", 
    "Sunset_ACCA - Sunset_CABE", 
    "Night_ACCA - Night_CABE"
  )
  
  input<-subset(dataset,Site==site & Size=="big")
  input$DayPhaseSp<-paste0(input$DayPhase, "_", input$Species)
  output_big<-as.data.frame(dunn.test(input$rel.incr, input$DayPhaseSp, table = F))
  output_big <- output_big[output_big$comparisons %in% desired_comparisons, ]
  output_big<-output_big[,c("comparisons", "P.adjusted")]
  names(output_big)<-c("Comparison", "pval")
  output_big$pval<-round(output_big$pval, 3)
  output_big$size<-"big"
  
  input<-subset(dataset,Site==site & Size=="small")
  input$DayPhaseSp<-paste0(input$DayPhase, "_", input$Species)
  output_small<-as.data.frame(dunn.test(input$rel.incr, input$DayPhaseSp, table = F))
  output_small <- output_small[output_small$comparisons %in% desired_comparisons, ]
  output_small<-output_small[,c("comparisons", "P.adjusted")]
  names(output_small)<-c("Comparison", "pval")
  output_small$pval<-round(output_small$pval, 3)
  output_small$size<-"small"
  
  output<-rbind(output_big, output_small)
  
  return(output)
}

## test.differences.species ####
# Test size-related differences in relative increments within a species across day phases.
test.differences.species<-function(site,species){
  
  ## testing arguments
  # site="Zofin"
  # species="PCAB"
  
  desired_comparisons <- c(
    "All_big - All_small", 
    "Sunrise_big - Sunrise_small", 
    "Day_big - Day_small", 
    "Sunset_big - Sunset_small", 
    "Night_big - Night_small"
  )
  
  input<-subset(dataset,Site==site & Species==species)
  input$DayPhaseSp<-paste0(input$DayPhase, "_", input$Size)
  output<-as.data.frame(dunn.test(input$rel.incr, input$DayPhaseSp, table = F))
  output<- output[output$comparisons %in% desired_comparisons, ]
  output<-output[,c("comparisons", "P.adjusted")]
  names(output)<-c("Comparison", "pval")
  output$pval<-round(output$pval, 3)
  output$Site<-site
  output$Species<-species
  return(output)
}

## Data calculations ####
dataset<-prepare.rel.gr.toDBH.dataset(dendrometer_data,"Growth")
temp<-dataset
temp$DayPhase<-"All"
dataset<-rbind(temp,dataset)

# Data analysis ####
differences<-rbind(test.differences.species("Ranspurk","ACCA"),
                   test.differences.species("Ranspurk","CABE"),
                   test.differences.species("Zofin","PCAB"),
                   test.differences.species("Zofin","FASY"),
                   test.differences.species("Boubin","PCAB"),
                   test.differences.species("Boubin","FASY"),
                   test.differences.species("Eustaska","PCAB"))

# Printing results ####
print("------------- Differences -------------")
print(differences)
print("---------------------------------------")

## Figure making ####
dataset<-add.x(dataset)

figure<-ggarrange(plot.data(subset(dataset,Site=="Ranspurk" & Species=="ACCA")),
                  plot.data(subset(dataset,Site=="Ranspurk" & Species=="CABE")),
                  plot.data(subset(dataset,Site=="Ranspurk" & Species=="ULSP")),
                  plot.data(subset(dataset,Site=="Zofin" & Species=="PCAB")),
                  plot.data(subset(dataset,Site=="Zofin" & Species=="FASY")),
                  plot.data(subset(dataset,Site=="Boubin" & Species=="PCAB")),
                  plot.data(subset(dataset,Site=="Boubin" & Species=="FASY")),
                  plot.data(subset(dataset,Site=="Eustaska" & Species=="PCAB")),
                  nrow=4,ncol=2,align="hv",common.legend=T,legend="bottom",labels=LETTERS[1:10])
