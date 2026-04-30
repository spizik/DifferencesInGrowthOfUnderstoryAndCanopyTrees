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

## calculate.data ####
# Calculate relative proportion of growth or TWD periods by day phase for each tree and year.
calculate.data<-function(input, site, size, variable="Growth"){
  
  ## testing arguments
  # input=dendrometer_data$ZF_PCAB_smallTrees
  # site="Zogin"
  # size="big"
  # variable="Growth"
  
  output<-aggregate(DBH_value~Tree+Year+DayPhase,input,length)
  names(output)<-c("Tree","Year","DayPhase","All_periods")
  
  temp<-aggregate(DBH_value~Tree+Year,input,length)
  temp$DayPhase<-"All"
  temp<-temp[,c("Tree","Year","DayPhase","DBH_value")]
  names(temp)<-c("Tree","Year","DayPhase","All_periods")
  
  output<-rbind(temp,output)
  
  if(variable=="Growth"){
    temp_dayPhase<-aggregate(Normalized_zero_increment~Tree+Year+DayPhase, subset(input,GrowthPhase=="Growth"),length)
    temp<-aggregate(Normalized_zero_increment~Tree+Year, subset(input,GrowthPhase=="Growth"),length)
  }
  if(variable=="TWD"){
    temp_dayPhase<-aggregate(Normalized_zero_increment~Tree+Year+DayPhase, subset(input,GrowthPhase=="TWD"),length)
    temp<-aggregate(Normalized_zero_increment~Tree+Year, subset(input,GrowthPhase=="TWD"),length)
  }
  
  names(temp_dayPhase)<-c("Tree","Year","DayPhase","No_periods")
  temp$DayPhase<-"All"
  temp<-temp[,c("Tree","Year","DayPhase","Normalized_zero_increment")]
  names(temp)<-c("Tree","Year","DayPhase","No_periods")
  temp<-rbind(temp,temp_dayPhase)
  
  
  output$No_periods<-NA
  output$No_periods[which(paste0(output$Tree,"_",output$Year,"_",output$DayPhase) %in% 
                          paste0(temp$Tree,"_",temp$Year,"_",output$DayPhase))]<-temp$No_periods[which(paste0(output$Tree,"_",output$Year,"_",output$DayPhase) %in% 
                                                                                                              paste0(temp$Tree,"_",temp$Year,"_",output$DayPhase))]
  
  
  
  output$Rel_periods<-output$No_periods/output$All_periods
  
  output$Site<-site
  output$Species<-unique(input$Species)
  output$Size<-size
  
  return(output)
}

## prepare.rel.gr.toDBH.dataset ####
# Prepare dataset of relative DBH increments during Growth or TWD phases across all sites and sizes.
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
          sub.tree.year$incr[2:nrow(sub.tree.year)]<-sub.tree.year$DBH_zero_growth[2:nrow(sub.tree.year)]-sub.tree.year$DBH_zero_growth[1:(nrow(sub.tree.year)-1)]
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
# Assign numeric x positions based on size and day phase for plotting.
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
# Create boxplots of relative growth-period proportions by day phase and size.
plot.data<-function(input){
  
  ## testing arguments
  # input=subset(dataset,Site=="Boubin" & Species=="PCAB")
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x,y=Rel_periods,group=grp,alpha=Size, colour=Size),fill=species.colors[unique(input$Species)], outliers=F)
  g<-g+scale_colour_manual(breaks=names(boxcols), values=boxcols)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_x_continuous("",
                          limits=c(0.5,10.5),
                          breaks=seq(1.5,9.5,2),
                          labels=c("All day","Sunrise", "Day", "Sunset", "Night"))
  g<-g+scale_y_continuous("Number of Growing periods (%)",limits=c(0,0.75),breaks=seq(0,1,0.15),labels=seq(0,100,15))
  g<-g+geom_vline(xintercept = c(2.5, 4.5, 6.5, 8.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  g<-my.theme(g)
  g
}

## plot.data.increment ####
# Create boxplots of relative stem increment normalized by DBH for size categories.
plot.data.increment<-function(input){
  
  ## testing arguments
  # input=subset(dataset_growth,Site=="Boubin" & Species=="FASY")
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x,y=rel.incr,group=Size,alpha=Size),fill=species.colors[unique(input$Species)], outliers=F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_x_continuous("",
                          limits=c(0.5,2.5),
                          breaks=seq(1,2,1),
                          labels=c("Canopy","Understory"))
  g<-g+scale_y_continuous("Stem increment (% of DBH)",limits=c(0,0.00020),breaks=seq(0,1,0.00005),labels=formatC(seq(0,100,0.005),format="f",digits=3))
  g<-my.theme(g)
  g
}

## test.differences.size.category ####
# Test species differences in relative growth-period proportions within each size class.
test.differences.size.category<-function(site){
  # site="Eustaska"
  # species = "PCAB"
  
  input<-subset(dataset, Site==site & Species ==species)
  
  allDay <- summary(lmer(Rel_periods   ~ Size + (1|Tree) + (1|Year), data = subset(input, DayPhase == "All")))
  Sunrise <- summary(lmer(Rel_periods   ~ Size + (1|Tree) + (1|Year), data = subset(input, DayPhase == "Sunrise")))
  Day <- summary(lmer(Rel_periods   ~ Size + (1|Tree) + (1|Year), data = subset(input, DayPhase == "Day")))
  Sunset <- summary(lmer(Rel_periods   ~ Size + (1|Tree) + (1|Year), data = subset(input, DayPhase == "Sunset")))
  Night <- summary(lmer(Rel_periods   ~ Size + (1|Tree) + (1|Year), data = subset(input, DayPhase == "Night")))
  
  allDay <- round(allDay$coefficients[,"Pr(>|t|)"][2],3)
  Sunrise <- round(Sunrise$coefficients[,"Pr(>|t|)"][2],3)
  Day <- round(Day$coefficients[,"Pr(>|t|)"][2],3)
  Sunset <- round(Sunset$coefficients[,"Pr(>|t|)"][2],3)
  Night <- round(Night$coefficients[,"Pr(>|t|)"][2],3)
  
  output<-data.frame(Site = site,
                     Species = species, 
                     All = allDay,
                     Sunrise = Sunrise,
                     Day = Day,
                     Sunset = Sunset,
                     Night, Night)
  
  return(output)
}

## test.differences.withinsize ####
# Test day-phase differences in relative growth-period proportions within a size class.
test.differences.withinsize<-function(site, species){
  # site="Zofin"
  # species="PCAB"
  
  input<-subset(dataset, Site==site & Species == species)
  
  big_all_sunrise <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Sunrise") & Size == "big")))
  big_all_day <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Day") & Size == "big")))
  big_all_sunset <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Sunset") & Size == "big")))
  big_all_night <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Night") & Size == "big")))
  
  big_all_sunrise <- round(big_all_sunrise$coefficients[,"Pr(>|t|)"][2],3)
  big_all_day <- round(big_all_day$coefficients[,"Pr(>|t|)"][2],3)
  big_all_sunset <- round(big_all_sunset$coefficients[,"Pr(>|t|)"][2],3)
  big_all_night <- round(big_all_night$coefficients[,"Pr(>|t|)"][2],3)
  
  output_big<-data.frame(Site = site,
                     Species = species, 
                     Size = "big",
                     all_sunrise = big_all_sunrise,
                     all_day = big_all_day,
                     all_sunset = big_all_sunset,
                     all_night = big_all_night)
  
  
  small_all_sunrise <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Sunrise") & Size == "small")))
  small_all_day <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Day") & Size == "small")))
  small_all_sunset <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Sunset") & Size == "small")))
  small_all_night <- summary(lmer(Rel_periods   ~ DayPhase + (1|Tree) + (1|Year), data = subset(input, DayPhase %in% c("All", "Night") & Size == "small")))
  
  small_all_sunrise <- round(small_all_sunrise$coefficients[,"Pr(>|t|)"][2],3)
  small_all_day <- round(small_all_day$coefficients[,"Pr(>|t|)"][2],3)
  small_all_sunset <- round(small_all_sunset$coefficients[,"Pr(>|t|)"][2],3)
  small_all_night <- round(small_all_night$coefficients[,"Pr(>|t|)"][2],3)
  
  output_small<-data.frame(Site = site,
                     Species = species, 
                     Size = "small",
                     all_sunrise = small_all_sunrise,
                     all_day = small_all_day,
                     all_sunset = small_all_sunset,
                     all_night = small_all_night)
  
  
  output<-rbind(output_big, output_small)
  
  return(output)
}

## test.differences.species ####
# Test size-related differences in relative growth-period proportions across day phases.
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
  output<-as.data.frame(dunn.test(input$Rel_periods, input$DayPhaseSp, table = F))
  output<- output[output$comparisons %in% desired_comparisons, ]
  output<-output[,c("comparisons", "P.adjusted")]
  names(output)<-c("Comparison", "pval")
  output$pval<-round(output$pval, 3)
  output$Site<-site
  output$Species<-species
  return(output)
}

## Data calculations ####
dataset<-rbind(calculate.data(dendrometer_data$RN_ACCA_bigTrees,"Ranspurk","big"),
               calculate.data(dendrometer_data$RN_ACCA_smallTrees,"Ranspurk","small"),
               calculate.data(dendrometer_data$RN_CABE_bigTrees,"Ranspurk","big"),
               calculate.data(dendrometer_data$RN_CABE_smallTrees,"Ranspurk","small"),
               calculate.data(dendrometer_data$RN_ULSP_bigTrees,"Ranspurk","big"),
               calculate.data(dendrometer_data$RN_ULSP_smallTrees,"Ranspurk","small"),
               calculate.data(dendrometer_data$ZF_PCAB_bigTrees,"Zofin","big"),
               calculate.data(dendrometer_data$ZF_PCAB_smallTrees,"Zofin","small"),
               calculate.data(dendrometer_data$ZF_FASY_bigTrees,"Zofin","big"),
               calculate.data(dendrometer_data$ZF_FASY_smallTrees,"Zofin","small"),
               calculate.data(dendrometer_data$BB_PCAB_bigTrees,"Boubin","big"),
               calculate.data(dendrometer_data$BB_PCAB_smallTrees,"Boubin","small"),
               calculate.data(dendrometer_data$BB_FASY_bigTrees,"Boubin","big"),
               calculate.data(dendrometer_data$BB_FASY_smallTrees,"Boubin","small"),
               calculate.data(dendrometer_data$EU_PCAB_bigTrees,"Eustaska","big"),
               calculate.data(dendrometer_data$EU_PCAB_smallTrees,"Eustaska","small"))

dataset_growth<-prepare.rel.gr.toDBH.dataset(dendrometer_data,"Growth")

## Testing differences ####
differences<-rbind(test.differences.species("Ranspurk","ACCA"),
                   test.differences.species("Ranspurk","CABE"),
                   test.differences.species("Ranspurk","ULSP"),
                   test.differences.species("Zofin","PCAB"),
                   test.differences.species("Zofin","FASY"),
                   test.differences.species("Boubin","PCAB"),
                   test.differences.species("Boubin","FASY"),
                   test.differences.species("Eustaska","PCAB"))

differences.2<-rbind(test.differences.withinsize("Ranspurk","ACCA"),
                   test.differences.withinsize("Ranspurk","CABE"),
                   test.differences.withinsize("Ranspurk","ULSP"),
                   test.differences.withinsize("Zofin","PCAB"),
                   test.differences.withinsize("Zofin","FASY"),
                   test.differences.withinsize("Boubin","PCAB"),
                   test.differences.withinsize("Boubin","FASY"),
                   test.differences.withinsize("Eustaska","PCAB"))

subset(differences.2, pval>0.05)


print("------------- Differences -------------")
print(differences)
per<-aggregate(Rel_periods~Site + Species + Size + DayPhase , data=dataset, FUN=mean)
per$Rel_periods<-round(per$Rel_periods*100,1)
print(per)
print("---------------------------------------")

print("----- Differences / within groups -----")
differences.2
print("---------------------------------------")

## Data analysis ####
dataset <- add.x(dataset)
dataset_growth$x <- 1
dataset_growth$x[which(dataset_growth$Size == "small")] <- 2

figure<-ggarrange(plot.data.increment(subset(dataset_growth,Site=="Ranspurk" & Species=="ACCA")),
                  plot.data(subset(dataset,Site=="Ranspurk" & Species=="ACCA")),
                  
                  plot.data.increment(subset(dataset_growth,Site=="Ranspurk" & Species=="CABE")),
                  plot.data(subset(dataset,Site=="Ranspurk" & Species=="CABE")),
                  
                  plot.data.increment(subset(dataset_growth,Site=="Ranspurk" & Species=="ULSP")),
                  plot.data(subset(dataset,Site=="Ranspurk" & Species=="ULSP")),
                  
                  plot.data.increment(subset(dataset_growth,Site=="Zofin" & Species=="PCAB")),
                  plot.data(subset(dataset,Site=="Zofin" & Species=="PCAB")),
                  
                  plot.data.increment(subset(dataset_growth,Site=="Zofin" & Species=="FASY")),
                  plot.data(subset(dataset,Site=="Zofin" & Species=="FASY")),
                  
                  plot.data.increment(subset(dataset_growth,Site=="Boubin" & Species=="PCAB")),
                  plot.data(subset(dataset,Site=="Boubin" & Species=="PCAB")),
                  
                  plot.data.increment(subset(dataset_growth,Site=="Boubin" & Species=="FASY")),
                  plot.data(subset(dataset,Site=="Boubin" & Species=="FASY")),
                  
                  plot.data.increment(subset(dataset_growth,Site=="Eustaska" & Species=="PCAB")),
                  plot.data(subset(dataset,Site=="Eustaska" & Species=="PCAB")),
                  nrow = 4, ncol = 4, align = "hv", common.legend = T, legend = "bottom",
                  widths = c(0.15, 0.35, 0.155, 0.35),
                  labels=c(LETTERS[1:5], rep("",5), LETTERS[6:10], rep("",5)))
