
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
# Assign fixed numeric x-axis positions based on site, species, and tree size combinations for plotting.
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

## prepare.rel.gr.toDBH.dataset ####
# Construct a combined dataset of relative DBH increments during growth or TWD phases across all sites, species, and tree sizes.
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
  
  return(dataset)
}

## sum.periods.data.general ####
# Summarize the relative contribution of day phases to total relative growth increment for a given site, species, and size class.
sum.periods.data.general<-function(input, site, species, size){
  
  ## testing arguments
  # input=dataset.specific
  # site="Ranspurk"
  # species="ACCA"
  # size="big"
  
  dta<-subset(input,Site==site & Species==species & Size==size)
  
  incr.byPhase<-aggregate(rel.incr~DayPhase+Tree+Year,data=dta,FUN=sum)
  incr.general<-aggregate(rel.incr~Tree+Year,data=dta,FUN=sum)
  
  combined<-merge(incr.byPhase,incr.general, by = c("Tree", "Year"), all = TRUE)
  names(combined)<-c("Tree", "Year", "DayPhase", "byPhase", "total")
  combined$rel<-combined$byPhase/combined$total
  
  output<-aggregate(rel~DayPhase,data=combined,FUN=sum)
  output$rel<-output$rel/length(unique(paste0(combined$Tree,"_",combined$Year)))
  # output<-aggregate(rel~DayPhase,data=combined,FUN=mean)
  
  output$Site<-site
  output$Size<-size
  output$Species<-unique(dta$Species)
  
  return(output)
}

## test.differences ####
# Test size-related differences in relative growth contribution across day phases for a given site and species.
test.differences<-function(site, species, test="kw-test"){
  
  ## testing arguments
  # site="Ranspurk"
  # species="ACCA"
  # test="kw-test"
  
  input<-subset(dataset,Site==site & Species==species)
  
  if(test=="anova"){
    all<-summary(aov(rel~Size,data=input))
    sunrise<-summary(aov(rel~Size,data=subset(input,DayPhase=="Sunrise")))
    day<-summary(aov(rel~Size,data=subset(input,DayPhase=="Day")))
    sunset<-summary(aov(rel~Size,data=subset(input,DayPhase=="Sunset")))
    night<-summary(aov(rel~Size,data=subset(input,DayPhase=="Night")))
    
    all<-round(all[[1]][["Pr(>F)"]][1],3)
    sunrise<-round(sunrise[[1]][["Pr(>F)"]][1],3)
    day<-round(day[[1]][["Pr(>F)"]][1],3)
    sunset<-round(sunset[[1]][["Pr(>F)"]][1],3)
    night<-round(night[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    all<-kruskal.test(rel~Size,data=input)
    sunrise<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Sunrise"))
    day<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Day"))
    sunset<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Sunset"))
    night<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Night"))
    
    all<-round(all$p.value,3)
    sunrise<-round(sunrise$p.value,3)
    day<-round(day$p.value,3)
    sunset<-round(sunset$p.value,3)
    night<-round(night$p.value,3)
  }
  if(test=="wilcox"){
    all<-wilcox.test(rel~Size,data=input)
    sunrise<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Sunrise"))
    day<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Day"))
    sunset<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Sunset"))
    night<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Night"))
    
    all<-round(all$p.value,3)
    sunrise<-round(sunrise$p.value,3)
    day<-round(day$p.value,3)
    sunset<-round(sunset$p.value,3)
    night<-round(night$p.value,3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     all=all,
                     sunrise=sunrise,
                     day=day,
                     sunset=sunset,
                     night=night)
  return(output)
}

## Calculations ####
dataset.specific<-prepare.rel.gr.toDBH.dataset(dendrometer_data,"Growth")

dataset<-rbind(sum.periods.data.general(dataset.specific, "Ranspurk", "ACCA", "big"),
               sum.periods.data.general(dataset.specific, "Ranspurk", "ACCA", "small"),
               sum.periods.data.general(dataset.specific, "Ranspurk", "CABE", "big"),
               sum.periods.data.general(dataset.specific, "Ranspurk", "CABE", "small"),
               sum.periods.data.general(dataset.specific, "Ranspurk", "ULSP", "big"),
               sum.periods.data.general(dataset.specific, "Ranspurk", "ULSP", "small"),
               
               sum.periods.data.general(dataset.specific, "Zofin", "PCAB", "big"),
               sum.periods.data.general(dataset.specific, "Zofin", "PCAB", "small"),
               sum.periods.data.general(dataset.specific, "Zofin", "FASY", "big"),
               sum.periods.data.general(dataset.specific, "Zofin", "FASY", "small"),
               
               sum.periods.data.general(dataset.specific, "Boubin", "PCAB", "big"),
               sum.periods.data.general(dataset.specific, "Boubin", "PCAB", "small"),
               sum.periods.data.general(dataset.specific, "Boubin", "FASY", "big"),
               sum.periods.data.general(dataset.specific, "Boubin", "FASY", "small"),
               
               sum.periods.data.general(dataset.specific, "Eustaska", "PCAB", "big"),
               sum.periods.data.general(dataset.specific, "Eustaska", "PCAB", "small"))

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
print("---------------------------------------")

## Figure ####
# dataset<-add.x.old(dataset)
dataset<-add.x(dataset)
dataset<-rename.dayphases(dataset)

cols.dayphase.adj<-cols.dayphase
names(cols.dayphase.adj)<-c("a.Night", "d.Sunrise", "c.Day", "b.Sunset")

g<-ggplot(dataset)
g<-g+geom_bar(aes(x=x, y=rel, fill=DayPhase, alpha=Size),stat="identity", width = 0.75)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))

# g<-g+geom_vline(xintercept = c(4.5, 8.5, 12.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
# g<-g+scale_x_continuous("",
#                         limits=c(0.5,15.5),
#                         breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5),
#                         labels=c("Ranspurk ACCA", "Ranspurk CABE", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Proportion of realized growth (%)",limits=c(0,1.05),breaks=seq(0,1,0.2),labels=seq(0,100,20))
g<-g+scale_fill_manual(breaks=names(cols.dayphase.adj), values=cols.dayphase.adj)
g<-my.theme(g)
figure<-g
