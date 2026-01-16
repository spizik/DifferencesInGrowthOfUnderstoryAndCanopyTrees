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

## rename.dayphases ####
# Rename day-phase categories to prefixed labels to enforce a desired plotting order.
rename.dayphases<-function(input){
  input$DayPhase[which(input$DayPhase=="Sunrise")]<-"d.Sunrise"
  input$DayPhase[which(input$DayPhase=="Day")]<-"c.Day"
  input$DayPhase[which(input$DayPhase=="Sunset")]<-"b.Sunset"
  input$DayPhase[which(input$DayPhase=="Night")]<-"a.Night"
  return(input)
}

## sum.periods.data.specific ####
# Calculate the relative contribution of each day phase to growth periods for individual trees and years.
sum.periods.data.specific<-function(input, site, size){
  
  ## testing arguments
  # input=dendrometer_data$RN_ACCA_smallTrees
  # site="RN"
  # size="small"
  
  gr.periods<-aggregate(Tape_value~Tree+Year,data=subset(input,GrowthPhase=="Growth"),FUN=length)
  gr.periods.byPhase<-aggregate(Tape_value~DayPhase+Tree+Year,data=subset(input,GrowthPhase=="Growth"),FUN=length)
  
  combined<-merge(gr.periods.byPhase,gr.periods, by = c("Tree", "Year"), all = TRUE)
  names(combined)<-c("Tree", "Year", "DayPhase", "byPhase", "total")
  combined$rel<-combined$byPhase/combined$total
  
  output<-combined
  
  output$Site<-site
  output$Size<-size
  output$Species<-unique(input$Species)
  
  return(output)
}

## sum.periods.data.general ####
# Summarize average relative contributions of day phases to growth periods at the site–species–size level.
sum.periods.data.general<-function(input, site, size){
  
  ## testing arguments
  # input=dendrometer_data$RN_ACCA_smallTrees
  # site="RN"
  # size="big"
  
  dta<-sum.periods.data.specific(input,site,size)
  
  output<-aggregate(rel~DayPhase,data=dta,FUN=sum)
  output$rel<-output$rel/length(unique(paste0(dta$Tree,"_",dta$Year)))
  
  output$Site<-site
  output$Size<-size
  output$Species<-unique(input$Species)
  
  return(output)
}

## test.differences ####
# Test size-related differences in the relative contribution of day phases to growth periods for a given site and species.
test.differences<-function(site, species, test="kw-test"){
  
  ## testing arguments
  # site="Ranspurk"
  # species="ACCA"
  # test="kw-test"
  
  input<-subset(dataset.trees,Site==site & Species==species)
  
  if(test=="anova"){
    sunrise<-summary(aov(rel~Size,data=subset(input,DayPhase=="Sunrise")))
    day<-summary(aov(rel~Size,data=subset(input,DayPhase=="Day")))
    sunset<-summary(aov(rel~Size,data=subset(input,DayPhase=="Sunset")))
    night<-summary(aov(rel~Size,data=subset(input,DayPhase=="Night")))
    
    sunrise<-round(sunrise[[1]][["Pr(>F)"]][1],3)
    day<-round(day[[1]][["Pr(>F)"]][1],3)
    sunset<-round(sunset[[1]][["Pr(>F)"]][1],3)
    night<-round(night[[1]][["Pr(>F)"]][1],3)
  }
  if(test=="kw-test"){
    sunrise<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Sunrise"))
    day<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Day"))
    sunset<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Sunset"))
    night<-kruskal.test(rel~Size,data=subset(input,DayPhase=="Night"))
    
    sunrise<-round(sunrise$p.value,3)
    day<-round(day$p.value,3)
    sunset<-round(sunset$p.value,3)
    night<-round(night$p.value,3)
  }
  if(test=="wilcox"){
    sunrise<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Sunrise"))
    day<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Day"))
    sunset<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Sunset"))
    night<-wilcox.test(rel~Size,data=subset(input,DayPhase=="Night"))
    
    sunrise<-round(sunrise$p.value,3)
    day<-round(day$p.value,3)
    sunset<-round(sunset$p.value,3)
    night<-round(night$p.value,3)
  }
  
  output<-data.frame(Site=site,
                     Species=species,
                     sunrise=sunrise,
                     day=day,
                     sunset=sunset,
                     night=night)
  return(output)
}

## Calculations ####
dataset.trees<-rbind(sum.periods.data.specific(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
                     sum.periods.data.specific(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small"),
                     sum.periods.data.specific(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
                     sum.periods.data.specific(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small"),
                     sum.periods.data.specific(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
                     sum.periods.data.specific(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small"),
                     
                     sum.periods.data.specific(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
                     sum.periods.data.specific(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small"),
                     sum.periods.data.specific(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
                     sum.periods.data.specific(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small"),
                     
                     sum.periods.data.specific(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
                     sum.periods.data.specific(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small"),
                     sum.periods.data.specific(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
                     sum.periods.data.specific(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small"),
                     
                     sum.periods.data.specific(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
                     sum.periods.data.specific(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"))

dataset<-rbind(sum.periods.data.general(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
               sum.periods.data.general(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small"),
               sum.periods.data.general(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
               sum.periods.data.general(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small"),
               sum.periods.data.general(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
               sum.periods.data.general(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small"),
               
               sum.periods.data.general(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
               sum.periods.data.general(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small"),
               sum.periods.data.general(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
               sum.periods.data.general(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small"),
               
               sum.periods.data.general(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
               sum.periods.data.general(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small"),
               sum.periods.data.general(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
               sum.periods.data.general(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small"),
               
               sum.periods.data.general(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
               sum.periods.data.general(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"))

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
print("------------- v relativnim zastoupeni rustovych hodin behem dne v kontextu vsech rustovych hodin -------------")
print(differences)
print("---------------------------------------")

minima<-aggregate(rel~DayPhase, data=dataset, FUN=min)
maxima<-aggregate(rel~DayPhase, data=dataset, FUN=max)

minima$rel<-round(minima$rel*100,1)
maxima$rel<-round(maxima$rel*100,1)

print("---------------------------------------")
print("Minimalni podily rustovych hodin")
print(minima)
print("---------------------------------------")
print("Maximalni podily rustovych hodin")
print(maxima)
print("---------------------------------------")

p_big<-subset(dataset, DayPhase=="Day" & Size=="big")
p_big$rel<-round(p_big$rel*100,1)
print(p_big)

p_small<-subset(dataset, DayPhase=="Day" & Size=="small")
p_small$rel<-round(p_small$rel*100,1)
print(p_small)

print(round(c(subset(dataset, DayPhase=="Day" & Size=="small")$rel - subset(dataset, DayPhase=="Day" & Size=="big")$rel)*100,1))

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

g<-g+scale_y_continuous("Proportion of growing periods (%)",limits=c(0,1.05),breaks=seq(0,1,0.2),labels=seq(0,100,20))
g<-g+scale_fill_manual(breaks=names(cols.dayphase.adj), values=cols.dayphase.adj)
g<-my.theme(g)
figure<-g




