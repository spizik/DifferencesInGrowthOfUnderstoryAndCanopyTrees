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

## calculate.percentiles ####
# Calculate DOY of growth start, selected growth percentiles, and growth end for each tree and year.
calculate.percentiles<-function(input){
  
  ## testing arguments
  # input=dendrometer_data$BB_FASY_bigTrees
  
  dates.beg<-aggregate(DOY~Tree+Year,data=input,FUN=min)
  
  dates.25<-aggregate(DOY~Tree+Year,data=subset(input,Normalized_zero_growth>0.25),FUN=min)
  dates.50<-aggregate(DOY~Tree+Year,data=subset(input,Normalized_zero_growth>0.50),FUN=min)
  dates.75<-aggregate(DOY~Tree+Year,data=subset(input,Normalized_zero_growth>0.75),FUN=min)
  
  dates.end<-aggregate(DOY~Tree+Year,data=input,FUN=max)
  
  dates.beg$percentile<-"GS_beg"
  dates.25$percentile<-"25th"
  dates.50$percentile<-"50th"
  dates.75$percentile<-"75th"
  dates.end$percentile<-"GS_end"
  
  output<-rbind(dates.beg, dates.25, dates.50, dates.75, dates.end)
  return(output)
}

## sum.data ####
# Combine percentile-based growth timing for big and small trees into one dataset.
sum.data<-function(input.big, input.small){
  
  ## testing arguments
  # input.big=dendrometer_data$BB_FASY_bigTrees
  # input.small=dendrometer_data$BB_FASY_smallTrees
  
  big<-calculate.percentiles(input.big)
  big$size<-"big"
  
  small<-calculate.percentiles(input.small)
  small$size<-"small"
  
  output<-rbind(big, small)
  output$grp<-paste0(output$size,"_",output$percentile)
  output$species<-unique(input.big$Species)
  
  return(output)
}

## plot.percentile.growth ####
# Visualize growth timing percentiles (DOY) for big vs. small trees and test size differences.
plot.percentile.growth<-function(input){
  
  ## testing arguments
  # input=sum.data(dendrometer_data$BB_PCAB_bigTrees, dendrometer_data$BB_PCAB_smallTrees)
  
  input$x<-1
  input$x[which(input$percentile=="GS_beg")]<-1
  input$x[which(input$percentile=="25th")]<-2
  input$x[which(input$percentile=="50th")]<-3
  input$x[which(input$percentile=="75th")]<-4
  input$x[which(input$percentile=="GS_end")]<-5
  
  input$x[which(input$size=="big")]<-input$x[which(input$size=="big")]-0.2
  input$x[which(input$size=="small")]<-input$x[which(input$size=="small")]+0.2
  
  col<-species.colors[unique(input$species)]
  
  g<-ggplot(input)
  g<-g+geom_boxplot(aes(x=x, y=DOY, alpha=size, group=grp), fill=col, outliers = F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_x_continuous("",
                          limits=c(0.5,5.5),
                          breaks=c(1:5),
                          labels=c("10th", "25th", "50th", "75th", "90th"))
  g<-g+scale_y_continuous("DOY",limits=c(120,255),breaks=seq(0,365,25),labels=seq(0,365,25))
  if(kruskal.test(DOY~size,data=subset(input,percentile=="GS_beg"))$p.value<0.05) g<-g+annotate("text",label="*",y=250,size=8,colour=col,x=1)
  if(kruskal.test(DOY~size,data=subset(input,percentile=="25th"))$p.value<0.05) g<-g+annotate("text",label="*",y=250,size=8,colour=col,x=2)
  if(kruskal.test(DOY~size,data=subset(input,percentile=="50th"))$p.value<0.05) g<-g+annotate("text",label="*",y=250,size=8,colour=col,x=3)
  if(kruskal.test(DOY~size,data=subset(input,percentile=="75th"))$p.value<0.05) g<-g+annotate("text",label="*",y=250,size=8,colour=col,x=4)
  if(kruskal.test(DOY~size,data=subset(input,percentile=="GS_end"))$p.value<0.05) g<-g+annotate("text",label="*",y=250,size=8,colour=col,x=4)
  g<-my.theme(g)
  g
}

## Data caculations ####
dataset <- rbind(sum.data(dendrometer_data$RN_ACCA_bigTrees, dendrometer_data$RN_ACCA_smallTrees),
                 sum.data(dendrometer_data$RN_CABE_bigTrees, dendrometer_data$RN_CABE_smallTrees),
                 sum.data(dendrometer_data$RN_ULSP_bigTrees, dendrometer_data$RN_ULSP_smallTrees),
                 sum.data(dendrometer_data$ZF_PCAB_bigTrees, dendrometer_data$ZF_PCAB_smallTrees),
                 sum.data(dendrometer_data$ZF_FASY_bigTrees, dendrometer_data$ZF_FASY_smallTrees),
                 sum.data(dendrometer_data$BB_PCAB_bigTrees, dendrometer_data$BB_PCAB_smallTrees),
                 sum.data(dendrometer_data$BB_FASY_bigTrees, dendrometer_data$BB_FASY_smallTrees),
                 sum.data(dendrometer_data$EU_PCAB_bigTrees, dendrometer_data$EU_PCAB_smallTrees))

dta<-aggregate(DOY~size+percentile+species,
               data=dataset,
               FUN=median)

## Printing results ####
print("Percentile 25")          
print(round(mean(subset(dta, percentile=="25th" & size == "big")$DOY - subset(dta, percentile=="25th" & size == "small")$DOY),1))
print(round(sd(subset(dta, percentile=="25th" & size == "big")$DOY - subset(dta, percentile=="25th" & size == "small")$DOY),1))

print("Percentile 50")
print(round(mean(subset(dta, percentile=="50th" & size == "big")$DOY - subset(dta, percentile=="50th" & size == "small")$DOY),1))
print(round(sd(subset(dta, percentile=="50th" & size == "big")$DOY - subset(dta, percentile=="50th" & size == "small")$DOY),1))

print("Percentile 75")
print(round(mean(subset(dta, percentile=="75th" & size == "big")$DOY - subset(dta, percentile=="75th" & size == "small")$DOY),1))
print(round(sd(subset(dta, percentile=="75th" & size == "big")$DOY - subset(dta, percentile=="75th" & size == "small")$DOY),1))

## Figure making ####
figure<-ggarrange(plot.percentile.growth(sum.data(dendrometer_data$RN_ACCA_bigTrees, dendrometer_data$RN_ACCA_smallTrees)),
                  plot.percentile.growth(sum.data(dendrometer_data$RN_CABE_bigTrees, dendrometer_data$RN_CABE_smallTrees)),
                  plot.percentile.growth(sum.data(dendrometer_data$RN_ULSP_bigTrees, dendrometer_data$RN_ULSP_smallTrees)),
                  plot.percentile.growth(sum.data(dendrometer_data$ZF_PCAB_bigTrees, dendrometer_data$ZF_PCAB_smallTrees)),
                  plot.percentile.growth(sum.data(dendrometer_data$ZF_FASY_bigTrees, dendrometer_data$ZF_FASY_smallTrees)),
                  plot.percentile.growth(sum.data(dendrometer_data$BB_PCAB_bigTrees, dendrometer_data$BB_PCAB_smallTrees)),
                  plot.percentile.growth(sum.data(dendrometer_data$BB_FASY_bigTrees, dendrometer_data$BB_FASY_smallTrees)),
                  plot.percentile.growth(sum.data(dendrometer_data$EU_PCAB_bigTrees, dendrometer_data$EU_PCAB_smallTrees)),
                  nrow=4,ncol=2,labels=LETTERS[1:10],align="hv", common.legend=T,legend="bottom")
