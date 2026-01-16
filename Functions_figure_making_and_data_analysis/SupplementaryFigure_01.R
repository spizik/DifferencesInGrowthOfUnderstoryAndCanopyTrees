## Functions ####

## calculate.data ####
# Calculate differences in start and end of season based on two growth thresholds.
calculate.data <- function(input, site, size){
  
  ## testing argiments
  # input = dendrometer_data$BB_FASY_bigTrees
  
  input_25 <- input
  input_50 <- subset(input, Normalized_growth >= 0.05 & Normalized_growth  <= 0.95)
  
  
  agg_sos_25 <- aggregate(DOY ~ Tree + Species + Year, data = input_25, FUN = min)
  agg_eos_25 <- aggregate(DOY ~ Tree + Species + Year, data = input_25, FUN = max)
  
  agg_sos_50 <- aggregate(DOY ~ Tree + Species + Year, data = input_50, FUN = min)
  agg_eos_50 <- aggregate(DOY ~ Tree + Species + Year, data = input_50, FUN = max)
  
  output_sos <- merge(
    agg_sos_25,
    agg_sos_50,
    by = c("Tree", "Species", "Year"),
    suffixes = c("_sos_25", "_sos_50")
  )
  output_sos$difference_sos <- output_sos$DOY_sos_50 - output_sos$DOY_sos_25
  
  output_eos <- merge(
    agg_eos_25,
    agg_eos_50,
    by = c("Tree", "Species", "Year"),
    suffixes = c("_eos_25", "_eos_50")
  )
  output_eos$difference_eos <- output_eos$DOY_eos_25 - output_eos$DOY_eos_50
  
  output <- merge(
    output_sos,
    output_eos,
    by = c("Tree", "Species", "Year")
  )
  
  output$Site <- site
  output$Size <- size
  
  return(output)
  
}

## my.theme ####
# Apply a classic ggplot2 theme with black axes and configurable legend position.
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
# Assign numeric x positions based on site, species, and size combinations.
add.x<-function(input){
  
  ## testing argiments
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

## plot.timing.differences ####
# Plot differences in season timing (SOS or EOS) across sites, species, and size classes.
plot.timing.differences <- function(input, time){
  
  ## testing argiments
  # input = dataset
  # time = "SOS"
  
  g <- ggplot(input)
  
  if(time == "SOS") g <- g + geom_boxplot(aes(x = x, y = difference_sos, fill=Species, alpha=Size, group=grp), outliers = F)
  if(time == "EOS") g <- g + geom_boxplot(aes(x = x, y = difference_eos, fill=Species, alpha=Size, group=grp), outliers = F)
  
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  
  g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
  
  if(time == "SOS") g<-g+scale_y_continuous("Difference in SOS",limits=c(0,15),breaks=seq(0,100,3))
  if(time == "EOS") g<-g+scale_y_continuous("Difference in EOS",limits=c(0,15),breaks=seq(0,100,3))
  
  g<-g+scale_x_continuous("",
                          limits=c(0.5,16.5),
                          breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                          labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
  
  
  g <- my.theme(g)
  g
}

## Dataset preparation ####
dataset <- rbind(calculate.data(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
                 calculate.data(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small"),
                 calculate.data(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
                 calculate.data(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small"),
                 
                 calculate.data(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
                 calculate.data(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"),
                 
                 calculate.data(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
                 calculate.data(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small"),
                 calculate.data(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
                 calculate.data(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small"),
                 calculate.data(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
                 calculate.data(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small"),
                 
                 calculate.data(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
                 calculate.data(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small"),
                 calculate.data(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
                 calculate.data(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small"))

## Figugure finalizations ####
dataset <- add.x(dataset)

print(paste0("Difference in number of days in SOS betveen 0.025 and 0.05 treshold: ", mean(dataset$difference_sos)))
print(paste0("Difference in number of days in EOS betveen 0.025 and 0.05 treshold: ", mean(dataset$difference_eos)))

figure <- ggarrange(plot.timing.differences(dataset, "SOS"),
                    plot.timing.differences(dataset, "EOS"),
                    nrow = 2, ncol = 1, align = "hv", common.legend = T, legend = "bottom",
                    labels = LETTERS[1:2])
