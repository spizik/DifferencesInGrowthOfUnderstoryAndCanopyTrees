## Functions ####

## my.theme ####
# Apply a classic ggplot2 theme with black axes and a user-defined legend position.
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
# Add a numeric x-position variable based on site–species combinations for plotting.
add.x<-function(input){
  
  ## testing arguments
  # input=gro.periods.data
  
  input$grp<-paste0(input$Site,"_",input$Species)
  
  input$x<-NA
  input$x[which(input$grp=="Ranspurk_ACCA")]<-1
  input$x[which(input$grp=="Ranspurk_CABE")]<-2
  input$x[which(input$grp=="Ranspurk_ULSP")]<-3
  
  input$x[which(input$grp=="Zofin_PCAB")]<-4
  input$x[which(input$grp=="Zofin_FASY")]<-5
  
  input$x[which(input$grp=="Boubin_PCAB")]<-6
  input$x[which(input$grp=="Boubin_FASY")]<-7
  
  input$x[which(input$grp=="Eustaska_PCAB")]<-8
  
  return(input)
}

## calculate.GS.length.data ####
# Calculate start, end, and length of the growing season for each tree and year from dendrometer data.
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

## calculate.timing.differences ####
# Quantify differences in start and end of the growing season between big and small trees at the site–species level.
calculate.timing.differences <- function(input_big, input_small){
  
  ## testing arguments
  # input_big = calculate.GS.length.data(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big")
  # input_small = calculate.GS.length.data(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small")
  
  
  big_sos <- aggregate(beginning ~ Site + Species + Year, data = input_big, mean)
  big_eos <- aggregate(ending ~ Site + Species + Year, data = input_big, mean)
  
  small_sos <- aggregate(beginning ~ Site + Species + Year, data = input_small, mean)
  small_eos <- aggregate(ending ~ Site + Species + Year, data = input_small, mean)
  
  sos_merged <- merge(
    big_sos, small_sos,
    by = c("Site", "Species", "Year"),
    suffixes = c("_big", "_small")
  )
  
  eos_merged <- merge(
    big_eos, small_eos,
    by = c("Site", "Species", "Year"),
    suffixes = c("_big", "_small")
  )
  
  sos_merged$difference <- sos_merged$beginning_small - sos_merged$beginning_big 
  eos_merged$difference <- eos_merged$ending_small - eos_merged$ending_big 
  
  output_sos <- data.frame(Site = unique(big_sos$Site),
                           Species = unique(big_sos$Species),
                           Parametr = "SOS",
                           min = min(sos_merged$difference),
                           mid = mean(sos_merged$difference),
                           max = max(sos_merged$difference))
  
  output_eos <- data.frame(Site = unique(big_sos$Site),
                           Species = unique(big_sos$Species),
                           Parametr = "EOS",
                           min = min(eos_merged$difference),
                           mid = mean(eos_merged$difference),
                           max = max(eos_merged$difference))
  
  output <- rbind(output_sos, output_eos)
  
  return(output)
  
}

## Calculations ####
dataset<-rbind(calculate.timing.differences(calculate.GS.length.data(dendrometer_data$RN_ACCA_bigTrees, "Ranspurk", "big"),
                                            calculate.GS.length.data(dendrometer_data$RN_ACCA_smallTrees, "Ranspurk", "small")),
               calculate.timing.differences(calculate.GS.length.data(dendrometer_data$RN_CABE_bigTrees, "Ranspurk", "big"),
                                            calculate.GS.length.data(dendrometer_data$RN_CABE_smallTrees, "Ranspurk", "small")),
               calculate.timing.differences(calculate.GS.length.data(dendrometer_data$RN_ULSP_bigTrees, "Ranspurk", "big"),
                                            calculate.GS.length.data(dendrometer_data$RN_ULSP_smallTrees, "Ranspurk", "small")),
               
               calculate.timing.differences(calculate.GS.length.data(dendrometer_data$ZF_PCAB_bigTrees, "Zofin", "big"),
                                            calculate.GS.length.data(dendrometer_data$ZF_PCAB_smallTrees, "Zofin", "small")),
               calculate.timing.differences(calculate.GS.length.data(dendrometer_data$ZF_FASY_bigTrees, "Zofin", "big"),
                                            calculate.GS.length.data(dendrometer_data$ZF_FASY_smallTrees, "Zofin", "small")),
               
               calculate.timing.differences(calculate.GS.length.data(dendrometer_data$BB_PCAB_bigTrees, "Boubin", "big"),
                                            calculate.GS.length.data(dendrometer_data$BB_PCAB_smallTrees, "Boubin", "small")),
               calculate.timing.differences(calculate.GS.length.data(dendrometer_data$BB_FASY_bigTrees, "Boubin", "big"),
                                            calculate.GS.length.data(dendrometer_data$BB_FASY_smallTrees, "Boubin", "small")),
               
               calculate.timing.differences(calculate.GS.length.data(dendrometer_data$EU_PCAB_bigTrees, "Eustaska", "big"),
                                            calculate.GS.length.data(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small")))



dataset <- add.x(dataset)

## Data plotting ####
show_data <- subset(dataset, Parametr == "SOS")

g <- ggplot(show_data)
g <- g + geom_vline(xintercept = c(3.5, 5.5, 7.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g <- g + geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.75, colour = "#000000")
g <- g + geom_linerange(aes(x = x, ymin = min, ymax = max, colour = Species), linewidth = 1.15)
g <- g + geom_point(aes(x = x, y = mid, colour = Species), size = 3)
g <- g + scale_colour_manual(breaks=names(species.colors), values=species.colors)
g <- g + scale_y_continuous("Number of days",limits = c(-90,45), breaks=seq(-100,100,10))
g <- g + scale_x_continuous("",
                            limits=c(0.5,8.5),
                            breaks=c(1:8),
                            labels=c("Maple", "Hornbeam", "Elm", "Spruce", "Beech", "Spruce", "Beech", "Spruce"))
g <- my.theme(g, "none")
g.sos <- g

## Data plotting ####
show_data <- subset(dataset, Parametr == "EOS")

g <- ggplot(show_data)
g <- g + geom_vline(xintercept = c(3.5, 5.5, 7.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g <- g + geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.75, colour = "#000000")
g <- g + geom_linerange(aes(x = x, ymin = min, ymax = max, colour = Species), linewidth = 1.15)
g <- g + geom_point(aes(x = x, y = mid, colour = Species), size = 3)
g <- g + scale_colour_manual(breaks=names(species.colors), values=species.colors)
g <- g + scale_y_continuous("Number of days",limits = c(-90,50), breaks=seq(-100,100,10))
g <- g + scale_x_continuous("",
                            limits=c(0.5,8.5),
                            breaks=c(1:8),
                            labels=c("Maple", "Hornbeam", "Elm", "Spruce", "Beech", "Spruce", "Beech", "Spruce"))
g <- my.theme(g, "none")
g.eos <- g

## Finalizing figures
figure <- ggarrange(g.sos, g.eos, nrow = 1, ncol = 2, align = "hv", labels = c("B", "C"), legend = "none")
