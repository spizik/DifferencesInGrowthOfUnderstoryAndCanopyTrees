## Functions ####

## plot.annual.increments ####
# Calculate and visualize annual DBH increments for big and small trees.
plot.annual.increments <- function(input_big, input_small, species){
  
  ## testing arguments
  # input_big = dendrometer_data$ZF_PCAB_bigTrees
  # input_small = dendrometer_data$ZF_PCAB_smallTrees
  # species = "PCAB"
  
  big_dta_individual <- aggregate(DBH_value ~ Tree + Year, data = input_big, FUN = min)
  names(big_dta_individual) <- c("Tree", "Year", "DBH_value_min")
  big_dta_individual$DBH_value_max <- aggregate(DBH_value ~ Tree + Year, data = input_big, FUN = max)$DBH_value
  big_dta_individual$DBH_increment <- big_dta_individual$DBH_value_max - big_dta_individual$DBH_value_min
  
  big_dta <- aggregate(DBH_increment ~ Year, data = big_dta_individual, FUN = median)
  
  big_dta_individual$size <- "big"
  big_dta$size <- "big"
  
  
  small_dta_individual <- aggregate(DBH_value ~ Tree + Year, data = input_small, FUN = min)
  names(small_dta_individual) <- c("Tree", "Year", "DBH_value_min")
  small_dta_individual$DBH_value_max <- aggregate(DBH_value ~ Tree + Year, data = input_small, FUN = max)$DBH_value
  small_dta_individual$DBH_increment <- small_dta_individual$DBH_value_max - small_dta_individual$DBH_value_min
  
  small_dta <- aggregate(DBH_increment ~ Year, data = small_dta_individual, FUN = median)
  
  small_dta_individual$size <- "small"
  small_dta$size <- "small"
  
  output <- list()
  output$individual_data <- rbind(big_dta_individual, small_dta_individual)
  output$aggregated_data <- rbind(big_dta, small_dta)
  
  
  input=output$aggregated_data
  
  to_corr <- data.frame(big = subset(input, size == "big")$DBH_increment,
                        small = subset(input, size == "small")$DBH_increment)
  
  print(cor.test(to_corr$big, to_corr$small))
  
  output$individual_data$x <- output$individual_data$Year
  output$individual_data$x[which(output$individual_data$size == "big")] <- output$individual_data$x[which(output$individual_data$size == "big")] - 0.20
  output$individual_data$x[which(output$individual_data$size == "small")] <- output$individual_data$x[which(output$individual_data$size == "small")] + 0.20
  
  output$aggregated_data$x <- output$aggregated_data$Year
  output$aggregated_data$x[which(output$aggregated_data$size == "big")] <- output$aggregated_data$x[which(output$aggregated_data$size == "big")] - 0.20
  output$aggregated_data$x[which(output$aggregated_data$size == "small")] <- output$aggregated_data$x[which(output$aggregated_data$size == "small")] + 0.20
  
  g<-ggplot(output$individual_data)
  g<-g+geom_boxplot(aes(x=x, y=DBH_increment, fill=size, group=x, alpha = size), fill = species.colors[species])
  g<-g+geom_line(data = output$aggregated_data, aes(x = x , y = DBH_increment, alpha = size), colour = species.colors[species], linewidth = 0.85, inherit.aes = F)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_x_continuous("Year",limits=c(2017.5,2024.5), breaks=seq(0,3000,1))
  g<-g+scale_y_continuous("DBH increment (mm)",limits=c(0,10), breaks=seq(0,100,2))
  g<-my.theme(g)
  g
}

## Figure finalization ####
figure<-ggarrange(plot.annual.increments(dendrometer_data$RN_ACCA_bigTrees, dendrometer_data$RN_ACCA_smallTrees, "ACCA"),
                  plot.annual.increments(dendrometer_data$RN_CABE_bigTrees, dendrometer_data$RN_CABE_smallTrees, "CABE"),
                  plot.annual.increments(dendrometer_data$RN_CABE_bigTrees, dendrometer_data$RN_CABE_smallTrees, "ULSP"),
                  plot.annual.increments(dendrometer_data$BB_PCAB_bigTrees, dendrometer_data$BB_PCAB_smallTrees, "PCAB"),
                  plot.annual.increments(dendrometer_data$BB_FASY_bigTrees, dendrometer_data$BB_FASY_smallTrees, "FASY"),
                  plot.annual.increments(dendrometer_data$ZF_PCAB_bigTrees, dendrometer_data$ZF_PCAB_smallTrees, "PCAB"),
                  plot.annual.increments(dendrometer_data$ZF_FASY_bigTrees, dendrometer_data$ZF_FASY_smallTrees, "FASY"),
                  plot.annual.increments(dendrometer_data$EU_PCAB_bigTrees, dendrometer_data$EU_PCAB_smallTrees, "PCAB"),
                  nrow=4,ncol=2,align="hv",labels=LETTERS[1:10],common.legend=T,legend="bottom")



