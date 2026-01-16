# Functions ####

## prepare.data.days ####
# Summarize normalized increments across the growing season timeline.
# Calculates daily position within the growing season, min/max increments,
# and bootstrap confidence intervals for each day of season.
prepare.data.days<-function(input, size){
  
  ## testing arguments
  # input=dendrometer_data$BB_PCAB_smallTrees
  # site="Boubin"
  # species="PCAB"
  # size="big"
  
  temp_growth<-aggregate(Normalized_zero_increment ~ Year + DOY + Tree, data=subset(input, GrowthPhase == "Growth"), FUN=mean)
  
  temp <- temp_growth
  
  GS_begs<-aggregate(DOY ~ Year, data=input, FUN=min)
  temp$GS_progress <- NA
  for(i in 1:nrow(GS_begs)) {
    temp$GS_progress[which(temp$Year==GS_begs$Year[i])] <- 1+temp$DOY[which(temp$Year==GS_begs$Year[i])]-GS_begs$DOY[which(GS_begs$Year==GS_begs$Year[i])]
  }
  
  output<-data.frame(#Site=site,
                     #Species=species,
                     Size=size,
                     GS_progress=c(1:200),
                     min_growth=NA,
                     max_growth=NA,
                     min_ci_growth=NA,
                     mid_ci_growth=NA,
                     max_ci_growth=NA)
  
  for(i in output$GS_progress){
    
    # i=2
    
    sub <- subset(temp, GS_progress==i)
    
    output$max_growth[i]<-max(sub$Normalized_zero_increment, na.rm=T)
    output$min_growth[i]<-min(sub$Normalized_zero_increment, na.rm=T)
    
    boot_data <- sub$Normalized_zero_increment
    
    if(length(boot_data)>=2) output[i, c("min_ci_growth","mid_ci_growth","max_ci_growth")]<-quantile(apply(replicate(1000,sample(boot_data,length(boot_data),T)),2,mean),probs=c(0.025,0.500,0.975))
  }
  
  output<-na.omit(output)
  
  return(output)
}

## prepare.data.growthperc ####
# Summarize normalized increments by proportion of realized annual growth.
# Computes min/max increments and bootstrap confidence intervals
# within growth-percent classes.
prepare.data.growthperc<-function(input, size){
  
  ## testing arguments
  # input=dendrometer_data$BB_PCAB_smallTrees
  # site="Boubin"
  # species="PCAB"
  # size="big"
  
  limits<-seq(0,1,0.05)
  labs<-seq(0.05,1,0.05)
  
  input$class<-cut(input$Normalized_zero_growth,limits,labs)
  
  temp_growth<-aggregate(Normalized_zero_increment ~ Year + class + Tree, data=subset(input, GrowthPhase == "Growth"), FUN=mean)
 
  temp <- temp_growth
  
  output<-data.frame(#Site=site,
                     #Species=species,
                     Size=size,
                     GS_progress=labs,
                     min_growth=NA,
                     max_growth=NA,
                     min_ci_growth=NA,
                     mid_ci_growth=NA,
                     max_ci_growth=NA)
  
  for(i in 1:nrow(output)){
    
    # i=0.05
    
    sub <- subset(temp, class==output$GS_progress[i])
    
    output$max_growth[i]<-max(sub$Normalized_zero_increment, na.rm=T)
    output$min_growth[i]<-min(sub$Normalized_zero_increment, na.rm=T)
    
    boot_data <- sub$Normalized_zero_increment
    
    if(length(boot_data)>=2) output[i, c("min_ci_growth","mid_ci_growth","max_ci_growth")]<-quantile(apply(replicate(1000,sample(boot_data,length(boot_data),T)),2,mean),probs=c(0.025,0.500,0.975))
  }
  
  output<-na.omit(output)
  
  return(output)
}

## plot.data.days ####
# Plot seasonal trajectories of normalized increments across the growing season.
# Includes confidence ribbons, median trends, and fitted GLM predictions
# comparing big vs. small trees.
plot.data.days<-function(input){
  
  ## testing arguments
  # input=rbind(prepare.data.days(dendrometer_data$ZF_PCAB_bigTrees,"big"),
  #             prepare.data.days(dendrometer_data$ZF_PCAB_smallTrees,"small"))
  
  l<-glm(mid_ci_growth ~ GS_progress * Size, family = quasibinomial(), data = input)
  summary(l)
  new_data <- data.frame(GS_progress = c(c(1:(max(input$GS_progress[which(input$Size=="big")])+1)),
                                         c(1:(max(input$GS_progress[which(input$Size=="small")])+1))),
                         Size = c(rep("big",max(input$GS_progress[which(input$Size=="big")])+1),
                                  rep("small",max(input$GS_progress[which(input$Size=="small")])+1)))
  
  new_data$predicted_growth <- predict(l, newdata = new_data, type = "response")
  l<-summary(l)
  
  input$max_ci_growth[which(input$max_ci_growth>0.015)]<-0.0125
  
  g<-ggplot(input)
  g<-g+geom_ribbon(aes(x=GS_progress, ymin=min_ci_growth, ymax=max_ci_growth, fill=Size),alpha=0.25)
  g<-g+geom_line(aes(x=GS_progress, y=mid_ci_growth, colour=Size))
  g<-g+geom_line(data=new_data, aes(x=GS_progress, y=predicted_growth, colour=Size), linetype="dashed", linewidth=1.1, inherit.aes=F)
  
  g<-g+scale_x_continuous("Day of the growing season", limits=c(0,175), breaks=seq(0,500,25))
  g<-g+scale_y_continuous("Normaliyed increments (% of annual growth)", limits=c(0,0.015), breaks=seq(0,1,.0025), labels=formatC(seq(0,100,0.25), format="f", digits=2))
  if(l$coefficients[4,4]<0.1) g<-g+annotate("text",x=170,y=0.0125,label="*",size=10,colour="#000000")
  g<-my.theme(g)
  
  g
}

## plot.data.growthperc ####
# Plot normalized increments against proportion of realized annual growth.
# Shows confidence ribbons, median trends, and GLM-based predictions
# for big vs. small trees.
plot.data.growthperc<-function(input){
  
  ## testing arguments
  # input=rbind(prepare.data.growthperc(dendrometer_data$BB_FASY_bigTrees,"big"),
  #             prepare.data.growthperc(dendrometer_data$BB_FASY_smallTrees,"small"))
  
  input$GS_progress<-input$GS_progress-0.025
  
  l<-glm(mid_ci_growth ~ GS_progress * Size, family = quasibinomial(), data = input)
  new_data <- data.frame(GS_progress = rep(seq(0.025,0.975,0.05),times=2),
                         Size = rep(unique(input$Size),each=20))
  new_data$predicted_growth <- predict(l, newdata = new_data, type = "response")
  l<-summary(l)
  
  input$max_ci_growth[which(input$max_ci_growth>0.015)]<-0.015
  
  g<-ggplot(input)
  g<-g+geom_ribbon(aes(x=GS_progress, ymin=min_ci_growth, ymax=max_ci_growth, fill=Size),alpha=0.25)
  g<-g+geom_line(aes(x=GS_progress, y=mid_ci_growth, colour=Size))
  g<-g+geom_line(data=new_data, aes(x=GS_progress, y=predicted_growth, colour=Size), linetype="dashed", linewidth=1.1, inherit.aes=F)
  g<-g+scale_x_continuous("Proportion of realized growth (%)", limits=c(0,1), breaks=seq(0,1,.20), labels=seq(0,100,20))
  g<-g+scale_y_continuous("Normaliyed increments (% of annual growth)", limits=c(0,0.015), breaks=seq(0,1,.0025), labels=formatC(seq(0,100,0.25), format="f", digits=2))
  if(l$coefficients[4,4]<0.1) g<-g+annotate("text",x=0.90,y=0.0125,label="*",size=10,colour="#000000")
  g<-my.theme(g)
  g
}

# Figure making ####
figure<-ggarrange(plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$RN_ACCA_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$RN_ACCA_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$RN_ACCA_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$RN_ACCA_smallTrees,"small"))),
                  
                  plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$RN_CABE_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$RN_CABE_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$RN_CABE_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$RN_CABE_smallTrees,"small"))),
                  
                  plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$RN_ULSP_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$RN_ULSP_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$RN_ULSP_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$RN_ULSP_smallTrees,"small"))),
                  
                  #Zofin
                  plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$ZF_PCAB_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$ZF_PCAB_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$ZF_PCAB_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$ZF_PCAB_smallTrees,"small"))),
                  
                  plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$ZF_FASY_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$ZF_FASY_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$ZF_FASY_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$ZF_FASY_smallTrees,"small"))),
                  
                  #Boubin
                  plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$BB_PCAB_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$BB_PCAB_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$BB_PCAB_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$BB_PCAB_smallTrees,"small"))),
                  
                  plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$BB_FASY_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$BB_FASY_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$BB_FASY_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$BB_FASY_smallTrees,"small"))),
                  
                  #Eustaska
                  plot.data.growthperc(rbind(prepare.data.growthperc(dendrometer_data$EU_PCAB_bigTrees,"big"),
                                             prepare.data.growthperc(dendrometer_data$EU_PCAB_smallTrees,"small"))),
                  plot.data.days(rbind(prepare.data.days(dendrometer_data$EU_PCAB_bigTrees,"big"),
                                       prepare.data.days(dendrometer_data$EU_PCAB_smallTrees,"small"))),
                  
                  nrow=8,ncol=2,align="hv",labels=LETTERS[1:20],common.legend=T,legend="bottom")











