## Functions ####

## my.theme ####
# Apply a simple classic ggplot2 theme with black axes and adjustable legend position.
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
# Create a grouping variable (Site–Species–Size) and assign fixed numeric x positions for plotting.
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

## sum.periods.data.specific ####
# Summarize growth periods by tree, year, and day phase:
# – proportion of growth periods,
# – proportion of measurement presence,
# – total growth increment per day phase.
sum.periods.data.specific<-function(input){
  
  ## testing arguments
  # input=dendrometer_data$RN_ACCA_smallTrees
  # site="RN"
  # size="small"
  
  gr.periods<-aggregate(Tape_value~Tree+Year,data=subset(input,GrowthPhase=="Growth"),FUN=length)
  gr.periods.byPhase<-aggregate(Tape_value~DayPhase+Tree+Year,data=subset(input,GrowthPhase=="Growth"),FUN=length)
  
  periods<-aggregate(Tape_value~Tree+Year,data=input,FUN=length)
  periods.byPhase<-aggregate(Tape_value~DayPhase+Tree+Year,data=input,FUN=length)
  
  incr<-aggregate(Normalized_zero_increment~DayPhase+Tree+Year,data=subset(input,GrowthPhase=="Growth"),FUN=sum)
  
  gr.combined<-merge(gr.periods.byPhase,gr.periods, by = c("Tree", "Year"), all = TRUE)
  names(gr.combined)<-c("Tree", "Year", "DayPhase", "byPhase", "total")
  gr.combined$rel<-gr.combined$byPhase/gr.combined$total
  
  pr.combined<-merge(periods.byPhase,periods, by = c("Tree", "Year"), all = TRUE)
  names(pr.combined)<-c("Tree", "Year", "DayPhase", "byPhase", "total")
  pr.combined$rel<-pr.combined$byPhase/pr.combined$total
  
  output <- full_join(gr.combined, pr.combined, by = c("Tree", "Year", "DayPhase"))
  
  output <- output[,c("Tree", "Year", "DayPhase", "rel.x", "rel.y")]
  names(output) <- c("Tree", "Year", "DayPhase", "growth", "presence")
  
  output <- full_join(output, incr, by = c("Tree", "Year", "DayPhase"))
  names(output) <- c("Tree", "Year", "DayPhase", "growth","presence", "increment")
  
  return(output)
}

## Datasets preparation - growing hours presence ####
mod_rn_acca_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ACCA_bigTrees))
mod_rn_cabe_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_CABE_bigTrees))
mod_rn_ulsp_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ULSP_bigTrees))
mod_zf_fasy_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_FASY_bigTrees))
mod_zf_pcab_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_PCAB_bigTrees))
mod_bb_fasy_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_FASY_bigTrees))
mod_bb_pcab_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_PCAB_bigTrees))
mod_eu_pcab_big<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$EU_PCAB_bigTrees))

mod_rn_acca_small<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ACCA_smallTrees))
mod_rn_cabe_small<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_CABE_smallTrees))
mod_rn_ulsp_small<-lmer((growth - presence) ~ DayPhase  + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ULSP_smallTrees))
mod_zf_fasy_small<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_FASY_smallTrees))
mod_zf_pcab_small<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_PCAB_smallTrees))
mod_bb_fasy_small<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_FASY_smallTrees))
mod_bb_pcab_small<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_PCAB_smallTrees))
mod_eu_pcab_small<-lmer((growth - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$EU_PCAB_smallTrees))

output_gs_presence <- data.frame(Site = rep(c("Ranspurk","Ranspurk","Ranspurk","Zofin","Zofin","Boubin","Boubin","Eustaska"), each = 2), 
                                 Species = rep(c("ACCA","CABE","ULSP","FASY","PCAB","FASY","PCAB","PCAB"), each = 2), 
                                 Size = rep(c("big", "small"),times = 8),
                                 Day = NA,
                                 Night = NA,
                                 Sunrise = NA,
                                 Sunset = NA)

output_gs_presence[1, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_acca_big)$coefficients[,1]
output_gs_presence[2, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_acca_small)$coefficients[,1]
output_gs_presence[3, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_cabe_big)$coefficients[,1]
output_gs_presence[4, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_cabe_small)$coefficients[,1]
output_gs_presence[5, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_ulsp_big)$coefficients[,1]
output_gs_presence[6, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_ulsp_small)$coefficients[,1]
output_gs_presence[7, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_pcab_big)$coefficients[,1]
output_gs_presence[8, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_pcab_small)$coefficients[,1]
output_gs_presence[9, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_fasy_big)$coefficients[,1]
output_gs_presence[10, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_fasy_small)$coefficients[,1]
output_gs_presence[11, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_pcab_big)$coefficients[,1]
output_gs_presence[12, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_pcab_small)$coefficients[,1]
output_gs_presence[13, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_fasy_big)$coefficients[,1]
output_gs_presence[14, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_fasy_small)$coefficients[,1]
output_gs_presence[15, c("Day","Night","Sunrise","Sunset")] <- summary(mod_eu_pcab_small)$coefficients[,1]
output_gs_presence[16, c("Day","Night","Sunrise","Sunset")] <- summary(mod_eu_pcab_big)$coefficients[,1]

## Datasets preparation - growith ####
mod_rn_acca_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ACCA_bigTrees))
mod_rn_cabe_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_CABE_bigTrees))
mod_rn_ulsp_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ULSP_bigTrees))
mod_zf_fasy_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_FASY_bigTrees))
mod_zf_pcab_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_PCAB_bigTrees))
mod_bb_fasy_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_FASY_bigTrees))
mod_bb_pcab_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_PCAB_bigTrees))
mod_eu_pcab_big<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$EU_PCAB_bigTrees))

mod_rn_acca_small<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ACCA_smallTrees))
mod_rn_cabe_small<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_CABE_smallTrees))
mod_rn_ulsp_small<-lmer((increment - presence) ~ DayPhase  + (1|Year), data = sum.periods.data.specific(dendrometer_data$RN_ULSP_smallTrees))
mod_zf_fasy_small<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_FASY_smallTrees))
mod_zf_pcab_small<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$ZF_PCAB_smallTrees))
mod_bb_fasy_small<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_FASY_smallTrees))
mod_bb_pcab_small<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$BB_PCAB_smallTrees))
mod_eu_pcab_small<-lmer((increment - presence) ~ DayPhase + (1|Tree) + (1|Year), data = sum.periods.data.specific(dendrometer_data$EU_PCAB_smallTrees))

output_growth <- data.frame(Site = rep(c("Ranspurk","Ranspurk","Ranspurk","Zofin","Zofin","Boubin","Boubin","Eustaska"), each = 2), 
                            Species = rep(c("ACCA","CABE","ULSP","FASY","PCAB","FASY","PCAB","PCAB"), each = 2), 
                            Size = rep(c("big", "small"),times = 8),
                            Day = NA,
                            Night = NA,
                            Sunrise = NA,
                            Sunset = NA)

output_growth[1, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_acca_big)$coefficients[,1]
output_growth[2, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_acca_small)$coefficients[,1]
output_growth[3, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_cabe_big)$coefficients[,1]
output_growth[4, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_cabe_small)$coefficients[,1]
output_growth[5, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_ulsp_big)$coefficients[,1]
output_growth[6, c("Day","Night","Sunrise","Sunset")] <- summary(mod_rn_ulsp_small)$coefficients[,1]
output_growth[7, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_pcab_big)$coefficients[,1]
output_growth[8, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_pcab_small)$coefficients[,1]
output_growth[9, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_fasy_big)$coefficients[,1]
output_growth[10, c("Day","Night","Sunrise","Sunset")] <- summary(mod_zf_fasy_small)$coefficients[,1]
output_growth[11, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_pcab_big)$coefficients[,1]
output_growth[12, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_pcab_small)$coefficients[,1]
output_growth[13, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_fasy_big)$coefficients[,1]
output_growth[14, c("Day","Night","Sunrise","Sunset")] <- summary(mod_bb_fasy_small)$coefficients[,1]
output_growth[15, c("Day","Night","Sunrise","Sunset")] <- summary(mod_eu_pcab_small)$coefficients[,1]
output_growth[16, c("Day","Night","Sunrise","Sunset")] <- summary(mod_eu_pcab_big)$coefficients[,1]

## Printing results ####
print("-----------------------------------------------------")
print("Difference between distribution of growings season hours and used growth hours")
print("-----------------------------------------------------")
print("Night")
print(round(c(mean(subset(output_gs_presence,Size=="big")$Night), sd(subset(output_gs_presence,Size=="big")$Night))*100,1))
print(round(c(mean(subset(output_gs_presence,Size=="small")$Night), sd(subset(output_gs_presence,Size=="small")$Night))*100,1))

print("Sunrise")
print(round(c(mean(subset(output_gs_presence,Size=="big")$Sunrise), sd(subset(output_gs_presence,Size=="big")$Sunrise))*100,1))
print(round(c(mean(subset(output_gs_presence,Size=="small")$Sunrise), sd(subset(output_gs_presence,Size=="small")$Sunrise))*100,1))

print("Sunset")
print(round(c(mean(subset(output_gs_presence,Size=="big")$Sunset), sd(subset(output_gs_presence,Size=="big")$Sunset))*100,1))
print(round(c(mean(subset(output_gs_presence,Size=="small")$Sunset), sd(subset(output_gs_presence,Size=="small")$Sunset))*100,1))

print("Day")
print(round(c(mean(subset(output_gs_presence,Size=="big")$Day), sd(subset(output_gs_presence,Size=="big")$Day))*100,1))
print(round(c(mean(subset(output_gs_presence,Size=="small")$Day), sd(subset(output_gs_presence,Size=="small")$Day))*100,1))

print("-----------------------------------------------------")
print("Difference between distribution of growings season hours and realized growth")
print("-----------------------------------------------------")
print("Night")
print(round(c(mean(subset(output_growth,Size=="big")$Night), sd(subset(output_growth,Size=="big")$Night))*100,1))
print(round(c(mean(subset(output_growth,Size=="small")$Night), sd(subset(output_growth,Size=="small")$Night))*100,1))

print("Sunrise")
print(round(c(mean(subset(output_growth,Size=="big")$Sunrise), sd(subset(output_growth,Size=="big")$Sunrise))*100,1))
print(round(c(mean(subset(output_growth,Size=="small")$Sunrise), sd(subset(output_growth,Size=="small")$Sunrise))*100,1))

print("Sunset")
print(round(c(mean(subset(output_growth,Size=="big")$Sunset), sd(subset(output_growth,Size=="big")$Sunset))*100,1))
print(round(c(mean(subset(output_growth,Size=="small")$Sunset), sd(subset(output_growth,Size=="small")$Sunset))*100,1))

print("Day")
print(round(c(mean(subset(output_growth,Size=="big")$Day), sd(subset(output_growth,Size=="big")$Day))*100,1))
print(round(c(mean(subset(output_growth,Size=="small")$Day), sd(subset(output_growth,Size=="small")$Day))*100,1))


## Graph making ####
## Panel A
output_gs_presence<-add.x(output_gs_presence)
output_gs_presence<-melt(output_gs_presence, id.vars=c("Site", "Species", "Size", "grp", "x"))

linerange<-aggregate(value ~ Site + Species + Size, output_gs_presence, min)
linerange$max<-aggregate(value ~ Site + Species + Size, output_gs_presence, max)$value
linerange<-add.x(linerange)

cols.fin <- c(species.colors, cols.dayphase)

g<-ggplot(output_gs_presence)
g<-g+geom_linerange(data=linerange, aes(x=x, ymin=value, ymax=max, colour=Species, alpha=Size), linewidth = 0.75, inherit.aes=F)
g<-g+geom_point(aes(x=x, y=value, colour=variable, alpha=Size),size=4)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))

g<-g+geom_hline(yintercept = 0,  linetype = "dotted", color = "#000000", linewidth = 0.75)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Difference against expected distribution (%)",limits=c(-0.25,0.35),breaks=seq(-1,1,0.1),labels=seq(-100,100,10))
g<-g+scale_colour_manual(breaks=names(cols.fin), values=cols.fin)
g<-my.theme(g, "none")
figure_a<-g

## Panel B
output_growth<-add.x(output_growth)
output_growth<-melt(output_growth, id.vars=c("Site", "Species", "Size", "grp", "x"))

linerange<-aggregate(value ~ Site + Species + Size, output_growth, min)
linerange$max<-aggregate(value ~ Site + Species + Size, output_growth, max)$value
linerange<-add.x(linerange)

cols.fin <- c(species.colors, cols.dayphase)

g<-ggplot(output_growth)
g<-g+geom_linerange(data=linerange, aes(x=x, ymin=value, ymax=max, colour=Species, alpha=Size), linewidth = 0.75, inherit.aes=F)
g<-g+geom_point(aes(x=x, y=value, colour=variable, alpha=Size),size=4)
g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))

g<-g+geom_hline(yintercept = 0,  linetype = "dotted", color = "#000000", linewidth = 0.75)
g<-g+geom_vline(xintercept = c(6.5, 10.5, 14.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_x_continuous("",
                        limits=c(0.5,16.5),
                        breaks=c(1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))

g<-g+scale_y_continuous("Difference against expected distribution (%)",limits=c(-0.25,0.35),breaks=seq(-1,1,0.1),labels=seq(-100,100,10))
g<-g+scale_colour_manual(breaks=names(cols.fin), values=cols.fin)
g<-my.theme(g, "none")
figure_b<-g

## Figure finalization
figure<-ggarrange(figure_a, figure_b, nrow=2,ncol=1,align="hv",common.legend=T,legend="bottom",labels=LETTERS[1:2])



