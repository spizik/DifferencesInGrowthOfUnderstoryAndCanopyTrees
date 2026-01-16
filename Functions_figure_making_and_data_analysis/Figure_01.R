## Functions ####
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

## Graph 1 - growth ####
example_tree<-subset(dendrometer_data$ZF_PCAB_bigTrees, Tree=="2J__45_0172" & Year==2021)

example_tree$x1<-paste0(example_tree$DOY,"_",example_tree$Hour)
example_tree<-example_tree[order(example_tree$DOY, example_tree$Hour),]
example_tree$x<-c(1:nrow(example_tree))

segments <- example_tree %>%
  arrange(x) %>%
  mutate(
    xend = lead(x),
    yend = lead(Normalized_growth),
    phase = GrowthPhase
  ) %>%
  filter(!is.na(xend)) 


g<-ggplot(segments)
g<-g+geom_segment(aes(x = x, y = Normalized_growth, xend = xend, yend = yend, color = phase), size = 1, lineend = "round")
g<-g+geom_line(aes(x=x,y=Normalized_zero_growth),colour="#AAAAAA",linewidth=0.25)
g<-g+geom_hline(yintercept=c(0.025,0.975), linetype="dashed", colour="#AAAAAA")
g<-g+scale_color_manual(values=cols.phases,breaks=names(cols.phases))
g<-g+scale_y_continuous("Percentage of anual growth (%)",limits=c(0,1),breaks=seq(0,1,0.2),labels=formatC(seq(0,1,0.2),format="f",digits=1))
g<-g+scale_x_continuous("DOY",limits=c(-28,3236),breaks=seq(-28,3236,240), labels=seq(115,250,10))
g<-my.theme(g)
panel1<-g
g

## Graph 2 - tileplot ####
g<-ggplot(example_tree)
g<-g+geom_tile(aes(x=x,y=1,fill=GrowthPhase))
g<-g+scale_y_continuous("",limits=c(0.5,1.5),breaks=c(0,2))
g<-g+scale_x_continuous("DOY",limits=c(-28,3236),breaks=seq(-28,3236,240), labels=seq(115,250,10))
g<-g+scale_fill_manual(values=cols.phases,breaks=names(cols.phases))
g<-my.theme(g,"none")
panel2<-g

## Graph 3 - Dayphases ####
days_in_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# Generate sequences for months, days, and hours
months <- rep(1:12, times = days_in_month)
days <- unlist(lapply(days_in_month, seq))
hours <- rep(0:23, times = length(days))  # 24 hours for each day

# Repeat the days and months for each hour
days <- rep(days, each = 24)
months <- rep(months, each = 24)

simulated_day<-data.frame(Year=2020,
                          Month=months,
                          Day=days,
                          Hour=hours)
simulated_day$DOY<-rep(c(1:366),each=24)
simulated_day<-determine.day.phase(simulated_day,50,15)

g<-ggplot(simulated_day)
g<-g+geom_tile(aes(x=DOY,y=Hour,fill=DayPhase))
g<-g+scale_y_continuous("Hour",limits=c(0,23),breaks=seq(0,23,4))
g<-g+scale_x_continuous("DOY",limits=c(115,255),breaks=seq(115,250,10))
g<-g+scale_fill_manual(values=cols.dayphase,breaks=names(cols.dayphase))
g<-my.theme(g)
panel3<-g

## Finalizing outputs ####

figure<-ggarrange(panel1,panel2,panel3,align="hv",labels=LETTERS[1:3],nrow=3,ncol=1, heights=c(0.4,0.2,0.4))
