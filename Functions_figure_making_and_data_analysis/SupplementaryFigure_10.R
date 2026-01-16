## Functions ####

## my.theme ####
# Apply a clean ggplot2 theme with black axes and adjustable legend position.
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
# Assign numeric x-positions for site × species × size combinations
# to standardize ordering in plots.
add.x<-function(input){
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

## calculate.growing.periods ####
# Calculate the proportion of growing periods per tree and year.
# Returns total periods, growing hours, and their relative proportion.
calculate.growing.periods<-function(input,site,size){
  # input=dendrometer_data$EU_PCAB_smallTrees
  # site="Eustaska"
  # size="small"
  
  growing.periods<-aggregate(Normalized_increment~Tree + Year , data=input, FUN=length)
  names(growing.periods)<-c("Tree","Year","NoGrPeriods")
  temp<-aggregate(Normalized_increment~Tree + Year , data=subset(input, Normalized_zero_increment>0), FUN=length)
  growing.periods<-merge(growing.periods,temp, by = c("Tree", "Year"), all = TRUE)
  names(growing.periods)<-c("Tree", "Year", "NoGrPeriods", "NoGrHours")
  
  
  growing.periods$PercGrPeriods<-growing.periods$NoGrHours/growing.periods$NoGrPeriods
  
  growing.periods$Site<-site
  growing.periods$Species<-unique(input$Species)
  growing.periods$Size<-size
  
  growing.periods<-na.omit(growing.periods)
  
  return(growing.periods)
}

## prepare.dataset ####
# Merge tree-level growing-period metrics with canopy openness data.
# Produces a combined dataset for analyzing growth vs. canopy conditions.
prepare.dataset<-function(in.tree.data, in.canopy){
  # in.tree.data=calculate.growing.periods(dendrometer_data$EU_PCAB_smallTrees, "Boubin", "small")
  # in.canopy=hemi_eustaska
  
  in.canopy$Tree<-paste0(substr(in.canopy$tree,4,5),"_",substr(in.canopy$tree,6,20))
  in.canopy$Tree<-gsub("-","_",in.canopy$Tree)
  
  output<-merge(in.tree.data,in.canopy,by="Tree")
  
  output<-output[,c("Site.x","Species.x","Size","Tree","Year","NoGrPeriods","NoGrHours","PercGrPeriods","perc_canopy_open")]
  names(output)<-c("Site","Species","Size","Tree","Year","NoGrPeriods","NoGrHours","PercGrPeriods","perc_canopy_open")
  
  # output<-output[,c("Site","Species","Size","Tree","Year","NoGrPeriods","NoGrHours","PercGrPeriods","perc_canopy_open")]
  
  return(output)
}

## Data preparation ####
## Hemidata ####
## Boubin
hemi_results=hemi_boubin
site_name="BB"
site_meta=read.xlsx("Datasets/Metadata/Boubin_meta.xlsx") 

output<-hemi_results
output$Site<-site_name
output<-output[order(output$tree),]
site_meta<-site_meta[which(substr(site_meta$Device_ID,5,20) %in% toupper(substr(output$tree, 7,20))),]
output$Species<-site_meta$Species
hemi_boubin<-output

## Eustaska
hemi_results=hemi_eustaska
site_name="EU"
site_meta=read.xlsx("Datasets/Metadata/Eustaska_meta.xlsx") 
output<-hemi_results
output$Site<-site_name
output$tree<-gsub("-","_",output$tree)
output<-output[order(output$tree),]
site_meta<-site_meta[which(substr(site_meta$Device_ID,5,20) %in% toupper(substr(output$tree, 7,20))),]
output$Species<-site_meta$Species
hemi_eustaska<-output

## Ranspurk
hemi_results=hemi_ranspurk
site_name="RN"
site_meta=read.xlsx("Datasets/Metadata/Ranspurk_meta.xlsx") 
output<-hemi_results
output$Site<-site_name
output$tree<-gsub("-","_",output$tree)
output<-output[order(output$tree),]
site_meta<-site_meta[which(substr(site_meta$Device_ID,5,20) %in% toupper(substr(output$tree, 9,20))),]
output$Species<-NA
output$Species[which(toupper(substr(output$tree, 9,20)) %in% substr(site_meta$Device_ID,5,20))]<-site_meta$Species
output$Species[which(output$tree=="RN_18_4_1578")]<-"ACCA"
output$Species[which(output$tree=="RN_25_4_2135")]<-"CABE"
output$Species[which(output$tree=="RN_26_4_1114")]<-"CABE"
output$Species[which(output$Species=="TICO")]<-NA
# output$Species[which(output$Species=="ULSP")]<-NA
output$Species[which(output$tree=="RN_13_4_1084")]<-"ULSP"
output<-na.omit(output)
hemi_ranspurk<-output

## Zofin
hemi_results=hemi_zofin
site_name="ZF"
site_meta=read.xlsx("Datasets/Metadata/Zofin_meta.xlsx") 
output<-hemi_results
output$Site<-site_name
output$tree<-gsub("-","_",output$tree)
output<-output[order(output$tree),]
site_meta<-site_meta[which(substr(site_meta$Device_ID,5,20) %in% toupper(substr(output$tree, 7,20))),]
output$Species<-site_meta$Species
hemi_zofin<-output

## Data compilation
hemi_all<-rbind(hemi_boubin, hemi_eustaska, hemi_ranspurk, hemi_zofin)

## EU dataset ####
dataset<-prepare.dataset(calculate.growing.periods(dendrometer_data$EU_PCAB_smallTrees, "Eustaska", "small"), hemi_eustaska)

## Figures making ####
## Panel A 
input<-hemi_all

input$grp<-paste0(input$Site,"_",input$Species)

input$x<-1
input$x[which(input$grp=="RN_ACCA")]<-1
input$x[which(input$grp=="RN_CABE")]<-2
input$x[which(input$grp=="RN_ULSP")]<-3
input$x[which(input$grp=="ZF_PCAB")]<-4
input$x[which(input$grp=="ZF_FASY")]<-5
input$x[which(input$grp=="BB_PCAB")]<-6
input$x[which(input$grp=="BB_FASY")]<-7
input$x[which(input$grp=="EU_PCAB")]<-8

g<-ggplot(input)
g<-g+geom_boxplot(aes(x=x,y=perc_canopy_open,fill=Species,group=grp), outliers=F)
g<-g+scale_x_continuous("",
                        limits=c(0.5,8.5),
                        breaks=c(1:8),
                        labels=c("Ranspurk ACCA", "Ranspurk CABE", "Ranspurk ULSP", "Zofin PCAB", "Zofin FASY", "Boubin PCAB", "Boubin FASY", "Eustaska PCAB"))
g<-g+scale_y_continuous("Tree crown exposure (%)",limits=c(0,1),breaks=seq(0,1,0.25),labels=seq(0,100,25))
g<-g+geom_vline(xintercept = c(3.5, 5.5, 7.5),  linetype = "dotted", color = "#AAAAAA", linewidth = 1)
g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
g<-my.theme(g)
panel_a<-g

## Panel B ####
l <- glm(PercGrPeriods ~ perc_canopy_open, 
         data = dataset, 
         family = quasibinomial(link = "logit"))

null_model <- glm(PercGrPeriods ~ 1, data = dataset, family = quasibinomial)
anv<-print(anova(null_model, l, test = "Chisq"))
anv<-na.omit(anv$`Pr(>Chi)`)
print("----------- Statistical significance -----------")
print(anova(null_model, l, test = "Chisq"))

new_data <- data.frame(perc_canopy_open = seq(min(dataset$perc_canopy_open), 
                                              max(dataset$perc_canopy_open), length.out = 100))
new_data$predicted <- predict(l, newdata = new_data, type = "response")

if(anv<0.05){
  lab<-paste0("R2 = ", round(rsq(l),2),"*")
} else{
  lab<-paste0("R2 = ", round(rsq(l),2))
}

g<-ggplot(dataset)
g<-g+geom_point(aes(x=perc_canopy_open, y=PercGrPeriods, colour=as.factor(Year)))
g<-g+geom_line(data=new_data,mappin=aes(x=perc_canopy_open, y=predicted),colour="#000000")
g<-g+annotate("text",
             x = 0.175,  # x-coordinate for the text
             y = 0.275,  # y-coordinate for the text
             label = lab,
             hjust = 0)
g<-g+scale_colour_manual(breaks=c(2020,2021,2022,2023,2024), values=brewer.pal(n = 5, name = "Dark2"))
g<-g+scale_y_continuous("Number of growing periods (%)",limits=c(0,0.3), breaks=seq(0,1,0.1), labels=seq(0,100,10))
g<-g+scale_x_continuous("Tree crown exposure (%)",limits=c(0.15,0.75), breaks=seq(0,1,0.1), labels=formatC(seq(0,1,0.1),format="f",digits=1))
g<-my.theme(g)
panel_b<-g

## Figure making ####
figure<-ggarrange(panel_a,panel_b,
                  nrow=2,ncol=1, 
                  labels=LETTERS[1:2],
                  align="hv")