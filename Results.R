# -------------------------------- Libraries ---------------------------------- ####
library("tidyverse")
library("lme4")
library("nlme")
library("mgcv")
library("MuMIn")
library("brms")
library("MCMCglmm")
library("geosphere")
library("sjPlot")
library("lmerTest")
library("car")
library("broom.mixed")
library("effectsize")
library("ggeffects")
library("effects")
library("plyr")
library("ggpubr")
library("reshape")
library("runner")
library("suncalc")
library("FSA")
library("dunn.test")
library("dplyr")
library("tidyr")
library("rsq")
library("dplyr")

# ----------------------------------------------------------------------------- ####
# ------------------------------- Data loading -------------------------------- ####
## Data ####

source("Functions_Data_preparation/01_base_functions.R")

## Files serving to data preparation (commented due to time saving)
source("Functions_Data_preparation/02_data_preparation.R")
source("Functions_Data_preparation/02_meteostations_data_prep.R")
# source("Functions_Data_preparation/02_hemiphoto_eval.R")

source("Functions_Data_preparation/03_data_loading.R")

source("Functions_Data_preparation/03_model_datasets_preparation.R")

## These functions are commented because model results are not a part of this example
# source("Functions_Data_preparation/03_model_calculations.R")
# source("Functions_Data_preparation/03_reading_calculated_models.R")

## Cut the data ####
# By uncomplete years
cut.year<-2018
dendrometer_data$BB_FASY_bigTrees<-subset(dendrometer_data$BB_FASY_bigTrees,Year>cut.year)
dendrometer_data$BB_PCAB_bigTrees<-subset(dendrometer_data$BB_PCAB_bigTrees,Year>cut.year)
dendrometer_data$BB_FASY_smallTrees<-subset(dendrometer_data$BB_FASY_smallTrees,Year>cut.year)
dendrometer_data$BB_PCAB_smallTrees<-subset(dendrometer_data$BB_PCAB_smallTrees,Year>cut.year)

cut.year<-2019
dendrometer_data$EU_PCAB_bigTrees<-subset(dendrometer_data$EU_PCAB_bigTrees,Year>cut.year)
dendrometer_data$EU_PCAB_smallTrees<-subset(dendrometer_data$EU_PCAB_smallTrees,Year>cut.year)

cut.year<-2017
dendrometer_data$ZF_FASY_bigTrees<-subset(dendrometer_data$ZF_FASY_bigTrees,Year>cut.year)
dendrometer_data$ZF_PCAB_bigTrees<-subset(dendrometer_data$ZF_PCAB_bigTrees,Year>cut.year)
dendrometer_data$ZF_FASY_smallTrees<-subset(dendrometer_data$ZF_FASY_smallTrees,Year>cut.year)
dendrometer_data$ZF_PCAB_smallTrees<-subset(dendrometer_data$ZF_PCAB_smallTrees,Year>cut.year)

cut.year<-2018
dendrometer_data$RN_ACCA_bigTrees<-subset(dendrometer_data$RN_ACCA_bigTrees,Year>cut.year)
dendrometer_data$RN_ACCA_smallTrees<-subset(dendrometer_data$RN_ACCA_smallTrees,Year>cut.year)
dendrometer_data$RN_CABE_bigTrees<-subset(dendrometer_data$RN_CABE_bigTrees,Year>cut.year)
dendrometer_data$RN_CABE_smallTrees<-subset(dendrometer_data$RN_CABE_smallTrees,Year>cut.year)
dendrometer_data$RN_ULSP_bigTrees<-subset(dendrometer_data$RN_ULSP_bigTrees,Year>cut.year)
dendrometer_data$RN_ULSP_smallTrees<-subset(dendrometer_data$RN_ULSP_smallTrees,Year>cut.year)

## By CI
# CI<-0.90
CI<-0.95

low<-(1-CI)/2
high<-CI+low

dendrometer_data$BB_FASY_bigTrees    <- subset(dendrometer_data$BB_FASY_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$BB_PCAB_bigTrees    <- subset(dendrometer_data$BB_PCAB_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$EU_PCAB_bigTrees    <- subset(dendrometer_data$EU_PCAB_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_FASY_bigTrees    <- subset(dendrometer_data$ZF_FASY_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_PCAB_bigTrees    <- subset(dendrometer_data$ZF_PCAB_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ACCA_bigTrees    <- subset(dendrometer_data$RN_ACCA_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_CABE_bigTrees    <- subset(dendrometer_data$RN_CABE_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ULSP_bigTrees    <- subset(dendrometer_data$RN_ULSP_bigTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)

dendrometer_data$BB_FASY_smallTrees  <- subset(dendrometer_data$BB_FASY_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$BB_PCAB_smallTrees  <- subset(dendrometer_data$BB_PCAB_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$EU_PCAB_smallTrees  <- subset(dendrometer_data$EU_PCAB_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_FASY_smallTrees  <- subset(dendrometer_data$ZF_FASY_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$ZF_PCAB_smallTrees  <- subset(dendrometer_data$ZF_PCAB_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ACCA_smallTrees  <- subset(dendrometer_data$RN_ACCA_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_CABE_smallTrees  <- subset(dendrometer_data$RN_CABE_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)
dendrometer_data$RN_ULSP_smallTrees  <- subset(dendrometer_data$RN_ULSP_smallTrees, Normalized_zero_growth>=low & Normalized_zero_growth<=high)

# by size
DBH_limit <- 250
BB_metadata <- read.xlsx("Datasets/Metadata/Boubin_meta.xlsx")
ZF_metadata <- read.xlsx("Datasets/Metadata/Zofin_meta.xlsx")
RN_metadata <- read.xlsx("Datasets/Metadata/Ranspurk_meta.xlsx")
EU_metadata <- read.xlsx("Datasets/Metadata/Eustaska_meta.xlsx")

dendrometer_data$BB_PCAB_bigTrees[which(dendrometer_data$BB_PCAB_bigTrees$Tree %in% subset(BB_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]

dendrometer_data$BB_FASY_bigTrees <- dendrometer_data$BB_FASY_bigTrees[which(dendrometer_data$BB_FASY_bigTrees$Tree %in% subset(BB_metadata, Species == "FASY" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$BB_PCAB_bigTrees <- dendrometer_data$BB_PCAB_bigTrees[which(dendrometer_data$BB_PCAB_bigTrees$Tree %in% subset(BB_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$EU_PCAB_bigTrees <- dendrometer_data$EU_PCAB_bigTrees[which(dendrometer_data$EU_PCAB_bigTrees$Tree %in% subset(EU_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$ZF_FASY_bigTrees <- dendrometer_data$ZF_FASY_bigTrees[which(dendrometer_data$ZF_FASY_bigTrees$Tree %in% subset(ZF_metadata, Species == "FASY" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$ZF_PCAB_bigTrees <- dendrometer_data$ZF_PCAB_bigTrees[which(dendrometer_data$ZF_PCAB_bigTrees$Tree %in% subset(ZF_metadata, Species == "PCAB" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$RN_ACCA_bigTrees <- dendrometer_data$RN_ACCA_bigTrees[which(dendrometer_data$RN_ACCA_bigTrees$Tree %in% subset(RN_metadata, Species == "ACCA" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$RN_CABE_bigTrees <- dendrometer_data$RN_CABE_bigTrees[which(dendrometer_data$RN_CABE_bigTrees$Tree %in% subset(RN_metadata, Species == "CABE" & DBH >= DBH_limit)$Device_ID),]
dendrometer_data$RN_ULSP_bigTrees <- dendrometer_data$RN_ULSP_bigTrees[which(dendrometer_data$RN_ULSP_bigTrees$Tree %in% subset(RN_metadata, Species == "ULSP" & DBH >= DBH_limit)$Device_ID),]

## Setting Colors for vizualization ####
species.colors<-c("#5C4B51", "#F06060", "#8CBEB2", "#F3B562", "#F2EBBF")
species.colors<-c("#5C4B51", "#F06060", "#8CBEB2", "#F3B562", "#A89F5D")
names(species.colors)<-c("FASY", "PCAB", "ACCA", "CABE", "ULSP")

# cols.phases<-c("#468966","#FFF0A5","#B64926","#AAAAAA")
cols.phases <- c("#3CA45C", "#4C7BBE", "#D65252", "#999999") # navrženo AI
names(cols.phases)<-c("Growth","Refiling","TWD","NoStatus")

cols.dayphase<-c("#333333","#F5CE41","#80BDF2","#D93E30")
names(cols.dayphase)<-c("Night","Sunrise","Day","Sunset")

boxcols <- c("#000000", "#999999")
names(boxcols) <- c("big", "small")

# ----------------------------------------------------------------------------- ####
# ------------------------- Figures - main story line ------------------------- ####
## Table 1 ####
# Boubin FASY
big<-dendrometer_data$BB_FASY_bigTrees
small<-dendrometer_data$BB_FASY_smallTrees

print("Years")
unique(dendrometer_data$BB_FASY_smallTrees$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
            aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

# Boubin PCAB
big<-dendrometer_data$BB_PCAB_bigTrees
small<-dendrometer_data$BB_PCAB_smallTrees

print("Years")
unique(small$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

# Eustaska PCAB
big<-dendrometer_data$EU_PCAB_bigTrees
small<-dendrometer_data$EU_PCAB_smallTrees

print("Years")
unique(small$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

# Ranspurk CABE
big<-dendrometer_data$RN_CABE_bigTrees
small<-dendrometer_data$RN_CABE_smallTrees

print("Years")
unique(small$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

# Ranspurk ACCA
big<-dendrometer_data$RN_ACCA_bigTrees
small<-dendrometer_data$RN_ACCA_smallTrees

print("Years")
unique(small$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

# Ranspurk ULSP
big<-dendrometer_data$RN_ULSP_bigTrees
small<-dendrometer_data$RN_ULSP_smallTrees

print("Years")
unique(small$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

# Zofin FASY
big<-dendrometer_data$ZF_FASY_bigTrees
small<-dendrometer_data$ZF_FASY_smallTrees

print("Years")
unique(dendrometer_data$ZF_FASY_smallTrees$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

# Zofin PCAB
big<-dendrometer_data$ZF_PCAB_bigTrees
small<-dendrometer_data$ZF_PCAB_smallTrees

print("Years")
unique(small$Year)

print("No. trees")
length(unique(big$Tree))
length(unique(small$Tree))

print("DBH")
dbh<-aggregate(DBH_value~Tree,data=big,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

dbh<-aggregate(DBH_value~Tree,data=small,FUN=max)$DBH_value
round(mean(dbh),0) ; round(sd(dbh),0)

print("Increment")
increment<-aggregate(DBH_value~Tree+Year,data=big,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=big,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

increment<-aggregate(DBH_value~Tree+Year,data=small,FUN=max)$DBH_value-
  aggregate(DBH_value~Tree+Year,data=small,FUN=min)$DBH_value
round(mean(increment),1) ; round(sd(increment),1)

## Figure 1 - Methodological figure  ####
source("Functions_figure_making_and_data_analysis/Figure_01.R")

w=12 ; h=18
# ggsave("Outputs/Figure_1.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Figure_1.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Figure 2 - growing season lengths ####
# source("Functions_figure_making_and_data_analysis/Figure_02A.R")
source("Functions_figure_making_and_data_analysis/Figure_02A_revised.R")
figure_a<-figure

source("Functions_figure_making_and_data_analysis/Figure_02BC.R")
figure_bc<-figure

# source("Functions_figure_making_and_data_analysis/Figure_02D.R")
source("Functions_figure_making_and_data_analysis/Figure_02D_revised.R")
figure_d<-figure

figure<-ggarrange(figure_a, figure_bc,figure_d,nrow=3,ncol=1,align="hv",common.legend=T,legend="bottom",heights=c(0.4,0.3,0.3),labels=c("A","","D"))

w=20 ; h=30
ggsave("Outputs/Figure_2.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Figure_2.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Figure 3 - daily timing of phases ####
# source("Functions_figure_making_and_data_analysis/Figure_03A.R")
source("Functions_figure_making_and_data_analysis/Figure_03A_revised.R")
figure_a<-figure

source("Functions_figure_making_and_data_analysis/Figure_03C.R")
figure_c<-figure

# source("Functions_figure_making_and_data_analysis/Figure_03B.R")
source("Functions_figure_making_and_data_analysis/Figure_03B_revised.R")
figure_b<-figure

source("Functions_figure_making_and_data_analysis/Figure_03D.R") # divna statistika, ale asi neni potreba, hlaska je v clanku jen, ze se to shoduje
figure_d<-figure

figure<-ggarrange(figure_a, figure_b, figure_c, figure_d,nrow=2,ncol=2,align="hv",common.legend=T,legend="bottom",labels=LETTERS[1:4])

w=28 ; h=20
ggsave("Outputs/Figure_3.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Figure_3.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Figure 4 - Number of GRO periods within a day ####
# source("Functions_figure_making_and_data_analysis/Figure_04.R")
source("Functions_figure_making_and_data_analysis/Figure_04_revised.R")

w=22 ; h=28
ggsave("Outputs/Figure_4.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Figure_4.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Figure 5 - growth progress vs growing hours ####
source("Functions_figure_making_and_data_analysis/Figure_05.R")

w=20 ; h=28
ggsave("Outputs/Figure_5.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Figure_5.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Figure 6 - climate model ####
# models<-models_base

# source("Functions_figure_making_and_data_analysis/Figure_06.R")
# 
# w=36 ; h=22
# ggsave("Outputs/Figure_6.png", figure, width=w, height=h, units="cm", dpi=600)
# ggsave("Outputs/Figure_6.pdf", figure, width = w, height = h, units="cm", dpi=600)
# 
# write.xlsx(ouptut_autocorrel, "Outputs/models_autocorrelations.xlsx")

## Testing overfittingu GMLLs ####
# source("03_testing_calculated_models.R")

# model_overfitting <- read.table("Outputs/Cross_validation_result.xlsx")
# model_overfitting
# 
# model_autocorrelations <- read.table("Outputs/models_autocorrelations.xlsx")
# model_autocorrelations

# ----------------------------------------------------------------------------- ####
# --------------------- Figures - supplementary material ---------------------- ####
## Supplementary Figure 1 - sensitivity analysis ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_01.R")

w=22 ; h=16
ggsave("Outputs/Supplementary_Figure_01.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_01.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 2 - growth timing ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_02.R")

w=28 ; h=14
ggsave("Outputs/Supplementary_Figure_02.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_02.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 3 - Growth percentile realization ####
# source("Functions_figure_making_and_data_analysis/SupplementaryFigure_03.R")
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_03_revised.R")

w=20 ; h=28
ggsave("Outputs/Supplementary_Figure_03.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_03.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 4 - periods ratios ####
# source("Functions_figure_making_and_data_analysis/SupplementaryFigure_04.R")
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_04_revised.R")

w=20 ; h=40
ggsave("Outputs/Supplementary_Figure_04.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_04.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 5 - Increments/schrinkage intensity ####
# source("Functions_figure_making_and_data_analysis/SupplementaryFigure_05.R")
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_05_revised.R")

w=40 ; h=20
ggsave("Outputs/Supplementary_Figure_05.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_05.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 6 - differences in hours distribution ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_06.R")

w=18 ; h=18
ggsave("Outputs/Supplementary_Figure_06.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_06.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 7 - Number of Growth intensity ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_07.R")

w=20 ; h=28
ggsave("Outputs/Supplementary_Figure_07.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_07.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 8 - growth intensity over time ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_08.R")

w=20 ; h=28
ggsave("Outputs/Supplementary_Figure_08.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_08.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 9 - Models with only Forest data  ####
# models<-models_understory
# 
# source("Functions_figure_making_and_data_analysis/SupplementaryFigure_09.R")
# 
# w=36 ; h=22
# ggsave("Outputs/Supplementary_Figure_09.png", figure, width=w, height=h, units="cm", dpi=600)
# ggsave("Outputs/Supplementary_Figure_09.pdf", figure, width = w, height = h, units="cm", dpi=600)
# 
# write.xlsx(ouptut_autocorrel, "Outputs/models_autocorrelations_supplementary.xlsx")

## Supplementary Figure 10 - canoppy openess  ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_10.R")

w=20 ; h=20
ggsave("Outputs/Supplementary_Figure_10.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_10.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 11 - climate comparison - podrost vs canopy ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_11.R")

w=32 ; h=27
ggsave("Outputs/Supplementary_Figure_11.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_11.pdf", figure, width = w, height = h, units="cm", dpi=600)


## Supplementary Figure 12 - annual increments ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_12.R")

w=20 ; h=28
ggsave("Outputs/Supplementary_Figure_12-annual_increments.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_12-annual_increments.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Figure 13 - bootstrapped growth curves ####
source("Functions_figure_making_and_data_analysis/SupplementaryFigure_13.R")

w=24 ; h=42
ggsave("Outputs/Supplementary_Figure_13.png", figure, width=w, height=h, units="cm", dpi=600)
ggsave("Outputs/Supplementary_Figure_13.pdf", figure, width = w, height = h, units="cm", dpi=600)

## Supplementary Table 1 - Model estimates ####
# source("Functions_figure_making_and_data_analysis/SupplementaryTable_01.R")
# 
# write.xlsx(estimates.table, "Outputs/SupplementaryTable_01a.xlsx")
# write.xlsx(pval.table, "Outputs/SupplementaryTable_01b.xlsx")

# ----------------------------------------------------------------------------- ####
# ----------------------------------------------------------------------------- ####
