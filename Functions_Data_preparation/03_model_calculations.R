#-------------------------------------------------------------------------------
## Loading and initializating libraries ####
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
## ---------------------------------------------------------------------------- ####
## ------------------------------ Data loading -------------------------------- ####
## Data loading ####

source("Functions_Data_preparation/03_model_datasets_preparation.R")

## ---------------------------------------------------------------------------- ####
## ---------------------------- Formulas setting ------------------------------ ####
## Setting basic main formula ####
form.1<-formula(phase2 ~ 
                scale(T) + 
                scale(VPD) + 
                scale(R) + 
                scale(P) + 
                scale(daylength) + 
                scale(SWC) + 
                DayPhase + 
                  
                scale(VPD):scale(SWC)+
                scale(SWC):scale(daylength)+
                scale(VPD):scale(daylength)+
                scale(R):scale(daylength)+
                  
                (1|Tree))

form.2<-formula(phase2 ~ 
                  scale(T) + 
                  scale(VPD) + 
                  scale(R) + 
                  scale(P) + 
                  scale(daylength) + 
                  scale(SWC) + 
                  DayPhase + 
                  
                  scale(VPD):scale(SWC)+
                  scale(SWC):scale(daylength)+
                  scale(VPD):scale(daylength)+
                  scale(R):scale(daylength))

## Setting formula for understory ####
form.1.forest<-formula(phase2 ~ 
                         scale(T_forest) + 
                         scale(VPD_forest) + 
                         scale(R) + 
                         scale(P) + 
                         scale(daylength) + 
                         scale(SWC) + 
                         DayPhase + 
                         
                         scale(VPD_forest):scale(SWC)+
                         scale(SWC):scale(daylength)+
                         scale(VPD_forest):scale(daylength)+
                         scale(R):scale(daylength)+
                         (1|Tree))

form.2.forest<-formula(phase2 ~ 
                         scale(T_forest) + 
                         scale(VPD_forest) + 
                         scale(R) + 
                         scale(P) + 
                         scale(daylength) + 
                         scale(SWC) + 
                         DayPhase + 
                         
                         scale(VPD_forest):scale(SWC)+
                         scale(SWC):scale(daylength)+
                         scale(VPD_forest):scale(daylength)+
                         scale(R):scale(daylength))

## ---------------------------------------------------------------------------- ####
## --------------------------- Model calculations ----------------------------- ####
## Basic models ####
model.eu.pcab.big <- glmer(form.1, data = model_dataset$EU_PCAB_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.bb.pcab.big <- glmer(form.1, data = model_dataset$BB_PCAB_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.bb.fasy.big <- glmer(form.1, data = model_dataset$BB_FASY_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.zf.pcab.big <- glmer(form.1, data = model_dataset$ZF_PCAB_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.zf.fasy.big <- glmer(form.1, data = model_dataset$ZF_FASY_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.rn.acca.big <- glmer(form.1, data = model_dataset$RN_ACCA_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.rn.cabe.big <- glmer(form.1, data = model_dataset$RN_CABE_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.rn.ulsp.big <- glmer(form.1, data = model_dataset$RN_ULSP_bigTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))

model.eu.pcab.small <- glmer(form.1, data = model_dataset$EU_PCAB_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.bb.pcab.small <- glmer(form.1, data = model_dataset$BB_PCAB_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.bb.fasy.small <- glmer(form.1, data = model_dataset$BB_FASY_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.zf.pcab.small <- glmer(form.1, data = model_dataset$ZF_PCAB_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.zf.fasy.small <- glmer(form.1, data = model_dataset$ZF_FASY_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.rn.acca.small <- glmer(form.1, data = model_dataset$RN_ACCA_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.rn.cabe.small <- glm(form.2, data = model_dataset$RN_CABE_smallTrees, family = binomial(link = "logit"))
model.rn.ulsp.small <- glm(form.2, data = model_dataset$RN_ULSP_smallTrees, family = binomial(link = "logit"))

## Podrost models ####
model.eu.pcab.small.podrost <- glmer(form.1.forest, data = model_dataset_podrost$EU_PCAB_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.bb.pcab.small.podrost <- glmer(form.1.forest, data = model_dataset_podrost$BB_PCAB_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.bb.fasy.small.podrost <- glmer(form.1.forest, data = model_dataset_podrost$BB_FASY_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.zf.pcab.small.podrost <- glmer(form.1.forest, data = model_dataset_podrost$ZF_PCAB_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.zf.fasy.small.podrost <- glmer(form.1.forest, data = model_dataset_podrost$ZF_FASY_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.rn.acca.small.podrost <- glmer(form.1.forest, data = model_dataset_podrost$RN_ACCA_smallTrees, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))
model.rn.cabe.small.podrost <- glm(form.2.forest, data = model_dataset_podrost$RN_CABE_smallTrees, family = binomial(link = "logit"))
model.rn.ulsp.small.podrost <- glm(form.2.forest, data = model_dataset_podrost$RN_ULSP_smallTrees, family = binomial(link = "logit"))

## ---------------------------------------------------------------------------- ####
## ------------------------------ Model saving -------------------------------- ####
## Basic models ####
write_rds(model.eu.pcab.big,"calculated_models/base_models/eu_pcab_big.rds")
write_rds(model.bb.pcab.big,"calculated_models/base_models/bb_pcab_big.rds")
write_rds(model.bb.fasy.big,"calculated_models/base_models/bb_fasy_big.rds")
write_rds(model.zf.pcab.big,"calculated_models/base_models/zf_pcab_big.rds")
write_rds(model.zf.fasy.big,"calculated_models/base_models/zf_fasy_big.rds")
write_rds(model.rn.acca.big,"calculated_models/base_models/rn_acca_big.rds")
write_rds(model.rn.cabe.big,"calculated_models/base_models/rn_cabe_big.rds")
write_rds(model.rn.ulsp.big,"calculated_models/base_models/rn_ulsp_big.rds")

write_rds(model.eu.pcab.small,"calculated_models/base_models/eu_pcab_small.rds")
write_rds(model.bb.pcab.small,"calculated_models/base_models/bb_pcab_small.rds")
write_rds(model.bb.fasy.small,"calculated_models/base_models/bb_fasy_small.rds")
write_rds(model.zf.pcab.small,"calculated_models/base_models/zf_pcab_small.rds")
write_rds(model.zf.fasy.small,"calculated_models/base_models/zf_fasy_small.rds")
write_rds(model.rn.acca.small,"calculated_models/base_models/rn_acca_small.rds")
write_rds(model.rn.cabe.small,"calculated_models/base_models/rn_cabe_small.rds")
write_rds(model.rn.ulsp.small,"calculated_models/base_models/rn_ulsp_small.rds")

## Understory models ####
write_rds(model.eu.pcab.small.podrost,"calculated_models/understory_models/eu_pcab_small_podrost.rds")
write_rds(model.bb.pcab.small.podrost,"calculated_models/understory_models/bb_pcab_small_podrost.rds")
write_rds(model.bb.fasy.small.podrost,"calculated_models/understory_models/bb_fasy_small_podrost.rds")
write_rds(model.zf.pcab.small.podrost,"calculated_models/understory_models/zf_pcab_small_podrost.rds")
write_rds(model.zf.fasy.small.podrost,"calculated_models/understory_models/zf_fasy_small_podrost.rds")
write_rds(model.rn.acca.small.podrost,"calculated_models/understory_models/rn_acca_small_podrost.rds")
write_rds(model.rn.cabe.small.podrost,"calculated_models/understory_models/rn_cabe_small_podrost.rds")
write_rds(model.rn.ulsp.small.podrost,"calculated_models/understory_models/rn_ulsp_small_podrost.rds")

