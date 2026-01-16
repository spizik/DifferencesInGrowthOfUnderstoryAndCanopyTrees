## Functions ####

## extract.estimates ####
# Extracts selected fixed-effect coefficients from a fitted model.
# Returns estimates and p-values (rounded) for predefined predictors,
# ordered consistently for reporting or plotting.
extract.estimates <- function(input){
  # input=models_base$model.rn.acca.big
  
  var_names <- c("(Intercept)", "scale(T)", "scale(VPD)", "scale(R)", "scale(P)", "scale(SWC)", "scale(daylength)", 
                 "scale(VPD):scale(SWC)", "scale(daylength):scale(SWC)", "scale(VPD):scale(daylength)",  "scale(R):scale(daylength)", 
                 "DayPhaseNight", "DayPhaseSunrise", "DayPhaseSunset")
  
  # var_names_2 <- c("Intercept", "Temperature", "VPD", "Solar radiation", "Precipitation", "SWP", "Day length",  
  #                  "VPD : SWP", "Day Length : SWP", "VPD : Day length",  "Solar radiation : Day length", 
  #                  "Night", "Sunrise", "Sunset")
  
  mod_summary <- as.data.frame(summary(input)$coefficients)
  mod_summary$effects <- rownames(mod_summary)
  mod_summary <- mod_summary[order(match(mod_summary$effects, var_names)), ]
  
  output <- data.frame(variable = var_names,
                       estimate = round(mod_summary[,1],2),
                       pval = round(mod_summary[,4],2))
  
  return(output)
}


## Data calculations ####
estimates.table<-data.frame(FixedEffect=c("Intercept", "Temperature", "VPD", "Solar radiation", "Precipitation", "SWP", "Day length",  
                                          "VPD : SWP", "Day Length : SWP", "VPD : Day length",  "Solar radiation : Day length", 
                                          "Night", "Sunrise", "Sunset"),
                            
                            RN_Maple_Canopy = extract.estimates(models_base$model.rn.acca.big)$estimate,
                            RN_Maple_Understory = extract.estimates(models_base$model.rn.acca.small)$estimate,
                            RN_Maple_Understory_2 = extract.estimates(models_podrost$model.rn.acca.small)$estimate,
                            
                            RN_Hornbeam_Canopy = extract.estimates(models_base$model.rn.cabe.big)$estimate,
                            RN_Hornbeam_Understory = extract.estimates(models_base$model.rn.cabe.small)$estimate,
                            RN_Hornbeam_Understory_2 = extract.estimates(models_podrost$model.rn.cabe.small)$estimate,
                            
                            RN_Elm_Canopy = extract.estimates(models_base$model.rn.ulsp.big)$estimate,
                            RN_Elm_Understory = extract.estimates(models_base$model.rn.ulsp.small)$estimate,
                            RN_Elm_Understory_2 = extract.estimates(models_podrost$model.rn.ulsp.small)$estimate,
                            
                            ZF_Spruce_Canopy = extract.estimates(models_base$model.zf.pcab.big)$estimate,
                            ZF_Spruce_Understory = extract.estimates(models_base$model.zf.pcab.small)$estimate,
                            ZF_Spruce_Understory_2 = extract.estimates(models_podrost$model.zf.pcab.small)$estimate,
                            
                            ZF_Beech_Canopy = extract.estimates(models_base$model.zf.fasy.big)$estimate,
                            ZF_Beech_Understory = extract.estimates(models_base$model.zf.fasy.small)$estimate,
                            ZF_Beech_Understory_2 = extract.estimates(models_podrost$model.zf.fasy.small)$estimate,
                            
                            BB_Spruce_Canopy = extract.estimates(models_base$model.bb.pcab.big)$estimate,
                            BB_Spruce_Understory = extract.estimates(models_base$model.bb.pcab.small)$estimate,
                            BB_Spruce_Understory_2 = extract.estimates(models_podrost$model.bb.pcab.small)$estimate,
                            
                            BB_Beech_Canopy = extract.estimates(models_base$model.bb.fasy.big)$estimate,
                            BB_Beech_Understory = extract.estimates(models_base$model.bb.fasy.small)$estimate,
                            BB_Beech_Understory_2 = extract.estimates(models_podrost$model.bb.fasy.small)$estimate,
                            
                            EU_Spruce_Canopy = extract.estimates(models_base$model.eu.pcab.big)$estimate,
                            EU_Spruce_Understory = extract.estimates(models_base$model.eu.pcab.small)$estimate,
                            EU_Spruce_Understory_2 = extract.estimates(models_podrost$model.eu.pcab.small)$estimate)


pval.table<-data.frame(FixedEffect=c("Intercept", "Temperature", "VPD", "Solar radiation", "Precipitation", "SWP", "Day length",  
                                     "VPD : SWP", "Day Length : SWP", "VPD : Day length",  "Solar radiation : Day length", 
                                     "Night", "Sunrise", "Sunset"),
                       
                       RN_Maple_Canopy = extract.estimates(models_base$model.rn.acca.big)$pval,
                       RN_Maple_Understory = extract.estimates(models_base$model.rn.acca.small)$pval,
                       RN_Maple_Understory_2 = extract.estimates(models_podrost$model.rn.acca.small)$pval,
                       
                       RN_Hornbeam_Canopy = extract.estimates(models_base$model.rn.cabe.big)$pval,
                       RN_Hornbeam_Understory = extract.estimates(models_base$model.rn.cabe.small)$pval,
                       RN_Hornbeam_Understory_2 = extract.estimates(models_podrost$model.rn.cabe.small)$pval,
                       
                       RN_Elm_Canopy = extract.estimates(models_base$model.rn.ulsp.big)$pval,
                       RN_Elm_Understory = extract.estimates(models_base$model.rn.ulsp.small)$pval,
                       RN_Elm_Understory_2 = extract.estimates(models_podrost$model.rn.ulsp.small)$pval,
                       
                       ZF_Spruce_Canopy = extract.estimates(models_base$model.zf.pcab.big)$pval,
                       ZF_Spruce_Understory = extract.estimates(models_base$model.zf.pcab.small)$pval,
                       ZF_Spruce_Understory_2 = extract.estimates(models_podrost$model.zf.pcab.small)$pval,
                       
                       ZF_Beech_Canopy = extract.estimates(models_base$model.zf.fasy.big)$pval,
                       ZF_Beech_Understory = extract.estimates(models_base$model.zf.fasy.small)$pval,
                       ZF_Beech_Understory_2 = extract.estimates(models_podrost$model.zf.fasy.small)$pval,
                       
                       BB_Spruce_Canopy = extract.estimates(models_base$model.bb.pcab.big)$pval,
                       BB_Spruce_Understory = extract.estimates(models_base$model.bb.pcab.small)$pval,
                       BB_Spruce_Understory_2 = extract.estimates(models_podrost$model.bb.pcab.small)$pval,
                       
                       BB_Beech_Canopy = extract.estimates(models_base$model.bb.fasy.big)$pval,
                       BB_Beech_Understory = extract.estimates(models_base$model.bb.fasy.small)$pval,
                       BB_Beech_Understory_2 = extract.estimates(models_podrost$model.bb.fasy.small)$pval,
                       
                       EU_Spruce_Canopy = extract.estimates(models_base$model.eu.pcab.big)$pval,
                       EU_Spruce_Understory = extract.estimates(models_base$model.eu.pcab.small)$pval,
                       EU_Spruce_Understory_2 = extract.estimates(models_podrost$model.eu.pcab.small)$pval)

## Printing results ####
print("------------------------------------------------------------------------")
print("Models estimates")
print("------------------------------------------------------------------------")
print(estimates.table)

print("------------------------------------------------------------------------")
print("Models pvals")
print("------------------------------------------------------------------------")
print(pval.table)
