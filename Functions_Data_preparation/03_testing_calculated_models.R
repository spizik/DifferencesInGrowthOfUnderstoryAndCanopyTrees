library("boot")
library("performance")
library("lme4")
library("dplyr")

## ----------------------------------- Funkce ---------------------------------- ####
## cross.validation ####
# Perform k-fold cross-validation of a binomial mixed-effects model
# predicting growth phase occurrence from climatic and phenological variables.
#
# Arguments:
#   df - data.frame containing the model dataset with a binary response variable
#        (phase2), predictor variables, and a Tree identifier
#   k  - number of cross-validation folds (default = 10)
#
# Output is umeric value representing mean log-loss across k cross-validation folds
#
cross.validation <- function(df, k = 10){
  ## inputs
  # df = model_dataset$EU_PCAB_bigTrees
  # k = 10
  
  ## Počet foldů
  set.seed(123)
  folds <- sample(rep(1:k, length.out = nrow(df)))
  
  ## Prázdné pole pro výsledky
  logloss <- numeric(k)
  
  ## Funkce pro výpočet log-loss (binární)
  log_loss <- function(y, p) {
    -mean(y * log(p + 1e-15) + (1 - y) * log(1 - p + 1e-15))
  }
  
  for (i in 1:k) {
    
    print(i)
    
    train_data <- df[folds != i, ]
    test_data  <- df[folds == i, ]
    
    ## Fit modelu
    m <- glmer(phase2 ~ scale(T) + scale(VPD) + scale(R) + scale(P) + scale(daylength) +
                 scale(SWC) + DayPhase + scale(VPD):scale(SWC) + scale(SWC):scale(daylength) +
                 scale(VPD):scale(daylength) + scale(R):scale(daylength) +
                 (1 | Tree),
               data = train_data, family = binomial(link = "logit"))
    
    ## Predikce
    pred_probs <- predict(m, newdata = test_data, type = "response", allow.new.levels = TRUE)
    
    ## Log-loss
    logloss[i] <- log_loss(test_data$phase2, pred_probs)
  }
  
  return(mean(logloss))  # průměrná predikční chyba (nižší = lepší)
}

## ------------------------------ CrossValidation ------------------------------ ####
cross_validation_result <- data.frame(Site = c(rep("Ranspurk", 6),
                                               rep("Zofin", 4),
                                               rep("Boubin", 4),
                                               rep("Eustaska", 2)),
                                      Species = c(rep("ACCA", 2),
                                                  rep("CABE", 2),
                                                  rep("ULSP", 2),
                                                  rep("PCAB", 2),
                                                  rep("FASY", 2),
                                                  rep("PCAB", 2),
                                                  rep("FASY", 2),
                                                  rep("PCAB", 2)),
                                      Size = rep(c("big", "small"), 8),
                                      CrossVal = c(cross.validation(model_dataset$RN_ACCA_bigTrees),
                                                   cross.validation(model_dataset$RN_ACCA_smallTrees),
                                                   cross.validation(model_dataset$RN_CABE_bigTrees),
                                                   NA, # cross.validation(model_dataset$RN_CABE_smallTrees),
                                                   cross.validation(model_dataset$RN_ULSP_bigTrees),
                                                   NA, # cross.validation(model_dataset$RN_ULSP_smallTrees),
                                                   
                                                   cross.validation(model_dataset$ZF_PCAB_bigTrees),
                                                   cross.validation(model_dataset$ZF_PCAB_smallTrees),
                                                   cross.validation(model_dataset$ZF_FASY_bigTrees),
                                                   cross.validation(model_dataset$ZF_FASY_smallTrees),
                                                   
                                                   cross.validation(model_dataset$BB_PCAB_bigTrees),
                                                   cross.validation(model_dataset$BB_PCAB_smallTrees),
                                                   cross.validation(model_dataset$BB_FASY_bigTrees),
                                                   cross.validation(model_dataset$BB_FASY_smallTrees),
                                                   
                                                   cross.validation(model_dataset$EU_PCAB_bigTrees),
                                                   cross.validation(model_dataset$EU_PCAB_smallTrees)))


write.xlsx(cross_validation_result, "Outputs/Cross_validation_result.xlsx")
