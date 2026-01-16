## Functions ####

## show.results ####
# Print basic diagnostics for a fitted model (R², VIF, ANOVA).
show.results<-function(mod, name){
  
  ## testing arguments
  # mod=models$model.zf.pcab.big
  # name="ACCA"
  
  print("-----------------------------------------------------------------------")
  print("-----------------------------------------------------------------------")
  print(name)
  print("-----------------------------------------------------------------------")
  print("RSQR")
  print(r.squaredGLMM(mod))
  print("-----------------------------------------------------------------------")
  print("VIF")
  print(vif(mod))
  print("-----------------------------------------------------------------------")
  print("ANOVA")
  print(anova(mod))
}

## my.theme ####
# Apply a classic ggplot2 theme with black axes and configurable legend position.
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
# Assign numeric x positions based on site, species, and size combinations.
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

## extract.rsqr.glm ####
# Extract marginal R² values from a GLM model.
extract.rsqr.glm<-function(input.model){
  
  ## testing arguments
  # input.model=models$model.rn.cabe.small

  
  rsqr.data<-as.data.frame(rsq(input.model))
  
  output<-data.frame(theoretical.R2m=NA, theoretical.R2c=NA,
                     delta.R2m=NA, delta.R2c=NA)
  
  output[,c("theoretical.R2m","theoretical.R2c")]<-c(as.numeric(rsqr.data[1,]),0)
  output[,c("delta.R2m","delta.R2c")]<-c(as.numeric(rsqr.data[1,]),0)
  
  return(output)
}

## extract.rsqr ####
# Extract marginal and conditional R² values from a mixed-effects model.
extract.rsqr<-function(input.model){
  
  ## testing arguments
  # input.model=models$model.eu.pcab.big

  rsqr.data<-as.data.frame(r.squaredGLMM(input.model))
  
  output<-data.frame(theoretical.R2m=NA, theoretical.R2c=NA,
                     delta.R2m=NA, delta.R2c=NA)
  
  output[,c("theoretical.R2m","theoretical.R2c")]<-as.numeric(rsqr.data[1,])
  output[,c("delta.R2m","delta.R2c")]<-as.numeric(rsqr.data[2,])
  
  return(output)
}

## plot.model.rsqr ####
# Visualize fixed, random, and total explained variance (R²) for big vs. small tree models.
plot.model.rsqr<-function(input.big, input.small, sp){
  
  ## testing arguments
  # input.big=models$model.rn.acca.big
  # input.small=models$model.rn.acca.small
  # sp="ACCA"
  
  temp<-extract.rsqr(input.big)
  
  show.data.big<-temp[,c("theoretical.R2m", "theoretical.R2c")]
  show.data.big$random<-show.data.big$theoretical.R2c-show.data.big$theoretical.R2m
  names(show.data.big)<-c("fixed", "all", "random")
  show.data.big$Size<-"big"
  show.data.big$x<-1
  
  if(sp == "CABE" || sp=="ULSP"){
    temp<-extract.rsqr.glm(input.small)
  }else{
    temp<-extract.rsqr(input.small)
  }
  show.data.small<-temp[,c("theoretical.R2m", "theoretical.R2c")]
  show.data.small$random<-show.data.small$theoretical.R2c-show.data.small$theoretical.R2m
  names(show.data.small)<-c("fixed", "all", "random")
  show.data.small$Size<-"small"
  show.data.small$x<-2

  show.data <- rbind(show.data.big, show.data.small)
  show.data$x <- rev(show.data$x)
  
  g<-ggplot(show.data)
  g<-g+geom_bar(aes(x=x,y=all,colour=sp), fill=NA,stat="identity",linewidth=0.75)
  g<-g+geom_bar(aes(x=x,y=fixed,alpha=Size,fill=sp,colour=sp),stat="identity",linewidth=0.75)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_fill_manual(breaks=names(species.colors), values=species.colors)
  g<-g+scale_colour_manual(breaks=names(species.colors), values=species.colors)
  g<-g+scale_x_continuous("", limits=c(0.5,2.5),breaks=c(1.5),
                          labels=c("Desc"))
  g<-g+scale_y_continuous("R2", limits=c(0,1),breaks=seq(0,1,0.2),labels=formatC(seq(0,1,0.2),format="f",digits=1))
  g<-g+coord_flip()
  g<-my.theme(g)
  g
}

## plot.estimates ####
# Plot standardized model coefficients with confidence ranges for big and small trees.
plot.estimates<-function(input.big, input.small, sp){
  
  ## testing arguments
  # input.big=models$model.eu.pcab.big
  # input.small=models$model.eu.pcab.small
  # sp="PCAB"
  
  temp<-extract.rsqr(input.big)
  show.data.big<-as.data.frame(summary(input.big)[["coefficients"]])
  show.data.big<-show.data.big[c(2:nrow(show.data.big)), c(1,2,4)]
  names(show.data.big)<-c("est", "std", "pval")
  show.data.big$variable<-rownames(show.data.big)
  temp<-rep("significant",nrow(show.data.big))
  temp[which(show.data.big$p.value>0.05)]<-"non-significant"
  show.data.big$grp<-"big"
  
  
  show.data.small<-as.data.frame(summary(input.small)[["coefficients"]])
  show.data.small<-show.data.small[c(2:nrow(show.data.small)), c(1,2,4)]
  names(show.data.small)<-c("est", "std", "pval")
  show.data.small$variable<-rownames(show.data.small)
  show.data.small$variable<-gsub("_forest","",show.data.small$variable)
  show.data.small$variable<-gsub("_forest_sim","",show.data.small$variable)
  temp<-rep("significant",nrow(show.data.small))
  temp[which(show.data.small$p.value>0.05)]<-"non-significant"
  show.data.small$grp<-"small"
  
  to_delete <- which(show.data.big$variable %in% c("daylength","DayPhaseNight","DayPhaseSunrise","DayPhaseSunset"))
  show.data.big <- show.data.big[-to_delete,]
  to_delete <- which(show.data.small$variable %in% c("daylength","DayPhaseNight","DayPhaseSunrise","DayPhaseSunset"))
  show.data.small <- show.data.small[-to_delete,]
  
  
  show.data.big$x<-c(nrow(show.data.big):1)+0.25
  show.data.small$x<-c(nrow(show.data.small):1)-0.25
  
  show.data<-rbind(show.data.big, show.data.small)
  show.data$variable<-gsub("scale\\(|\\)", "", show.data$variable)
  
  var.names<-c("T","VPD","R","P","DayLength","SWC","VPD:SWC","DayLength:SWC","VPD:DayLength","R:DayLength")
  var.names<-rev(var.names)
  
  labs<-show.data[,c("est","pval", "x", "grp")]
  labs$l<-"*"
  labs$y<-5.5
  labs$y[which(labs$est<0)]<-labs$y[which(labs$est<0)]*(-1)
  labs<-subset(labs,pval<0.05)
  
  g<-ggplot(show.data)
  g<-g+geom_vline(xintercept=c(1:10)-0.5, linetype="dotted",linewidth=0.5)
  g<-g+geom_bar(aes(x=x, y=est, alpha=grp, colour = grp), fill=species.colors[sp],stat="identity", linewidth=0.75)
  g<-g+geom_linerange(aes(x=x,ymin=est-std,ymax=est+std, alpha=grp), colour=species.colors[sp],linewidth=1.25)
  g<-g+geom_text(data=labs,mapping=aes(x=x, y=y, label=l, alpha=grp), colour=species.colors[sp], inherit.aes=F, size=8)
  g<-g+scale_colour_manual(breaks=names(boxcols), values=boxcols)
  g<-g+scale_alpha_manual(breaks=c("big","small"), values=c(0.75,0.25))
  g<-g+scale_y_continuous("Estimate",limits=c(-6,3.5), breaks=seq(-10,10,2))
  g<-g+scale_x_continuous("", limits=c(0.50,(length(var.names)+0.5)), breaks=c(1:length(var.names)), labels=var.names)
  g<-g+geom_hline(yintercept=0, colour="#000000", linewidth=0.5)
  g<-g+coord_flip()
  g<-my.theme(g, "none")
  g
}

## return.R.estimate ####
# Extract radiation (R) coefficient estimates for big and small tree models.
return.R.estimate<-function(input.big, input.small){
  
  ## testing arguments
  # input.big=models$model.rn.acca.small
  # input.small=models$model.rn.acca.small
  
  input.big<-as.data.frame(summary(input.big)[["coefficients"]])
  input.small<-as.data.frame(summary(input.small)[["coefficients"]])
  
  input.big<-input.big[which(rownames(input.big)=="scale(R)"),"Estimate"]
  input.small<-input.small[which(rownames(input.small)=="scale(R)"),"Estimate"]
  
  output<-c(input.big, input.small)
  names(output)<-c("big","small")
  
  return(output)
}

## return.dayphase.estimate ####
# Extract day-phase coefficient estimates for big and small tree models.
return.dayphase.estimate<-function(input.big, input.small){
  
  ## testing arguments
  # input.big=models$model.rn.acca.big
  # input.small=models$model.rn.acca.small
  
  input.big<-as.data.frame(summary(input.big)[["coefficients"]])
  input.small<-as.data.frame(summary(input.small)[["coefficients"]])
  
  dayphases<-c("(Intercept)", "DayPhaseNight", "DayPhaseSunrise", "DayPhaseSunset")
  
  input.big<-input.big[which(rownames(input.big) %in% dayphases),"Estimate"]
  input.small<-input.small[which(rownames(input.small) %in% dayphases),"Estimate"]
  
  names(input.big)<-c("B-day", "B-Night", "B-Sunrise", "B-Sunset")
  names(input.small)<-c("S-day", "S-Night", "S-Sunrise", "S-Sunset")
  
  output<-c(input.big, input.small)
  output<-round(output,2)
  
  return(output)
}

## plot.panel ####
# Combine R² and coefficient plots into a single multi-panel figure.
plot.panel <- function(input.big, input.small, sp){
  
  ## testing arguments
  # input.big=models$model.eu.pcab.big
  # input.small=models$model.eu.pcab.small
  # sp="PCAB"
  
  output<-ggarrange(plot.model.rsqr(input.big, input.small, sp),
                    plot.estimates(input.big, input.small, sp),
                    nrow=2,ncol=1,align="hv",heights=c(0.25,0.75),
                    common.legend=T, legend="none")
}

## acf_autocorr_by_tree_year ####
# Evaluate lag-1 temporal autocorrelation of model residuals by tree and year.
acf_autocorr_by_tree_year <- function(model, df, min_n = 5, threshold = 0.4) {
  
  ## testing arguments
  # model = models$model.bb.fasy.big
  # df = model_dataset$BB_FASY_bigTrees
  # min_n = 5
  # threshold = 0.4
  
  # Requires: DHARMa, dplyr
  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    stop("Package 'DHARMa' is required. Install it via install.packages('DHARMa').")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install it via install.packages('dplyr').")
  }
  
  # Check required columns (fixed names)
  req <- c("DOY", "Hour", "Tree", "Year")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("Missing required columns in df: ", paste(miss, collapse = ", "))
  }
  
  df2 <- df
  df2$time_day <- df2$DOY + df2$Hour / 24
  df2$group_ty <- interaction(df2$Tree, df2$Year, drop = TRUE)
  
  # DHARMa residuals
  res <- DHARMa::simulateResiduals(model)
  
  # Aggregate to unique time steps within Tree and Year
  tmp <- dplyr::summarise(
    dplyr::group_by(
      dplyr::mutate(df2, u = res$scaledResiduals),
      group_ty, time_day
    ),
    u_mean = mean(u, na.rm = TRUE),
    .groups = "drop"
  )
  
  tmp <- tmp[order(tmp$group_ty, tmp$time_day), ]
  
  # Lag-1 ACF per group
  acf1_one <- function(d) {
    if (nrow(d) < min_n) return(NA_real_)
    as.numeric(stats::acf(d$u_mean, plot = FALSE)$acf[2])
  }
  
  split_list <- split(tmp, tmp$group_ty)
  acf1 <- sapply(split_list, acf1_one)
  
  pvals <- data.frame(
    group_ty = names(acf1),
    acf1 = as.numeric(acf1),
    stringsAsFactors = FALSE
  )
  
  list(
    pvals = pvals,
    summary = summary(pvals$acf1),
    prop_above_threshold = mean(abs(pvals$acf1) > threshold, na.rm = TRUE)
  )
}

## Outputs - rsqr ####
rsqr.dataset<-rbind(extract.rsqr(models$model.rn.acca.big),
                    extract.rsqr(models$model.rn.acca.small),
                    extract.rsqr(models$model.rn.cabe.big),
                    extract.rsqr.glm(models$model.rn.cabe.small),
                    extract.rsqr(models$model.rn.ulsp.big),
                    extract.rsqr.glm(models$model.rn.ulsp.small),
                    extract.rsqr(models$model.zf.pcab.big),
                    extract.rsqr(models$model.zf.pcab.small),
                    extract.rsqr(models$model.zf.fasy.big),
                    extract.rsqr(models$model.zf.fasy.small),
                    extract.rsqr(models$model.bb.pcab.big),
                    extract.rsqr(models$model.bb.pcab.small),
                    extract.rsqr(models$model.bb.fasy.big),
                    extract.rsqr(models$model.bb.fasy.small),
                    extract.rsqr(models$model.eu.pcab.big),
                    extract.rsqr(models$model.eu.pcab.small))

rsqr.dataset$Site <-c(rep("Ranspurk",6),rep("Zofin",4),rep("Boubin",4),rep("Eustaska",2))
rsqr.dataset$Species <-c(rep("ACCA",2),rep("CABE",2),rep("ULSP",2),rep("PCAB",2),rep("FASY",2),rep("PCAB",2),rep("FASY",2),rep("PCAB",2))
rsqr.dataset$Size <-c(rep(c("big","small"),8))


## Outputs - model ####
rsqr.dataset<-add.x(rsqr.dataset)
print("-------------------------------------------")
print("Model RQSR")
print(rsqr.dataset)
print("Theoretical RQSR velke stromy")
print(round(subset(rsqr.dataset, Size=="big")$theoretical.R2m*100,1))
print("Theoretical RQSR male stromy")
print(round(subset(rsqr.dataset, Size=="small")$theoretical.R2m*100,1))

print("RE velke stromy")
print(round(subset(rsqr.dataset, Size=="big")$theoretical.R2c*100,1) - round(subset(rsqr.dataset, Size=="big")$theoretical.R2m*100,1))

print("RE male stromy")
print(round(subset(rsqr.dataset, Size=="small")$theoretical.R2c*100,1) - round(subset(rsqr.dataset, Size=="small")$theoretical.R2m*100,1))

## Outputs - Estimates ####
## Outputs - autocorrelations ####
results <- list()

results[["BB_FASY_big"]] <- acf_autocorr_by_tree_year(models$model.bb.fasy.big, model_dataset$BB_FASY_bigTrees)
results[["BB_FASY_small"]] <- acf_autocorr_by_tree_year(models$model.bb.fasy.small, model_dataset$BB_FASY_smallTrees)
results[["BB_PCAB_big"]] <- acf_autocorr_by_tree_year(models$model.bb.pcab.big, model_dataset$BB_PCAB_bigTrees)
results[["BB_PCAB_small"]] <- acf_autocorr_by_tree_year(models$model.bb.pcab.small, model_dataset$BB_PCAB_smallTrees)

results[["EU_PCAB_big"]] <- acf_autocorr_by_tree_year(models$model.eu.pcab.big, model_dataset$EU_PCAB_bigTrees)
results[["EU_PCAB_small"]] <- acf_autocorr_by_tree_year(models$model.eu.pcab.small, model_dataset$EU_PCAB_smallTrees)

results[["RN_ACCA_big"]] <- acf_autocorr_by_tree_year(models$model.rn.acca.big, model_dataset$RN_ACCA_bigTrees)
results[["RN_ACCA_small"]] <- acf_autocorr_by_tree_year(models$model.rn.acca.small, model_dataset$RN_ACCA_smallTrees)
results[["RN_CABE_big"]] <- acf_autocorr_by_tree_year(models$model.rn.cabe.big, model_dataset$RN_CABE_bigTrees)
results[["RN_CABE_small"]] <- acf_autocorr_by_tree_year(models$model.rn.cabe.small, model_dataset$RN_CABE_smallTrees)
results[["RN_ULSP_big"]] <- acf_autocorr_by_tree_year(models$model.rn.ulsp.big, model_dataset$RN_ULSP_bigTrees)
results[["RN_ULSP_small"]] <- acf_autocorr_by_tree_year(models$model.rn.ulsp.small, model_dataset$RN_ULSP_smallTrees)

results[["ZF_FASY_big"]] <- acf_autocorr_by_tree_year(models$model.zf.fasy.big, model_dataset$ZF_FASY_bigTrees)
results[["ZF_FASY_small"]] <- acf_autocorr_by_tree_year(models$model.zf.fasy.small, model_dataset$ZF_FASY_smallTrees)
results[["ZF_PCAB_big"]] <- acf_autocorr_by_tree_year(models$model.zf.pcab.big, model_dataset$ZF_PCAB_bigTrees)
results[["ZF_PCAB_small"]] <- acf_autocorr_by_tree_year(models$model.zf.pcab.small, model_dataset$ZF_PCAB_smallTrees)

ouptut_autocorrel <- data.frame(Site = c(rep("Boubin",4),rep("Eustaska",2),rep("Ranspurk",6),rep("Zofin",4)),
                                Species = c(rep("FASY",2),rep("PCAB",2),rep("PCAB",2),rep("ACCA",2),rep("CABE",2),rep("ULSP",2),rep("FASY",2),rep("PCAB",2)),
                                Size = rep(c("big", "small"), times = 8),
                                mean = c(results$BB_FASY_big$summary["Mean"], results$BB_FASY_small$summary["Mean"],
                                         results$BB_PCAB_big$summary["Mean"], results$BB_PCAB_small$summary["Mean"],
                                         
                                         results$EU_PCAB_big$summary["Mean"], results$EU_PCAB_small$summary["Mean"],
                                         
                                         results$RN_ACCA_big$summary["Mean"], results$RN_ACCA_small$summary["Mean"],
                                         results$RN_CABE_big$summary["Mean"], results$RN_CABE_small$summary["Mean"],
                                         results$RN_ULSP_big$summary["Mean"], results$RN_ULSP_small$summary["Mean"],
                                         
                                         results$ZF_FASY_big$summary["Mean"], results$ZF_FASY_small$summary["Mean"],
                                         results$ZF_PCAB_big$summary["Mean"], results$ZF_PCAB_small$summary["Mean"]),
                                
                                above_tresh = c(results$BB_FASY_big$prop_above_threshold, results$BB_FASY_small$prop_above_threshold,
                                                results$BB_PCAB_big$prop_above_threshold, results$BB_PCAB_small$prop_above_threshold,
                                                
                                                results$EU_PCAB_big$prop_above_threshold, results$EU_PCAB_small$prop_above_threshold,
                                                
                                                results$RN_ACCA_big$prop_above_threshold, results$RN_ACCA_small$prop_above_threshold,
                                                results$RN_CABE_big$prop_above_threshold, results$RN_CABE_small$prop_above_threshold,
                                                results$RN_ULSP_big$prop_above_threshold, results$RN_ULSP_small$prop_above_threshold,
                                                
                                                results$ZF_FASY_big$prop_above_threshold, results$ZF_FASY_small$prop_above_threshold,
                                                results$ZF_PCAB_big$prop_above_threshold, results$ZF_PCAB_small$prop_above_threshold))

## returning estimates for radiation ####
print("------------------------------------------------------------------------")
print("Global radiation estimates")
print("------------------------------------------------------------------------")

print("RN - ACCA")
print(return.R.estimate(models$model.rn.acca.big, models$model.rn.acca.small))
print("RN - CABE")
print(return.R.estimate(models$model.rn.cabe.big, models$model.rn.cabe.small))
print("RN - ULSP")
print(return.R.estimate(models$model.rn.ulsp.big, models$model.rn.ulsp.small))

print("ZF - PCAB")
print(return.R.estimate(models$model.zf.pcab.big, models$model.zf.pcab.small))
print("ZF - FASY")
print(return.R.estimate(models$model.zf.fasy.big, models$model.zf.fasy.small))

print("BB - PCAB")
print(return.R.estimate(models$model.bb.pcab.big, models$model.bb.pcab.small))
print("BB - FASY")
print(return.R.estimate(models$model.bb.fasy.big, models$model.bb.fasy.small))

print("EU - PCAB")
print(return.R.estimate(models$model.eu.pcab.big, models$model.eu.pcab.small))


print("------------------------------------------------------------------------")
print("Daphases estimates")
print("------------------------------------------------------------------------")
print("RN - ACCA")
print(return.dayphase.estimate(models$model.rn.acca.big, models$model.rn.acca.small))
print("RN - CABE")
print(return.dayphase.estimate(models$model.rn.cabe.big, models$model.rn.cabe.small))
print("RN - ULSP")
print(return.dayphase.estimate(models$model.rn.ulsp.big, models$model.rn.ulsp.small))

print("ZF - PCAB")
print(return.dayphase.estimate(models$model.zf.pcab.big, models$model.zf.pcab.small))
print("ZF - FASY")
print(return.dayphase.estimate(models$model.zf.fasy.big, models$model.zf.fasy.small))

print("BB - PCAB")
print(return.dayphase.estimate(models$model.bb.pcab.big, models$model.bb.pcab.small))
print("BB - FASY")
print(return.dayphase.estimate(models$model.bb.fasy.big, models$model.bb.fasy.small))

print("EU - PCAB")
print(return.dayphase.estimate(models$model.eu.pcab.big, models$model.eu.pcab.small))

## returning autocorrelations ####
print("------------------------------------------------------------------------")
print("autocorrelation analysis")
print("------------------------------------------------------------------------")
print(ouptut_autocorrel)

## Figure making ####
figure <- ggarrange(plot.panel(models$model.rn.acca.big, models$model.rn.acca.small, "ACCA"),
                    plot.panel(models$model.rn.cabe.big, models$model.rn.cabe.small, "CABE"),
                    plot.panel(models$model.rn.ulsp.big, models$model.rn.ulsp.small, "ULSP"),
                    plot.panel(models$model.zf.pcab.big, models$model.zf.pcab.small, "PCAB"),
                    plot.panel(models$model.zf.fasy.big, models$model.zf.fasy.small, "FASY"),
                    plot.panel(models$model.bb.pcab.big, models$model.bb.pcab.small, "PCAB"),
                    plot.panel(models$model.bb.fasy.big, models$model.bb.fasy.small, "FASY"),
                    plot.panel(models$model.eu.pcab.big, models$model.eu.pcab.small, "PCAB"),
                    nrow=2,ncol=4,align="hv",labels=LETTERS[1:8])
