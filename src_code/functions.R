source("caret_calibration.R")
library(tidyverse)
library(ggpubr)

# The following function transforms a data frame by adding features and collapsing sales leads
# into one observation
Flatten <- function(df){
  # Collapse the observations onto one row for each sales lead id
  as.data.frame(df %>% 
                  group_by(SalesLeadId) %>%
                  arrange(SalesLeadId, AuditDate) %>% 
                  filter(row_number() == n())) -> df
  return(df)
}
calibrationAes <- function(calibration_obj, title = ""){
  cal <- ggplot(calibration_obj, aes(x = calibration_obj$data$midpoint,
                                     y = calibration_obj$data$Percent)) +
    geom_abline(lty = 3, size = .9, color =  "gray") +
    geom_vline(xintercept = calibration_obj$data$midpoint,
               alpha = 0.2) +
    ggtitle(title) +
    ylab("") +
    xlab("") +
    theme_bw()
  return(cal)
}
calibrationPlot <- function(model, old_mod = TRUE, test_data, cuts = 5, by_group = ""){
  #' This function takes EITHER a caret train object ('model') or direct probabilities
  #' as its arguments to calculate calibration. If the former is given,
  #' probabilities are calculated by predicting using the train object and test_data
  #' as the newdata argument in predict.train(obj, newdata, type = "prob").
  #' 
  #' 
  #' Out: a calibation object.
  
  # Rename the outcome variable for compatibility with caret
  test_data <- as.data.frame(dplyr::rename(test_data, .outcome = ProjAwarded))
  # Indexes corresponding to business group data
  process_index = which(test_data$BusinessGroup == "Process Systems")
  vision_index = which(test_data$BusinessGroup == "Vision")
  
  ylb <- ggplot2::ylab("Observed Event Percentage")
  xlb <- ggplot2::xlab("Predicted Probability Bin Midpoint")

  # If the model argument is supplied, calculate the probabilities
  if(!missing(model)){
    model_data <- test_data[,names(model$trainingData)]
    test_probs <- data.frame(obs = test_data$.outcome,
                             old = c(test_data$CalcProbability * 0.01),
                             new = caret::predict.train(model, newdata = model_data, type = "prob")$Awarded)
  }else{
    test_probs <- data.frame(obs = test_data$.outcome,
                             old = c(test_data$CalcProbability * 0.01))
  }
  # If the model is supplied and old_mod is not set to FALSE 
  if(!missing(model) & old_mod){
    if(tolower(by_group) == "process"){
      calibration_obj <- calibration(obs ~ new + old,
                                     data = test_probs[process_index,],
                                     class = "Awarded",
                                     cuts = cuts)
      cal <- calibrationAes(calibration_obj, title = "Process Systems") + xlb + ylb
    }
    else if(tolower(by_group) == "vision"){
      calibration_obj <- calibration(obs ~ new + old,
                                     data = test_probs[vision_index,],
                                     class = "Awarded",
                                     cuts = cuts)
      cal <- calibrationAes(calibration_obj, title = "Vision") + xlb + ylb
    }
    else{
      calibration_obj <- calibration(obs ~ new + old,
                                     data = test_probs,
                                     class = "Awarded",
                                     cuts = cuts)
  
      cal <- calibrationAes(calibration_obj, title = "All Business Groups") + xlb + ylb
    }
  }
  else if(old_mod){
    if(tolower(by_group) == "process"){
      calibration_obj <- calibration(obs ~ old,
                                     data = test_probs[process_index,],
                                     class = "Awarded",
                                     cuts = cuts)
      cal <- calibrationAes(calibration_obj, title = "Process Systems") + xlb + ylb
    }
    else if(tolower(by_group) == "vision"){
      calibration_obj <- calibration(obs ~ old,
                                     data = test_probs[vision_index,],
                                     class = "Awarded",
                                     cuts = cuts)
      cal <- calibrationAes(calibration_obj, title = "Vision") + xlb + ylb
    }
    else{
      calibration_obj <- calibration(obs ~ old,
                                     data = test_probs,
                                     class = "Awarded",
                                     cuts = cuts)

      cal <- calibrationAes(calibration_obj, title = "All Business Groups") + xlb + ylb
    }
  }
  return(cal)
}
gridCalPlot <- function(model = NA, test_data = NA, stage = "", labels = TRUE){
  if(labels){
    ylb <- ggplot2::ylab("Observed Event Percentage")
    xlb <- ggplot2::xlab("Bin Midpoint")
  } else { 
    ylb <- ggplot2::ylab("")
    xlb <- ggplot2::xlab("")
  }
  if(!missing(model)){
    out <- annotate_figure(
      ggarrange(
        # Aggregate
        calibrationPlot(model = model, 
                        test_data = test_data) + ylb + xlb,
        # By Process
        calibrationPlot(model = model, 
                        test_data = test_data, 
                        by_group = "process") + ylab("Observed Event Percentage") + xlb,
        # By Vision
        calibrationPlot(model = model, 
                        test_data = test_data, 
                        by_group = "vision") + xlab("Bin Midpoint") + ylb,
        common.legend = TRUE,
        nrow = 3,
        legend = "right"
      ), top = text_grob(paste0(stage, "stage Data"), size = 14),
         bottom = text_grob("Predicted Probabilities", size = 12)
    )
  } else {

    # Aggregate
    aggregate_ <- calibrationPlot(test_data = test_data) + ylb + xlb
    # By Process
    process_ <- calibrationPlot(test_data = test_data, 
                    by_group = "process") + ylab("Observed Event Percentage") + xlb
    # By Vision
    vision_ <- calibrationPlot(test_data = test_data, 
                    by_group = "vision") + xlab("Bin Midpoint") + ylb
  out <- annotate_figure(
    ggarrange(
    # Aggregate
    aggregate_,
    # By Process
    process_,
    # By Vision
    vision_,
    common.legend = TRUE,
    nrow = 3,
    legend = "right"
    ), top = text_grob(paste0(stage, "stage Data"), size = 14),
       bottom = text_grob("Predicted Probabilities", size = 12)
       
  )
  }
  return(out)
}