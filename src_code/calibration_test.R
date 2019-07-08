library(tidyverse)
source("functions.R")

# Load the trained model
bayesGLM_mod <- readRDS(file = "~/EPIC_Project/Modeling Prob. Win/serialized_models/bayes_glm.rds")

# Load the sales leads used in the training set
sales_train <- readRDS(file = "sales_in_train.rds")

# Load the data into the environment
dir = getwd()
model_data <- lapply(list.files(path = paste0(dir,"/data/clean/"),
                                pattern = "^flattened",
                                full.names = TRUE),
                     read.csv,
                     header = TRUE)
# Name the list elements
names(model_data) <- gsub(".csv", "",
                          list.files(path = paste0(dir,"/data/clean/"),
                                     pattern = "^flattened",
                                     full.names = FALSE))

categoricals <- c("OwnerId", "Made_Initial_Contact", "Project_Scope_Fully_Understood",
                  "Established_EPICs_Superiority", "ROM_Quoted", "Quoted_FEE",                    
                  "Awarded_FEE", "FEEinprogress", "ProjectQuoted", "Completed_FEE", "Q1", "Q2",                            
                  "Q3", "Driver_Value_Id", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11",                           
                  "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Instinct_Factor_Id",            
                  "Q21", "Q22", "Q23", "HasFileUploaded", "BusinessGroup")
# Set categorical variables as factors
model_data <- lapply(model_data, function(x){
  x[,names(x) %in% categoricals] <- 
    lapply(x[,names(x) %in% categoricals], as.factor)
  return(x)
})
# Ensure that the outcomes are properly ordered
model_data <- lapply(model_data, function(x){
  x$ProjAwarded = as.factor(ifelse(x$ProjAwarded == "1", "Awarded", "Lost"))
  x$ProjAwarded = relevel(x$ProjAwarded, "Awarded")
  return(x)
})

# Put the stages data that wasn't used in model training in a list for testing calibration
test_set_stages <- list(
  early = model_data$flattened_early[!model_data$flattened_early$SalesLeadId %in% sales_train,],
  mid = model_data$flattened_mid[!model_data$flattened_mid$SalesLeadId %in% sales_train,],
  late = model_data$flattened_late[!model_data$flattened_late$SalesLeadId %in% sales_train,]
)


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Early stage data
early_cal <- calibrationPlot(model = bayesGLM_mod, 
                             test_data = test_set_stages$early)
early_cal_process <- calibrationPlot(model = bayesGLM_mod, 
                                     test_data = test_set_stages$early,
                                     by_group = "process")
early_cal_vision <- calibrationPlot(model = bayesGLM_mod, 
                                test_data = test_set_stages$early,
                                by_group = "vision")
gridCalPlot(model = bayesGLM_mod, test_data = test_set_stages$early, stage = "Early-",labels = FALSE)
## Midstage data
mid_cal <- calibrationPlot(model = bayesGLM_mod, 
                           test_data = test_set_stages$mid)
mid_cal_process <- calibrationPlot(model = bayesGLM_mod, 
                                   test_data = test_set_stages$mid,
                                   by_group = "process")
mid_cal_vision <- calibrationPlot(model = bayesGLM_mod, 
                                  test_data = test_set_stages$mid,
                                  by_group = "vision")
gridCalPlot(model = bayesGLM_mod, test_data = test_set_stages$mid, stage = "Mid",labels = FALSE)
# Late stage data
late_cal <- calibrationPlot(model = bayesGLM_mod, 
                            test_data = test_set_stages$late)
late_cal_process <-  calibrationPlot(model = bayesGLM_mod, 
                                     test_data = test_set_stages$late,
                                     by_group = "process")
late_cal_vision <- calibrationPlot(model = bayesGLM_mod, 
                                   test_data = test_set_stages$late,
                                   by_group = "vision")
gridCalPlot(model = bayesGLM_mod, test_data = test_set_stages$late, stage = "Late-",labels = FALSE)
