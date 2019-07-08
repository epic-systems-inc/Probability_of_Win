# Load Libraries
library(scales)
library(lubridate)
library(mlr)
library(tidyverse)
library(caret)
library(lattice)
library(RANN)

# Disable scientific notation
options(scipen=999)

# User-defined functions
source("functions.R")

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

# Split training-test set
# Splitting is stratified random sampling within quartile ranges of the target variable
# The idea is to try to preserve the distribution
set.seed(123)
in_train <- caret::createDataPartition(y = model_data$flattened_closed$ProjAwarded, 
                                       p = 0.5, 
                                       list = FALSE)
training_set <- model_data$flattened_closed[in_train,]
# Remove near-zero variance predictors for model stability
training_set <- training_set[ , !(names(training_set) %in% caret::nzv(training_set, names = TRUE))]
model_features <- names(training_set)

# The testing set that will be used as the final arbiter
testing_set <- model_data$flattened_closed[-in_train, model_features]

# Sales lead ids in the training set
sales_train <- training_set$SalesLeadId

# Variations of the testing set focused on the particular stage
# Want only data for leads not in the training set, so we exclude sales lead ids in sales_train
test_set_stages <- list(
  test_early = model_data$flattened_early[!model_data$flattened_early$SalesLeadId %in% sales_train,
                                            model_features],
  test_mid = model_data$flattened_mid[!model_data$flattened_mid$SalesLeadId %in% sales_train,
                                       model_features],
  test_late = model_data$flattened_late[!model_data$flattened_late$SalesLeadId %in% sales_train,
                                         model_features]
)
###############################################################################
########################## CURRENT MODEL DIAGNOSTICS ##########################
###############################################################################
## How well-calibrated are the predicted probabilities for the early-stage data?
ealy_grid <- gridCalPlot(test_data = test_set_stages$test_early, stage = "Early-")
## How well-calibrated are the predicted probabilities for the mid-stage data?
gridCalPlot(test_data = test_set_stages$test_mid, stage = "Mid")
## How well-calibrated are the predicted probabilities for the late-stage data?
gridCalPlot(test_data = test_set_stages$test_late, stage = "Late-")

# What the current model would be predicting
current_pred <- lapply(test_set_stages, function(x){
  prob <- x$CalcProbability * 0.01
  prediction <- as.factor(ifelse(prob >= 0.5, "Awarded", "Lost"))
  return(prediction)
})
# Confusion matrix at each stage
confusionMatrix(current_pred$test_early, test_set_stages$test_early$ProjAwarded)
confusionMatrix(current_pred$test_mid, test_set_stages$test_mid$ProjAwarded)
confusionMatrix(current_pred$test_late, test_set_stages$test_late$ProjAwarded)

###############################################################################
############################### MODEL TRAINING ################################
###############################################################################

# Control function for model traning and tuning
ctrl = caret::trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5,
                           verboseIter = TRUE, 
                           savePredictions = "final",
                           classProbs = TRUE
)

# Train without these for now
excluded_var <- c("SalesLeadId", "CalcProbability")
  
#### Logistic Regression
set.seed(123)
glm_mod <- caret::train(ProjAwarded ~ ., 
                        data = training_set[!names(training_set) %in% excluded_var],
                        preProcess = c("center", "scale", "pca"),
                        method = "glm", 
                        family = "binomial",
                        trControl = ctrl)
glm_mod

### Model diagnostics
caret::confusionMatrix.train(glm_mod)

## How well-calibrated are the predicted probabilities for the early-stage data?
gridCalPlot(glm_mod,
            test_data = test_set_stages$test_early, 
            stage = "Early-")
## How well-calibrated are the predicted probabilities for the mid-stage data?
gridCalPlot(glm_mod,
            test_data = test_set_stages$test_mid, 
            stage = "Mid")
## How well-calibrated are the predicted probabilities for the late-stage data?
gridCalPlot(glm_mod,
            test_data = test_set_stages$test_late, 
            stage = "Late-")


#### Boosted Logistic Regression
set.seed(123)
logitBoost_mod <- caret::train(ProjAwarded ~ ., 
                               data = training_set[!names(training_set) %in% excluded_var],
                               method = "LogitBoost", 
                               trControl = ctrl)
logitBoost_mod

### Model diagnostics
varImp(logitBoost_mod)
caret::confusionMatrix.train(logitBoost_mod)

## How well-calibrated are the predicted probabilities for the early-stage data?
gridCalPlot(model = logitBoost_mod,
            test_data = test_set_stages$test_early, stage = "Early-")
## How well-calibrated are the predicted probabilities for the mid-stage data?
gridCalPlot(model = logitBoost_mod,
            test_data = test_set_stages$test_mid, stage = "Mid")
## How well-calibrated are the predicted probabilities for the late-stage data?
gridCalPlot(model = logitBoost_mod,
            test_data = test_set_stages$test_late, stage = "Late-")

#### Bayesian Generalized Linear Model
set.seed(123)
bayesGLM_mod <- caret::train(ProjAwarded ~ ., 
                             data = training_set[ ,!names(training_set) %in% excluded_var],
                             method = "bayesglm", 
                             trControl = ctrl)
bayesGLM_mod

### Model diagnostics
caret::varImp(bayesGLM_mod)
caret::confusionMatrix.train(bayesGLM_mod)
## How well-calibrated are the predicted probabilities for the early-stage data?
gridCalPlot(model = bayesGLM_mod,
            test_data = test_set_stages$test_early, 
            stage = "Early-")
## How well-calibrated are the predicted probabilities for the mid-stage data?
gridCalPlot(model = bayesGLM_mod,
            test_data = test_set_stages$test_mid, 
            stage = "Mid")
## How well-calibrated are the predicted probabilities for the late-stage data?
gridCalPlot(model = bayesGLM_mod,
            test_data = test_set_stages$test_late, 
            stage = "Late-")

###############################################################################
####################### TESTING SET AS FINAL ARBITRATOR #######################
###############################################################################
# The selected model (Bayesian GLM)
bayesGLM_pred_prob <- predict.train(bayesGLM_mod, 
                                    newdata = testing_set,
                                    type = "prob")$Awarded
bayesGLM_pred <- predict.train(bayesGLM_mod, 
                               newdata = testing_set)
cm_bayes <- confusionMatrix(bayesGLM_pred, testing_set$ProjAwarded)

# The old model
old_test_pred_prob <- testing_set$CalcProbability*0.01
old_test_pred <- as.factor(ifelse(testing_set$CalcProbability >= 50,
                                  "Awarded", 
                                  "Lost"))
cm_old <- confusionMatrix(old_test_pred, testing_set$ProjAwarded)

# Brier score
## The old model
1/nrow(testing_set)*sum((old_test_pred_prob - ifelse(testing_set$ProjAwarded == "Awarded", 1, 0))**2)
## Decomposition
### The Brier score is equal to the reliability - resolution + uncertainty
#### Uncertainty

## The Bayesian GLM
1/nrow(testing_set)*sum((bayesGLM_pred_prob - ifelse(testing_set$ProjAwarded == "Awarded", 1, 0))**2)
## Decomposition
### Uncertainty



## Hypothesis testing
# # The accuracy of the old and new model
# p_1 <- cm_old$overall[[1]]
# p_2 <- cm_bayes$overall[[1]]
# # The number of samples correctly predicted for the models
# x <- list(cm_old$table[1]+cm_old$table[4], cm_bayes$table[1]+cm_bayes$table[4])
# p_hat <- (x[[1]] + x[[2]])/(2*nrow(testing_set))
# # The test statistic
# test_stat <- (p_1 - p_2)/sqrt(2*p_hat*(1-p_hat)/nrow(testing_set))
