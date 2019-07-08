library(RODBC)
library(dotenv)
source("microsoft_serialization_fork.R")

# Read in the serialized caret train object
bayesGLM_mod <- readRDS(file = "~/EPIC_Project/Probability_of_Win/serialized_models/bayes_glm.rds")

# Load the environment variables related to database stuff
dotenv::load_dot_env("db_qa.env")

# Get the channel
channel <- odbcConnect(Sys.getenv("dsn"),
                       Sys.getenv("user"),
                       Sys.getenv("pass"))

connectionString <- "Data Source=EPICPORTAL;Initial Catalog=EPICWebPortalQA;Integrated Security=True;MultipleActiveResultSets=True"
dbSaveRDS(connectionString = connectionString, 
          key = 1, 
          object = bayesGLM_mod$finalModel, 
          table = '[mkt].[CalcProbModel]',
          objectColumn = 'model',
          description = 'The data was split 50/50 training-testing. The training set contained about 700 records. 5 repeats of 10-fold CV.',
          model_type='Bayesian GLM')

