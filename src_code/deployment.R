library(RODBC)
library(dotenv)
source("microsoft_serialization.R")

bayesGLM_mod <- readRDS(file = "~/EPIC_Project/Modeling Prob. Win/serialized_models/bayes_glm.rds")
# modelbin <- serialize(bayesGLM_mod, NULL)
# modelbinstr=paste(modelbin, collapse="")

# Load the environment variables related to database stuff
dotenv::load_dot_env("db_qa.env")

# Get the channel
channel <- odbcConnect(Sys.getenv("dsn"),
                       Sys.getenv("user"),
                       Sys.getenv("pass"))
# 
# q <- paste("EXEC PersistModel @m='", modelbinstr,"'", sep="")
# sqlQuery(channel, q)
# 
# fileName <- "~/EPIC_Project/Modeling Prob. Win/serialized_models/binary.txt"
# binary <- readChar(fileName, file.info(fileName)$size)
# # 
# # bin_mod <- unserialize(as.raw(modelbinstr))
# s3 <- substring(modelbinstr, seq(1,nchar(modelbinstr),2), seq(2,nchar(modelbinstr),2))
# s4 <- as.raw(as.integer(paste0('0x', s3)))
# unserialize(s4)
# 
# charToRaw(binary)

#sqlConnString <- "Driver=SQL Server Native Client 11.0;Server=EPICPORTAL; Database=EPICWebPortalQA"
connectionString <- "Data Source=EPICPORTAL;Initial Catalog=EPICWebPortalQA;Integrated Security=True;MultipleActiveResultSets=True"
dbSaveRDS(connectionString = connectionString, 
          key = 'bayesGLM', 
          object = bayesGLM_mod$finalModel, 
          table = '[mkt].[CalcProbModel]',
          objectColumn = 'model')

