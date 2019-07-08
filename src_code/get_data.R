library(RODBC)
library(dotenv)

#' The purpose of this file is to get the raw data necessary for the model.

# Load the environment variables related to database stuff
dotenv::load_dot_env("db_qa.env")

# Get the channel
channel <- odbcConnect(Sys.getenv("dsn"),
                       Sys.getenv("user"),
                       Sys.getenv("pass"))
# Run the query
# This is the main view
raw_data <- RODBC::sqlQuery(channel, Sys.getenv("query1"))
# Close the ODBC connection
RODBC::odbcClose(channel = channel)


# Remove records for which ModifiedDate > CloseReasonDate
# We do not care for records that have been modified after closing
# In addition, a "fudge factor" of 5 days is used to account for time lags in database updates
raw_data <- raw_data[-which((as.Date(raw_data$ModifiedDate) - 
                               lubridate::days(5) >= raw_data$CloseReasonDate) |
                              (as.Date(raw_data$CloseReasonDate_current) <= 
                                 as.Date(raw_data$CreatedDate))), ]

write.csv(raw_data, "data/raw/raw_data.csv", row.names = FALSE)
