# Get functions for preprocessing
source("functions.R")
library(lubridate)


# Read in the raw data
raw_data <- read.csv("data/raw/raw_data.csv")

# Remove leading and trailing whitespace
raw_data <- as.tibble(lapply(raw_data, trimws), stringsAsFactors = FALSE)

# Change to numeric
columns_to_replace = c("Amount_current", 
                       "numEngagements", "SalesLeadId",
                       "CalcProbability", "num_sales_leads", 
                       "num_projects_won", "num_ROM_Quoted",
                       "num_Quoted_FEE", "num_Awarded_FEE", 
                       "num_FEE_in_progress", "num_Completed_FEE", 
                       "num_Project_Quoted","num_notes", 
                       "audit_frequency")

# Change to numeric
raw_data[columns_to_replace ] <- lapply(raw_data[columns_to_replace ], as.numeric)

summary(is.na(raw_data))

# Deal with NAs
# Calc prob set to 1 because these are typically leads early in the pipeline
raw_data$CalcProbability[is.na(raw_data$CalcProbability)] <- 1
# Replace with the most commonly occuring values
raw_data[,c("Instinct_Factor_Id")] <- names(sort(table(raw_data[,c("Instinct_Factor_Id")]), 
                                                 decreasing = TRUE)[1])
raw_data[,c("Driver_Value_Id")] <- names(sort(table(raw_data[,c("Driver_Value_Id")]), 
                                              decreasing = TRUE)[1])
# Ignore these columns since they will be used later or dropped from the data frame
ignore <- c("CloseReasonId", "ActionDate",  
            "CloseReasonDate", "CloseReasonDate_current", 
            "CloseReason_current", "Amount_current")
# Reassign NA values to 0
# since values are missing in clusters only for earlier records for some leads
raw_data[rowSums(is.na(raw_data[,!names(raw_data) %in% ignore])) > 0,
         !names(raw_data) %in% ignore] <- 
  raw_data %>% 
  select(-c(CloseReasonId, ActionDate, 
          CloseReasonDate, CloseReasonDate_current, 
          CloseReason_current, Amount_current)) %>% 
  filter(!complete.cases(.)) %>% 
  mutate_if(is.character , replace_na, replace = 0)

# Create a variable for the days difference in CreatedDate 
# from an arbitrary baseline ('2016-01-01')
raw_data$CreatedDateProxy <- as.numeric(base::as.Date(as.character(raw_data$CreatedDate)) - 
                                        lubridate::date('2016-01-01'))

# Whether or not the project was awarded
# This is the target variable
raw_data$ProjAwarded <- as.factor(ifelse(raw_data$CloseReason_current == "Awarded project", 
                                         1, 
                                         0))

# Examine which sales leads have NA for close reason but a close date
raw_data %>% 
  select(SalesLeadId, CloseReason_current, CloseReasonDate_current) %>% 
  filter(is.na(CloseReason_current), !is.na(CloseReasonDate_current)) %>% 
  unique() %>% 
  select(SalesLeadId)
# Get rid of these - reason is: what if we win a lead after closing it 
# but train our model on the incorrect presumption that we lost the lead
raw_data <- raw_data %>%
  filter(!SalesLeadId %in% 
        pull(raw_data %>% 
             select(SalesLeadId, CloseReason_current, CloseReasonDate_current) %>% 
             filter(is.na(CloseReason_current), !is.na(CloseReasonDate_current)) %>% 
             unique() %>% 
             select(SalesLeadId))
        )

# Subset of data corresponding to closed leads
# Don't want to include Custom Machine leads data
# since this is not a business in which EPIC engages any longer
closed_leads <- as.data.frame(raw_data[!is.na(raw_data$ProjAwarded) & 
                                       raw_data$BusinessGroup != 'Custom Machine',])

# Remove records where the lead is marked as active but has a CloseReasonDate
# These don't appear anomalous - 
# rather, they seem to be the result of the database writing procedure
closed_leads <- closed_leads[!(closed_leads$IsActive == 1 & 
                                 !is.na(closed_leads$CloseReasonDate)),]

# Create a variable for the difference in the created date and the date of the current audit
closed_leads$created_audit_diff <- as.numeric(base::as.Date(as.character(closed_leads$AuditDate)) - 
                                              base::as.Date(as.character(closed_leads$CreatedDate)))

# Check for sales leads with > 1 business group
multi_group_sl <- closed_leads %>% 
  select(SalesLeadId, BusinessGroup) %>% 
  unique() %>% 
  dplyr::count(SalesLeadId) %>% 
  filter(n>1)
# Reassign the business groups in these records
i <- 1
for(saleslead in closed_leads$SalesLeadId){
  if(closed_leads$SalesLeadId[i] %in% multi_group_sl$SalesLeadId){
    if("Vision" %in% closed_leads$BusinessGroup[closed_leads$SalesLeadId == saleslead]){
      closed_leads$BusinessGroup[i] <- "Vision"
    }
    else if("Process Systems" %in% closed_leads$BusinessGroup[closed_leads$SalesLeadId == saleslead]){
      closed_leads$BusinessGroup[i] <- "Process Systems"
    }
    else{closed_leads$BusinessGroup[i] <- "Corporate"}
  }
  i <- i + 1
}
# Remove the resulting duplicate records 
# (business group is the only column that duplicates records)
closed_leads <- closed_leads %>% distinct()

summary(is.na(closed_leads))

# Create a new variable representing days to action date from the audit date
closed_leads$action_audit_diff <- ifelse(is.na(closed_leads$ActionDate),
                                         30,
                                         base::as.Date(as.character(closed_leads$ActionDate)) - 
                                         base::as.Date(as.character(closed_leads$AuditDate))
                                        )

# A data frame that represents sales leads early on in the sales funnel
# We take the first record for each sales lead that is available
# so long as it was set up within the time frame represented in the data set
early_stage <- as.data.frame(closed_leads %>% 
  filter(created_audit_diff == 0) %>% 
  group_by(SalesLeadId) %>% 
  arrange(SalesLeadId, AuditDate) %>% 
  filter(row_number()==n()))

# In addition to early stage leads, we want a data frame that represents 
# the set of records at and before the mid-life of each sales lead.
# The algo: for each sales lead id (sorted by sales lead id and numEngagements),
#           add half the days difference between close date and created date to created date
#           as this is the mid-life point, call it mid_dates. Filter out records where
#           the audit date is past the mid date and this is the final data set.
#           Another procedure considered was slice(1:(n()/2)), which would take the midpoint
#           in the number of audit records, but this may be biased one way or another
#           e.g. many audits in one day or more audits toward lead close - not truly
#           representative of the half-life of a lead
mid_stage <- as.data.frame(closed_leads %>% 
  filter(SalesLeadId %in% early_stage$SalesLeadId) %>% 
  group_by(SalesLeadId) %>% 
  arrange(SalesLeadId, AuditDate) %>% 
  mutate(mid_dates = (lubridate::as_datetime(CreatedDate) +
                          floor((lubridate::as_datetime(CloseReasonDate_current) - 
                                 lubridate::as_datetime(CreatedDate))/2))) %>% 
  #filter(AuditDate <= mid_dates) %>% 
  slice(1:which.min(abs(lubridate::as_datetime(AuditDate) - mid_dates[1]))) %>% 
  select(-mid_dates))

# The records before the lead is marked closed
late_stage <- as.data.frame(closed_leads %>% 
  filter(SalesLeadId %in% early_stage$SalesLeadId) %>% 
  group_by(SalesLeadId) %>% 
  arrange(SalesLeadId, AuditDate) %>% 
  mutate(rownum = row_number()) %>% 
  slice(1:(max(rownum) - 1)) %>% 
  select(-rownum))

# Put the different data frames in a list
data_list <- list(closed = closed_leads, 
                  early = early_stage, 
                  mid = mid_stage,
                  late = late_stage)

# Prepare data for the model - remove unnecessary variables
drops <- c('ModifiedDate', 'CloseReasonId',
           'Project', 'IsActive', 'CloseReasonDate', 
           'ActionDate', 'CloseReasonDate_current', 
           'IsActive_current', 'CloseReason_current',
           'CreatedDate','CompanyId')
# Drop
data_list <- lapply(data_list, function(x){
  x[ ,!(names(x) %in% drops)]
})

# Create a vector of categorical variable names
categoricals <- c()
for(i in 1:length(data_list$closed)){
  if(length(data_list$closed[i][is_character(data_list$closed[,i])]) > 0){
    categoricals <- append(categoricals, names(data_list$closed[i]))
  }
}
# Examine unique combinations of sales lead id and each categorical variable.
# If count of observations is less than 30, collapse levels into "other"
data_list <- lapply(data_list, function(x){
  for(var in categoricals){
    names <- c("SalesLeadId", var)
    tmp <- dplyr::pull(unique(x[ ,names]) %>%
                       # Note that !! is rlang for unquoting
                       dplyr::count(!!as.name(var)) %>%
                       dplyr::filter(n < 30) %>%
                       dplyr::select(!!as.name(var)))
    i <- 0
    for(element in x[,var]){
      i <- i + 1
      if((element %in% tmp) & (length(tmp) > 1)){
        x[,var][i] <- "other"
      }
    }
  }
  return(x)
})

# Variables that wouldn't be "properly" captured by taking the last record
# Want the max of these (feature engineering)
to_max <- c("audit_frequency", "created_audit_diff", "action_audit_diff")
data_list <- lapply(data_list, function(x){
  # The grouping variable needs to be retained
  x[,c("SalesLeadId", to_max)] <- as.data.frame(x %>% 
                                                  group_by(SalesLeadId) %>% 
                                                  mutate(audit_freq_max = max(audit_frequency),
                                                         created_audit_diff_max = max(created_audit_diff),
                                                         action_audit_diff_max = max(action_audit_diff)) %>% 
                                                  select(SalesLeadId,
                                                         audit_freq_max, 
                                                         created_audit_diff_max, 
                                                         action_audit_diff_max)
                                                )
  return(x)
})

# We can now comfortably take the last record for each sales lead id
model_data <- lapply(data_list, function(x){
  x <- Flatten(df = x)
  return(x)
})

# Write the data frames in model_data to csv files
mapply(write.csv, 
       model_data, 
       row.names = FALSE, 
       file = paste0("data/clean/flattened_", names(model_data), ".csv")
       )

# Write the long data to csv files
mapply(write.csv, 
       data_list, 
       row.names = FALSE, 
       file = paste0("data/clean/long_format_", names(data_list), ".csv")
)
