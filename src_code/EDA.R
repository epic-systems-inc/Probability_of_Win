# Load Libraries
library(scales)
library(lubridate)
library(mlr)
library(fitdistrplus)
library(xts)
library(tidyverse)

# Disable scientific notation
options(scipen=999)
# Load the preprocessed data into the environment
data <- read.csv("data/raw/raw_data.csv")

# The unique combinations of SalesLeadId and CloseReason_current
sales.reason.unique <- unique(data[,c('SalesLeadId', 'CloseReason_current')])
# The SalesLeadIds for projects that have been marked as won
sales.id.awarded <- sales.reason.unique$SalesLeadId[which(sales.reason.unique$CloseReason_current == "Awarded project")]

# Summary of unique SalesLeadId and CloseReason_current
# Gives an idea of the distribution of closed leads over the reasons
summary(unique(data[,c('SalesLeadId', 'CloseReason_current')]), maxsum = 20)
length(unique(data$SalesLeadId[which(data$IsActive == 0 & data$CloseReasonId == 1)]))  # Verify that this is the same # obs
unique(data$SalesLeadId[which(data$CloseReasonId == 1)]) # Same bc all leads with CloseReasonId == 1 have IsActive == 0

# Which SalesLeadIds are marked as awarded but not in unique(data$SalesLeadId[which(data$CloseReasonId == 1)])?
# I.e. the field CloseReason_current = "Awarded project" but CloseReasonId = <NA> or 0
sales.id.awarded[which(!(sales.id.awarded %in% unique(data$SalesLeadId[which(data$CloseReasonId == 1)])))]
# Vice-versa:
unique(data$SalesLeadId[which(data$CloseReasonId == 1)])[which(!(unique(data$SalesLeadId[which(data$CloseReasonId == 1)]) %in% sales.id.awarded))]

# Want number of engagements corresponding to when the lead was closed (won)
unique(data$SalesLeadId[which(data$SalesLeadId %in% sales.id.awarded)])  # unique SalesLeadIds for awarded proj
won.engagements <- c()
for(i in 1:nrow(data)){
    if(data$SalesLeadId[i] %in% sales.id.awarded){
      if(length(data$IsActive[i-1]) > 0 & length(data$SalesLeadId[i-1] > 0)){
        if(data$IsActive[i] == 0 & data$IsActive[i-1] != 0){
          won.engagements[length(won.engagements)+1] <- data$numEngagements[i]        
        }
      }
    }
}


#### Unexpected Results
#::::::::::::::::::::::

# Currently marked active but have been closed at some point:
unique(data$SalesLeadId[which(data$SalesLeadId %in% closed.leads$salesLeadId.vec & data$IsActive_current == 1)])

# Zero previous engagements and project awarded
hist(data$numEngagements[which(data$CloseReason_current=="Awarded project")])
no.engagements <- data[which(data$CloseReason_current=="Awarded project" & data$numEngagements == 0),]

# data for which there was no initial contact
no.initial.contact <- as.data.frame(unclass(summary(data[which(data$Made_Initial_Contact == "0"),])))

# Suspicious data where there are NAs in Made_Initial_Contact - should be binary-valued
suspect <- as.data.frame(data[which(is.na(data$Made_Initial_Contact)),])

