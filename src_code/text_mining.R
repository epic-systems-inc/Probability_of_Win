library(RODBC)
library(tidyverse)
library(tidytext)
# Load the environment variables related to database stuff
dotenv::load_dot_env("db.env")

# Get the channel
channel <- odbcConnect(Sys.getenv("dsn"),
                       Sys.getenv("user"),
                       Sys.getenv("pass"))
text_data <- sqlQuery(channel, 
                     "SELECT DISTINCT sla.Project, sla.SalesLeadId, bsg.BusinessGroup
                      FROM crm.SalesLeadAudit sla
                      LEFT JOIN
                      (SELECT slbg.SalesLeadId, bsg.Name BusinessGroup
                      FROM crm.SalesLeadBusinessGroup slbg
                      INNER JOIN
                      crm.BusinessGroup bg
                      ON bg.BusinessGroupId = slbg.BusinessGroupId
                      INNER JOIN
                      crm.BusinessSuperGroup bsg
                      ON bsg.BusinessSuperGroupId = bg.BusinessSuperGroupId) bsg
                      ON bsg.SalesLeadId = sla.SalesLeadId")
text_data$Project <- as.character(text_data$Project)
text_data$BusinessGroup <- as.character(text_data$BusinessGroup)
# Text not meaningful for analysis
data(stop_words)

# Get rid of unnamed projects
text_data %>% 
  filter(tolower(Project) != "unnamed project") %>% 
  filter(Project != "") -> text_data

# Text data in tidy format
text_data %>% 
  tidytext::unnest_tokens(word, Project) -> tidy_data

# Get rid of stop words in the data frame
tidy_data <- tidy_data %>% 
  anti_join(stop_words, by = "word")

# Visualize most common words
tidy_data %>% 
  filter(tolower(BusinessGroup) != "custom machine") %>% 
  group_by(BusinessGroup) %>% 
  count(word) %>%
  filter(n > 30) %>%
  arrange(desc(n), .by_group = TRUE) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip() +
  facet_grid(BusinessGroup ~ ., scales = "free_y")

