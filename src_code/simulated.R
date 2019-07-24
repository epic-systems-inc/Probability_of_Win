# This file is for simulating data for our test, which is found in tests.R
# The test is for checking that the recursive decision tree built in association_rule_mining.R
# is producing expected results

library(data.table)

# Our data is binary with the possibility of being missing
alternate_between <- c(0, 1, NA)

set.seed(123)

fake_data <- data.frame("Q1" = NA, "Q2" = NA, 'Q3' = NA, "Q4" = NA, "Q5" = NA, "Q6" = NA)
for(i in 1:30){
  observation <- sample(alternate_between, size = 6, replace = TRUE, prob = c(0.3, 0.5, 0.2))
  for(j in 1:6){
    fake_data[i,j] <- observation[j]
  }
}

fake_rules <- list()
# Want 2 fake rules per variable
for(i in 1:6){
  rule_set = c()
  consequent = c()
  for(j in 1:2){
    sampled <- sample(c(1:6)[!c(1:6) %in% i], size = 1)
    rule_set <- append(rule_set, sprintf("{Q%s=%s,Q%s=%s}", 
                                         sampled, 
                                         sample(c(1,0), size = 1), 
                                         sample(c(1:6)[!c(1:6) %in% c(i, sampled)], size = 1), 
                                         sample(c(1,0), size = 1))
                       )
    consequent <- append(consequent, sprintf("{Q%s=1}", i))
    var <- sprintf("Q%s", i)
    fake_rules[[var]] <- data.table(lhs = rule_set, 
                             rhs = consequent
                             )
  }
}
