library(data.table)
library(tidyverse)

check_rules <- function(rules, check, var_name, vec){
  
  if(length(check) > 0){
    
    # Set the variable with the NA as the root node in the decision tree
    root_node <- var_name
    
    # Set a variable for keeping track of whether or not the rule list for the particular variable has been exhausted
    rule_list_exhausted <- length(rules[[root_node]])
    
    print(paste0("root node: ", root_node))
    
    # Enter the rules list for the node and check through rule sets iteratively
    for(rl_index in 1:nrow(rules[[root_node]])){
      
      rule <- rules[[root_node]][rl_index]
      
      rule_split <- strsplit(rule$lhs, ",")[[1]]
      
      rule_set_names <- gsub("[{}]|=\\d|=\\d\\d", "", rule_split) # vector of the variables to be checked in the rule
      rule_set_values <- as.numeric(gsub("[{}]|^.*\\=", "", rule_split)) # vector of values
      
      consequent <- gsub("[{}]|=\\d|=\\d\\d", "", rule$rhs)
      # The value we want to assign if one of our rules are satisfied
      consequent_value <- as.numeric(gsub("[{}]|^.*\\=", "", rule$rhs))
      
      print(paste0("rules_index: ", rl_index))
      
      # If any of the rules we are checking are in the current rule set, we move on to the next rule set in the list
      # to avoid getting caught in a never-terminating recursive loop
      if(any(check %in% rule_set_names)){
        # If also we are on the last index within the rules list for this variable, consider the search exhausted
        # and assign the consequent a value of zero
        if(rule_list_exhausted == rl_index){
          vec[consequent] <- 0
          checking <- check[!check %in% consequent]
          check_rules(rules = rules, check = checking, var_name = checking[length(checking)], vec = vec)
        }
        else{
          next
        }
      }
      
      satisfied <- c() # Initialize vector to store rules that have been satisfied
      
      # For every rule in the rule set, check if the condition is satisfied or not.
      # If not, it's either bc it's missing (in which case, check the rules list of the missing value)
      # or it is the complement of the condition, in which case, move to the next rule
      for(rs_index in 1:length(rule_set_names)){
        var <- rule_set_names[rs_index]  # The variable associated with the i_th rule in the rule set at rule_index
        value <- rule_set_values[rs_index]  # The corresponding value
        
        print(paste0(c("we are checking: ", check), collapse = " "))
        print(paste0(c("The vector is: ", vec), collapse=" "))
        if(is.na(vec[var])){
          checking <- append(check, var)
          # If the rule we want to check is NA, search its rules and try to determine a value
          check_rules(rules = rules, check = checking, var_name = checking[length(checking)], vec = vec)
        }
        # If our particular rule is satisfied (e.g. Q3=1),
        # append the name of the variable (Q3) to our satisfied vector, remove it from checking
        # and continue on to the next rule in this rule set
        else if(vec[var] == value){
          satisfied <- append(satisfied, var)
          checking <- check[!check %in% var]
          
          if(length(satisfied) == length(rule_set_names)){
            vec[consequent] <- consequent_value
            check_rules(rules = rules, check = checking, var_name = checking[length(checking)], vec = vec)
          }
          else{
            next
          }
        }
        else{
          # If we are at the last rule in the set and the last rule set in the list, accept defeat
          if(rs_index == length(rule_set_names) & rule_list_exhausted == rl_index){
            vec[consequent] <- 0
            checking <- check[!check %in% consequent]
            check_rules(rules = rules, check = checking, var_name = checking[length(checking)], vec = new_vec)
          }
          #check <- check[!check %in% var]
          next # If the rule is not satisfied (reality is the complement), break out of this loop and check the next rule
        }
        
      }
      
    }
    
  }
  
  return(vec)
}

