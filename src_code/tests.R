source("simulated.R")
source("recursive_decision_tree.R")

# Add some edge-cases to the simulated data
fake_data <- rbind(fake_data, rep(NA, 6))

# Add expected results from the algorithm
expected <- list(c(1,0,1,1,0), 
                 c(0,0,0,1,1,1), 
                 c(0,0,1,0,1,1),
                 c(1,0,0,0,0,1),
                 c(0,0,0,0,1,1),
                 c(0,0,0,0,1,1),
                 c(0,1,1,1,1,1),
                 c(1,1,1,1,1,1),
                 c(1,0,1,1,0,1),
                 c(0,1,1,0,0,1),
                 c(0,1,1,1,0,1),
                 c(1,0,0,1,0,0),
                 c(0,1,1,1,1,0),
                 c(1,1,1,0,1,0),
                 c(1,1,0,0,0,1),
                 c(1,0,1,0,1,1),
                 c(0,1,1,0,0,1),
                 c(1,1,1,1,1,0),
                 c(1,1,0,1,1,1),
                 c(0,1,0,1,0,1),
                 c(0,1,1,1,1,0),
                 c(1,1,1,0,0,0),
                 c(0,0,0,0,1,0),
                 c(1,1,1,1,1,1),
                 c(0,1,1,1,1,0),
                 c(1,1,1,1,1,1),
                 c(0,1,1,1,0,1),
                 c(0,1,1,0,0,1),
                 c(1,1,0,1,1,0),
                 c(0,0,1,0,1,0),
                 c(0,0,1,0,0,1)
                )


test_vec <- fake_data[4,]

for(i in 1:length(test_vec)){
  
  print(test_vec[i])
  
  # Check all the variables for NAs
  if(is.na(test_vec[i])){
    # Set the variable with the NA as our root node in our tree
    root_node <- names(test_vec[i])
    
    # Begin the search process: check every rule for the root node until a match is found or the search
    # is exhausted. If another NA is encountered during the search process (while checking through the rules)
    # deal with it the same way - essentially creating a recursive search tree
    
    checking <- root_node # Initialize vector to store rules we are currently checking
    
    test_vec <- check_rules(rules = fake_rules, check = checking, var_name = root_node, vec = test_vec)
  }
}
