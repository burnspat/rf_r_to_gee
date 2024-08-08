# SCRIPT METADATA ==========================================================================================
# Description: R script to read in ranger random forest model and format it for upload to Google Earth Engine
# Script Author: Kathleen Orndahl
# Script Date: 2023-05-03

# NOTE:

# Here, classification refers to a probability forest fit in the ranger package using the parameter probability=TRUE
# This is NOT the same thing as a classification forest, as each node and tree produces a probability estimate rather than a class vote
# See here for more information: https://stackoverflow.com/questions/62806074/how-to-get-the-same-prediction-probability-and-class-in-a-random-forest
# Reference for probability forests: Malley, J. D., Kruppa, J., Dasgupta, A., Malley, K. G., & Ziegler, A. (2012). Probability machines: consistent probability estimation using nonparametric learning machines. Methods Inf Med 51:74-81.

# SET UP ====================================================================================================

library(dplyr)
library(tidyr)
library(ranger)
library(reprtree)
source('reprtree_ranger.R')
options(scipen=999) # Ensure numbers are not represented in scientific notation

response_type = 'regression' # Choose 'classification' or 'regression'
response_var = 'prediction' # Specify name of response variable - for example dataset choose 'pred.presence' for classification and 'prediction' for regression
shortened = FALSE # Use shortened format?

mod = readRDS(paste0('mod_', response_type, '.rds')) # Get model
data = read.csv(paste0('data_', response_type, '.csv')) # Get training data

# Set output file name
if(shortened){
  tree_file = paste0(response_type, '_gee_short.txt')
}else{
  tree_file = paste0(response_type, '_gee.txt')
}

# Create output file
file.create(tree_file)

# LOOP TREES ====================================================================================================

# Format binary as regression trees
# Tidymodels set_mode('classification') sets probability = TRUE for ranger random forest
# i.e. it produces a probability forest
# https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.html
# https://stackoverflow.com/questions/62806074/how-to-get-the-same-prediction-probability-and-class-in-a-random-forest

for(i in 1:mod$num.trees){
  
  # Get tree
  tree = treeInfo(mod, tree = i)

  # FORMAT TREE ====================================================================================================
  
  tree_formatted = tree %>% mutate(prediction = !!as.name(response_var)) # Rename response variable
  tree_formatted = tree_formatted %>% select(-c('nodeID', 'splitvarID')) # Remove unnecessary columns
  tree_formatted$leftChild = tree_formatted$leftChild+1 # Fix child indexing
  tree_formatted$rightChild = tree_formatted$rightChild+1 # Fix child indexing
  tree_formatted = tree_formatted %>% mutate(terminal = if_else(terminal, -1, 1)) # Convert from TRUE/FALSE to -1/1
  tree_formatted = tree_formatted %>% mutate(leftChild = if_else(is.na(leftChild), 0, leftChild)) # Convert NAs to zeros
  tree_formatted = tree_formatted %>% mutate(rightChild = if_else(is.na(rightChild), 0, rightChild)) # Convert NAs to zeros
  tree_formatted = tree_formatted %>% mutate(splitval = if_else(is.na(splitval), 0, splitval)) # Convert NAs to zeros
  tree_formatted = tree_formatted %>% mutate(splitStat = if_else(is.na(splitStat), 0, splitStat)) # Convert NAs to zeros
  tree_formatted = tree_formatted %>% rename('left daughter' = 'leftChild', 'right daughter' = 'rightChild', 'split var' = 'splitvarName', 'split point' = 'splitval', 'status' = 'terminal') # Rename columns
  
  # CONVERT TREE ====================================================================================================
  
  if(shortened){
    tree_final = as.tree.ranger.short(gTree = tree_formatted, 
                                      rforest = mod, 
                                      training_data = data, 
                                      response_type = 'regression')
  }else{
    tree_final = as.tree.ranger(gTree = tree_formatted, 
                                rforest = mod, 
                                training_data = data, 
                                response_type = 'regression')
  }
  
  # WRITE TREE ====================================================================================================

  sink(tree_file, append = TRUE)
  print(tree_final)
  sink(append = TRUE)
  
  # Report progress
  print(paste0('Tree ', i, ' of ', mod$num.trees, ' completed'))
  
}

# TIDY FOREST ====================================================================================================
# Note: you might discover special characters unique to your model that are not read by GEE, if so use this section to remove them

tree_file_headers = readLines(tree_file)
tree_file_headers  = gsub(pattern = "node\\), split, n, deviance, yval", replace = "", x = tree_file_headers) # Remove header
tree_file_headers  = gsub(pattern = "      \\* denotes terminal node", replace = "", x = tree_file_headers) # Remove header
tree_file_headers  = gsub(pattern = "\\.(?![0-9])", replace = "", x = tree_file_headers, perl=TRUE) # Periods cannot be read by GEE, remove periods if not followed by a number
tree_file_headers = tree_file_headers[which(tree_file_headers!="")] # Remove blank lines

# Save
if(shortened){
  writeLines(tree_file_headers, con = paste0(response_type, '_formatted_gee_short.txt'))
}else{
  writeLines(tree_file_headers, con = paste0(response_type, '_formatted_gee.txt'))
}

# Clean up
if (file.exists(tree_file)) {
  file.remove(tree_file)
}
