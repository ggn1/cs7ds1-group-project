# LOAD PACKAGES
require(hash) # For hash map data structure.

# SET WORKING DIRECTORY
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print( getwd() )

# HELPER FUNCTIONS
source("./helper_functions.R")
source("./model_selection.R")
source("./split_variable_selection.R")
source("./split_set_selection.R")

create_tree <- function(data, level=0) {
  ### A function that recursively builds a decision tree.
  ### @param data: Data set using which to build the tree.
  ### @param RESPONSE_VARIABLE: The variable that is to be
  ###                           predicted.
  ### @return: Terminal or intermediate nodes of the tree
  ###          with information such as given below.
  ###          Here, $ => for intermediate node only and
  ###                £ => for terminal node only.
  ###          * Node type 
  ###          * Data £
  ###          * Model type $
  ###          * Split variable $
  ###          * Split set $
  ###          * Left child $
  ###          * Right child $
  
  print(paste(
    "Building Tree ( level", level, ")"
  ))
  
  # 2. Model selection.
  model_type <- get_best_model(
    data = data,
    response_variable = RESPONSE_VARIABLE
  )
  
  # 1. Check for stopping condition.
  # Stop growing if this node has < 5% of
  # the data points in the original data set
  # or if all values in the response variable
  # are the same.
  
  if (
    nrow(data) <= (0.05 * TOTAL_N_ROWS) ||
    length(unique(data[[RESPONSE_VARIABLE]])) == 1
  ) {
    m <- fit_model(
      model_type = model_type,
      data = data,
      response_variable = response_variable
    )
    
    # Return terminal node.
    node <- hash()
    node[["type"]] <- "terminal"
    node[['data']] <- data
    return(node)
  }
  
  # 3. Split variable selection.
  split_variable <- get_split_variable(
    data = data,
    response_variable = RESPONSE_VARIABLE,
    model_type = model_type
  )
  
  # 4. Split set selection.
  split_set <- get_split_set(
    split_variable = split_variable,
    data = data,
    response_variable = RESPONSE_VARIABLE
  )
  
  # 5. Split as per best identified criterion.
  # Split the data as per split condition.
  split_var_dtype <- get_cat_con(data[[split_variable]])
  data_split <- hash()
  if (split_var_dtype == 'con') {
    data_split[['left']] <- data[
      data[[split_variable]] < split_set, 
      , drop = FALSE
    ]
    data_split[['right']] <- data[
      data[[split_variable]] >= split_set, 
      , drop = FALSE
    ]
  } else {
    data_split[['left']] <- data[
      data[[split_variable]] %in% split_set, 
      , drop = FALSE
    ]
    data_split[['right']] <- data[
      !data[[split_variable]] %in% split_set, 
      , drop = FALSE
    ]
  }

  # 6. Recursively call the create tree
  #    function with data from both child
  #    nodes to build the next level of the tree.
  left_child <- create_tree(
    data = data_split[['left']], 
    level=level+1
  )
  right_child <- create_tree(
    data = data_split[['right']], 
    level=level+1
  )
  
  # Return intermediate node.
  node <- hash()
  node[['type']] <- "intermediate"
  node[['split_variable']] <- split_variable
  node[['split_set']] <- split_set
  node[['model_type']] <- model_type
  node[['left_child']] <- left_child
  node[['right_child']] <- right_child
  return(node)
}

# MAIN

# Load data.
school_absences <- read.csv(
  "../Data/data_clean.csv",
  header=TRUE, sep=","
)
RESPONSE_VARIABLE <- "absences"

# Shuffle
set.seed(32)
shuffled_data <- school_absences[
  sample(nrow(school_absences)), 
]

# Reset index.
rownames(shuffled_data) <- NULL

# 80/20 train/test split.
index_train <- createDataPartition(
  shuffled_data[RESPONSE_VARIABLE][,], 
  p = 0.8, list = FALSE
)
data_train <- shuffled_data[index_train, ]
data_test <- shuffled_data[-index_train, ]

# Sanitize data for model fitting.
data_sanitized <- sanitize_fit_input(
  data_check = data_train,
  data_apply = list(data_train, data_test),
  response_variable = RESPONSE_VARIABLE
)
data_train <- data_sanitized[[1]]
data_test <- data_sanitized[[2]]

# Build tree.
TOTAL_N_ROWS <- nrow(data_train) # global variable.
root <- create_tree(data = data_train)


