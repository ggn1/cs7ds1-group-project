# LOAD PACKAGES
require(hash) # For hash map data structure.
require(rstudioapi) # For dynamic setwd(...)

# SET WORKING DIRECTORY
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print(getwd())

# HELPER FUNCTIONS
source("./helper_functions.R")
source("./model_selection.R")
source("./split_variable_selection.R")
source("./split_set_selection.R")


create_tree <- function(data, level=0, TOTAL_N_ROWS=TOTAL_N_ROWS, min_split_pc=0.05, max_depth=-1) {

  ### A function that recursively builds a decision tree.
  ### @param data: Data set using which to build the tree.
  ### @param RESPONSE_VARIABLE: The variable that is to be
  ###                           predicted.
  ### @param min_split_pc: The minimum percent of the original
  ###                      no. of data points that can be in
  ###                      the parent node to allow for
  ###                      splitting.
  ### @param max_depth: The maximum no. of levels that the
  ###                   are allowed.
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

  
  # 1. Model selection.
  model_type <- get_best_model(
    data = data,
    response_variable = RESPONSE_VARIABLE
  )
  
  # 2. Check for stopping condition.
  
  # If no model could fit this data, then this 
  # is an invalid leaf that will be pruned.
  if(model_type == 'none' || (max_depth >= 0 && level > max_depth)) {
    # Return terminal node.
    node <- hash()
    node[["type"]] <- "invalid"
    node[['data']] <- data
    return(node)
  }
  
  # Stop growing if this node has < 5% of
  # the data points in the original data set
  # or if all values in the response variable
  # are the same. This is a valid leaf.
  if (
    nrow(data) < (min_split_pc * TOTAL_N_ROWS) ||
    length(unique(data[[RESPONSE_VARIABLE]])) == 1 ||
    (max_depth >= 0 && level == max_depth)
  ) {
    
    # Drop factors that only have one level
    cols_unique <- sapply(data, function(x) length(unique(x))) > 1
    data <- data[names(cols_unique[cols_unique])]
    
    # Fit model for terminal node
    model <- fit_model(
      model_type = model_type, 
      data = data, 
      response_variable = RESPONSE_VARIABLE
    )
      
    # Return terminal node.
    node <- hash()
    node[["type"]] <- "terminal"
    node[['data']] <- data
    node[['model_type']] <- model_type
    node[['model']] <- model
    node[['has_left_child']] <- FALSE
    node[['has_right_child']] <- FALSE
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
    model_type = model_type,
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
  #data_split <- do_split_data(data, split_variable, split_set)

  # 6. Recursively call the create tree
  #    function with data from both child
  #    nodes to build the next level of the tree.
  left_child <- create_tree(
    data = data_split[['left']], 
    level=level+1,
    TOTAL_N_ROWS,
    min_split_pc=min_split_pc, 
    max_depth=min_split_pc
  )
  right_child <- create_tree(
    data = data_split[['right']], 
    level=level+1,
    TOTAL_N_ROWS,
    min_split_pc=min_split_pc, 
    max_depth=min_split_pc
  )
  
  # Return intermediate node.
  node <- hash()
  node[['type']] <- "intermediate"
  node[['split_variable']] <- split_variable
  node[['split_set']] <- split_set
  node[['model_type']] <- model_type
  if (!left_child[['type']] == 'invalid') {
    node[['left_child']] <- left_child
    node[['has_left_child']] <- TRUE
  } else {
    node[['has_left_child']] <- FALSE
  }
  if (!right_child[['type']] == 'invalid') {
    node[['right_child']] <- right_child
    node[['has_right_child']] <- TRUE
  } else {
    node[['has_right_child']] <- FALSE
  }
  if (
    node[['has_left_child']] == FALSE &&
    node[['has_right_child']] == FALSE
  ) {
    node[['type']] <- "terminal"
    
    # Drop factors that only have one level
    cols_unique <- sapply(data, function(x) length(unique(x))) > 1
    data <- data[names(cols_unique[cols_unique])]
    
    # Fit model for terminal node
    model <- fit_model(
      model_type = model_type, 
      data = data, 
      response_variable = RESPONSE_VARIABLE
    )
    node[['data']] <- data
    node[['model_type']] <- model_type
    node[['model']] <- model
  }
  return(node)
}

# MAIN

## Load data.
#school_absences <- read.csv(
#  "../Data/data_clean.csv",
#  header=TRUE, sep=","
#)
#RESPONSE_VARIABLE <- "absences"
#
## Shuffle
#set.seed(32)
#shuffled_data <- school_absences[
#  sample(nrow(school_absences)), 
#]
#
## Reset index.
#rownames(shuffled_data) <- NULL

## 80/20 train/test split.
#index_train <- createDataPartition(
#  shuffled_data[RESPONSE_VARIABLE][,], 
#  p = 0.8, list = FALSE
#)
#data_train <- shuffled_data[index_train, ]
#data_test <- shuffled_data[-index_train, ]


## Sanitize data for model fitting.
#data_sanitized <- sanitize_fit_input(
#  data_check = data_train,
#  data_apply = list(data_train, data_test),
#  response_variable = RESPONSE_VARIABLE
#)
#data_train <- data_sanitized[[1]]
#data_test <- data_sanitized[[2]]
#
## Build tree.
#TOTAL_N_ROWS <- nrow(data_train) # global variable.
#root <- create_tree(
#  data = data_train,
#  min_split_pc = 0.3
#)


