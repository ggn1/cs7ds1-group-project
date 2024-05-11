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


create_tree <- function(
    data, level=0, TOTAL_N_ROWS=TOTAL_N_ROWS, min_split_pc=0.05, max_depth=-1
) {
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
  
  # INVALID NODE
  # If no model could fit this data, then this 
  # is an invalid leaf that will be pruned.
  if(model_type == 'none') {
    # Return invalid node.
    node <- hash()
    node[["type"]] <- "invalid"
    return(node)
  }
  
  # TERMINAL NODE 
  # Stop growing if this node has < 5% of
  # the data points in the original data set
  # or if all values in the response variable
  # are the same. This is a valid leaf.
  if (
    nrow(data) < (min_split_pc * TOTAL_N_ROWS) ||
    length(unique(data[[RESPONSE_VARIABLE]])) == 1 ||
    (max_depth >= 0 && level >= max_depth)
  ) {
    # Return terminal node.
    node <- hash()
    node[["type"]] <- "terminal"
    node[['data']] <- data
    node[['model_type']] <- model_type
    node[['has_left_child']] <- FALSE
    node[['has_right_child']] <- FALSE
    data_sanitized <- sanitize_fit_input(
      data_check = data,
      data_apply = list(data),
      response_variable = RESPONSE_VARIABLE
    )
    data2fit <- data_sanitized[[1]]
    m <- fit_model(
      model_type = model_type,
      data = data2fit,
      response_variable = RESPONSE_VARIABLE
    )
    node[['model']] <- m
    node[['features']] <- names(data2fit)
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
  
  # 6. Recursively call the create tree
  #    function with data from both child
  #    nodes to build the next level of the tree.
  left_child <- create_tree(
    data = data_split[['left']], 
    level=level+1,
    TOTAL_N_ROWS = TOTAL_N_ROWS,
    max_depth = max_depth,
    min_split_pc = min_split_pc
  )
  right_child <- create_tree(
    data = data_split[['right']], 
    level=level+1,
    TOTAL_N_ROWS = TOTAL_N_ROWS,
    max_depth = max_depth,
    min_split_pc = min_split_pc
  )
  
  # INTERMEDIATE (POSSIBLY TERMINAL) NODE
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
    node[['has_left_child']] == FALSE ||
    node[['has_right_child']] == FALSE
  ) {
    node[['left_left_child']] == FALSE
    node[['has_right_child']] == FALSE
    if ("left_child" %in% keys(node)) {
      del("left_child", node)
    }
    if ("right_child" %in% keys(node)) {
      del("right_child", node)
    }
    node[['type']] <- "terminal"
    node[['data']] <- data
  }
  data_sanitized <- sanitize_fit_input(
    data_check = data,
    data_apply = list(data),
    response_variable = RESPONSE_VARIABLE
  )
  data2fit <- data_sanitized[[1]]
  m <- fit_model(
    model_type = model_type,
    data = data2fit,
    response_variable = RESPONSE_VARIABLE
  )
  node[['model']] <- m
  node[['features']] <- names(data2fit)
  return(node)
}