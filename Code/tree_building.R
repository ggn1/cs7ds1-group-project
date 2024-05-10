# LOAD PACKAGES
require(hash) # For hash map data structure.

# SET WORKING DIRECTORY
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

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
  
  # 1. Check for stopping condition.
  # Stop growing if this node has < 5% of
  # the data points in the original data set
  # or if all values in the response variable
  # are the same.
  
  if (
    nrow(data) <= (0.05 * TOTAL_N_ROWS) ||
    length(unique(data[[RESPONSE_VARIABLE]])) == 1
  ) {
    # Return terminal node.
    return(list(
      "Terminal Node",
      data = data
    ))
  }
  
  # 2. Model selection.
  model_type <- get_best_model(
    data = data,
    response_variable = RESPONSE_VARIABLE
  )
  
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
  left_tree <- create_tree(
    data = data_split[['left']], 
    level=level+1
  )
  right_tree <- create_tree(
    data = data_split[['right']], 
    level=level+1
  )
  
  # Return intermediate node.
  return(list(
    "Intermediate Node", 
    split_variable = split_variable, 
    split_set = split_set, 
    model_type = model_type,
    left = left_tree, 
    right = right_tree
  ))
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
tree <- create_tree(data = data_train)

