# LOAD PACKAGES
require(hash) # For hash map data structure.
require(pscl) # Zero inflated poisson model.
require(MASS) # Poisson and negative binomial models.

# SET WORKING DIRECTORY
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

# HELPER FUNCTIONS
source("./helper_functions.R")

mae <- function(predictions, truth) {
  ### Computes and returns Mean Absolute Error (MAE).
  ### @param predictions: Some predictions.
  ### @param truth: Ground truth.
  ### @return: MAE = mean(abs(predictions - truth))
  return(mean(abs(predictions - truth)))
} 

can_consider_split <- function(
    responses_left, responses_right
) {
  ### Returns whether or not this split set is to be
  ### considered at all. If a split results in 
  ### all 0 counts of the response variable in one 
  ### branch and all non-zero response variable counts
  ### in the other branch, then this split is disregarded.
  ### @param responses_left: Counts of the response
  ###                        variable from the left branch.
  ### @param responses_right: Counts of the response
  ###                         variable from the right branch.
  ### @return: TRUE if this set can be considered and
  ###          FALSE otherwise.
  
  # As in papaer.
  if (
    (sum(responses_left) == 0) && (0 %in% responses_right) ||
    (sum(responses_right) == 0) && (0 %in% responses_left)
  ) {
    return(FALSE)
  }
  return(TRUE)
  
  # # Modified for zero inflated models.
  # if (
  #   (sum(responses_left) != 0) && 
  #   (sum(responses_right) != 0) &&
  #   (0 %in% responses_right) && 
  #   (0 %in% responses_left)
  # ) {
  #   return(TRUE)
  # }
  # return(FALSE)
}

get_split_deviance_mae <- function(
    model_type, response_variable, 
    data_parent, data_left, data_right
) {
  ### Computes weighted MAE of the split condition as follows.
  ### Weighted MAE(children) = 
  ###       (n_left / n_total) * MAE(left) 
  ###       + (n_right / n_total) * MAE(right)
  ### Deviance = MAE(parent) - Weighted MAE(children)
  ### @param model_type: Type of model used.
  ### @param response_variable: Variable to be predicted.
  ### @param data_parent: Data in the parent node.
  ### @param data_left: Data in the left child.
  ### @param data_right: Data in the right child.
  sanitized_data <- sanitize_fit_input(
    data_check = data_parent,
    data_apply = list(data_parent, data_left, data_right),
    response_variable = response_variable
  )
  data_parent <- sanitized_data[[1]]
  data_left <- sanitized_data[[2]]
  data_right <- sanitized_data[[3]]
  m <- fit_model(
    model_type = model_type,
    data = data_parent,
    response_variable = response_variable
  )
  pred_parent <- predict(m, newdata=data_parent)
  pred_left <- predict(m, newdata=data_left)
  pred_right <- predict(m, newdata=data_right)
  truth_parent <- data_parent[[response_variable]]
  truth_left <- data_left[[response_variable]]
  truth_right <- data_right[[response_variable]]

  mae_parent <- mae(pred_parent, truth_parent)
  mae_left <- mae(pred_left, truth_left)
  mae_right <- mae(pred_right, truth_right)

  n_total <- length(data_parent)
  n_left <- length(data_left)
  n_right <- length(data_right)
  
  weighted_mae_children <- (
      ((n_left/n_total) * mae_left) +
      ((n_right/n_total) * mae_right)
  )
  return(mae_parent - weighted_mae_children)
}

# SPLIT SET SELECTION
get_split_set <- function(
    split_variable, data, response_variable, model_type
) {
  ### Returns best criterion based on which to split
  ### values in this split variable.
  ### @param split_variable: Name of variable whose values 
  ###                        are the basis for the split.
  ### @param model_type: Type of model to use for the split.
  ### @param data: Data set.
  ### @param response_variable: The variable to be predicted.
  ### @return split_set: The split criterion. 
  print("Selecting split set ...")
  
  split_set <- NA
  
  # 1. Determine if selected variable is 
  #    categorical or continuous.
  var_values <- data[[split_variable]]
  var_classes <- unique(var_values)
  var_type <- get_cat_con(var_values)
  
  # 2. Get deviance scores for all possible 
  #    binary splits of the data.
  split_set_deviance <- hash()
  if (var_type == "cat") {
    # [cat] 2.1. Loop over every possible binary split.
    #            and obtain data subset corresponding to 
    #            left (1) and right (0) branches.
    for (var_class in var_classes) {
      bool_values <- rep(0, length(var_values))
      bool_values[var_values == var_class] = 1
      data_split <- hash()
      data_split[['left']] <- data[bool_values == 1, ]
      data_split[['right']] <- data[bool_values == 0, ]
      
      # [cat] 2.2. Consider or disregard this split. 
      #            If all response variables are 0
      #            in one branch and 1 in the other branch.
      #            Then, ignore this split.
      if (!can_consider_split(
        data_split[['left']][[response_variable]],
        data_split[['right']][[response_variable]]
      )) {
        next # Disregard this set if can't be considered.
      }
      
      # [cat] 2.3. Compute and keep track of 
      #            set split deviance.
      # split_set_deviance[[
      #   var_class
      # ]] <- get_split_deviance_gini(
      #   responses_parent = data[[response_variable]],
      #   responses_left = data_split[['left']][[
      #     response_variable
      #   ]],
      #   responses_right = data_split[['right']][[
      #     response_variable
      #   ]]
      # )
      split_set_deviance[[
        var_class
      ]] <- get_split_deviance_mae(
        model_type = model_type,
        response_variable = response_variable,
        data_parent = data,
        data_left = data_split[['left']],
        data_right = data_split[['right']]
      )
    }
  } else { # (variable_type == "con")
    # [con] 2.1. Loop over every possible binary split.
    #            and obtain data subset corresponding to 
    #            left (1) and right (0) branches.
    for (var_class in var_classes) {
      bool_values <- rep(0, length(var_values))
      bool_values[var_values >= var_class] = 1
      data_split <- hash()
      data_split[['left']] <- data[bool_values == 1, ]
      data_split[['right']] <- data[bool_values == 0, ]
      
      # [con] 2.2. Consider or disregard this split.
      #            If all response variables are 0
      #            in one branch and 1 in the other branch.
      #            Then, ignore this split.
      if (!can_consider_split(
        data_split[['left']][[response_variable]],
        data_split[['right']][[response_variable]]
      )) {
        next # Disregard this set if can't consider.
      }
      
      # [con] 2.3. Compute and keep track
      #            of set split deviance.
      
      # split_set_deviance[[
      #   as.character(var_class)
      # ]] <- get_split_deviance_gini(
      #   responses_parent = data[[response_variable]],
      #   responses_left = data_split[['left']][[
      #     response_variable
      #   ]],
      #   responses_right = data_split[['right']][[
      #     response_variable
      #   ]]
      # )
      
      split_set_deviance[[
        as.character(var_class)
      ]] <- get_split_deviance_mae(
        model_type = model_type,
        response_variable = response_variable,
        data_parent = data,
        data_left = data_split[['left']],
        data_right = data_split[['right']]
      )
    }
  }
  
  # 3. Get the the set corresponding to the max 
  #    split set deviance value.
  cur_max <- -Inf
  arg_max <- NA
  for (var_name in keys(split_set_deviance)) {
    var_deviance <- split_set_deviance[[var_name]]
    if (is.na(var_deviance)) { # Happens due to underflow.
      var_deviance <- 0
    }
    if (var_deviance > cur_max) {
      cur_max <- var_deviance
      arg_max <- var_name
    }
  }
  split_set <- arg_max
  if (var_type == 'con') {
    split_set <- as.numeric(arg_max)
  }
  
  print(paste(
    "Split set found. (", split_variable, 
    "=", split_set, ")"
  ))
  
  return(split_set)
}

# MAIN

# # Load data set.
# school_absences <- read.csv(
#   "../Data/data_clean.csv",
#   header=TRUE, sep=","
# )
# 
# # Split set selection.
# split_set <- get_split_set(
#   split_variable = "Fjob",
#   data = school_absences,
#   response_variable = 'absences',
#   model_type = 'zip'
# )


