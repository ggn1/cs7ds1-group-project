# LOAD PACKAGES
require(hash) # For hash map data structure.
require(pscl) # Zero inflated poisson model.
require(MASS) # Poisson and negative binomial models.
require(DescTools) # For Gini Index.

# SET WORKING DIRECTORY
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

# HELPER FUNCTIONS
source("./helper_functions.R")

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

get_split_deviance_gini <- function(
    responses_parent, responses_left, responses_right
) {
  ### Computes weighted average Gini impurity of
  ### the split condition as follows.
  ### Weighted Gini(children) = 
  ###       (n_left / n_total) * Gini(left) 
  ###       + (n_right / n_total) * Gini(right)
  ### Deviance = Gini(parent) - Weighted Gini(children)
  ### @param responses_parent: Responses before split.
  ### @param responses_left: Responses in the 
  ###                        left branch.
  ### @param responses_right: Responses in the 
  ###                         right branch.
  n_total <- length(responses_parent)
  n_left <- length(responses_left)
  n_right <- length(responses_right)
  gini_parent <- Gini(responses_parent)
  gini_left <- Gini(responses_left)
  gini_right <- Gini(responses_right)
  weighted_gini_children <- (
    ((n_left/n_total) * gini_left) + 
    ((n_right/n_total) * gini_right) 
  )
  return(gini_parent - weighted_gini_children)
}

get_split_deviance_mse <- function(
    responses_parent, responses_left, responses_right,
    truth_parent, truth_left, truth_right
) {
  ### Computes weighted MSE of the split condition as follows.
  ### Weighted MSE(children) = 
  ###       (n_left / n_total) * MSE(left) 
  ###       + (n_right / n_total) * MSE(right)
  ### Deviance = MSE(parent) - Weighted MSE(children)
  ### @param responses_parent: Responses before split.
  ### @param responses_left: Responses in the 
  ###                        left branch.
  ### @param responses_right: Responses in the 
  ###                         right branch.
  # n_total <- length(responses_parent)
  # n_left <- length(responses_left)
  # n_right <- length(responses_right)
  # gini_parent <- Gini(responses_parent)
  # gini_left <- Gini(responses_left)
  # gini_right <- Gini(responses_right)
  # weighted_gini_children <- (
  #   ((n_left/n_total) * gini_left) + 
  #     ((n_right/n_total) * gini_right) 
  # )
  # return(gini_parent - weighted_gini_children)
  # TO DO ...
}

# SPLIT SET SELECTION
get_split_set <- function(
    split_variable, data, response_variable
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
        next # Disregard this set if can't consider.
      }
      
      # [cat] 2.3. Compute and keep track of 
      #            set split deviance.
      split_set_deviance[[
        var_class
      ]] <- get_split_deviance_gini(
        responses_parent = data[[response_variable]],
        responses_left = data_split[['left']][[
          response_variable
        ]],
        responses_right = data_split[['right']][[
          response_variable
        ]]
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
      # ]] <- get_split_deviance(
      #   data_split, response_variable, model_type,
      #   saturated_model_log_likelihood
      # )
      
      split_set_deviance[[
        as.character(var_class)
      ]] <- get_split_deviance_gini(
        responses_parent = data[[response_variable]],
        responses_left = data_split[['left']][[
          response_variable
        ]],
        responses_right = data_split[['right']][[
          response_variable
        ]]
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
#   split_variable = 'famsize',
#   data = school_absences,
#   response_variable = 'absences'
# )


