# LOAD PACKAGES
require(pscl) # Zero inflated poisson model.
require(MASS) # Poisson and negative binomial models.
require(caret) # For cross validation.

# AUTOMATICALLY SET WORKING DIRECTORY
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print( getwd() )

# HELPER FUNCTIONS
source("./helper_functions.R")

cv10fold <- function(
    model_type, data, response_variable
) {
  ### Returns average MSE after 10 fold cross validation.
  ### @param response_variable: Variable to predict.
  ### @param model_type: Code of the type of 
  ###                    model to use.
  ### @param data: Data to perform 10 fold CV using.
  ### @return: Average of MSE values across 10 folds.
  folds <- 10
  n <- nrow(data)
  fold_indices <- cut(
    1:n, breaks = folds, 
    labels = FALSE
  )
  
  # Initialize for accumulating errors.
  total_error <- 0
  
  for (i in 1:folds) {
    fold_train <- data[fold_indices != i,]
    fold_validate <- data[fold_indices == i,]
    
    data_sanitized <- sanitize_fit_input(
      data_check = fold_train,
      data_apply = list(fold_train, fold_validate),
      response_variable = response_variable
    )
    fold_train <- data_sanitized[[1]]
    fold_validate <- data_sanitized[[2]]
    
    # Fit model on train set.
    m <- fit_model(
      model_type = model_type,
      data = fold_train,
      response_variable = response_variable
    )
    
    # Make predictions on validation set.
    y <- fold_validate[[response_variable]]
    y_pred <- predict(
      m, newdata = fold_validate, 
      type = "response"
    )
    
    # Calculate squared error.
    error <- sum((y - y_pred)^2)
    total_error <- total_error + error
  }
  
  # Calculate and return mean squared error (MSE).
  mse <- total_error / n
  return(mse)
}

# MODEL SELECTION
get_best_model <- function(data, response_variable) {
  ### Returns best model. The best model is the
  ### one that resulted in lowest MSE after
  ### 10 fold cross validation (CV).
  ### @param data: Input data set.
  ### @param response_variable: Variable to predict.
  ### @return: Code indicating best model so identified.
  print('Determining best model through 10 Fold CV ...')
  model_types <- list(
    'p', 'nb', 'zip', 'zinb', 'hp', 'hnb'
  )
  res_cv <- list()
  for (model_type in model_types) {
    mse <- tryCatch({cv10fold(
      model_type = model_type, data = data, 
      response_variable = response_variable
    )}, error = function(e) {
      print(paste(
        "Model", model_type, "cannot be fit.",
        "Thus, no longer a candidate for best model."
      ))
      return(NA)
    })
    res_cv <- append(res_cv, mse)
    print(paste(
      'Evaluated model:', model_type,
      '(mse =', mse, ")" 
    ))
  }
  if(is.na(min(unlist(res_cv), na.rm=T))){
    model_best <- NULL
  }
  else{
    model_best <- model_types[which.min(res_cv)]
  }
  print(paste(
    'Model resulting in lowest avg.',
    'MSE across 10 folds =', model_best
  ))
  return(model_best)
}

# MAIN

# # Load data.
# school_absences <- read.csv(
#   "../Data/data_clean.csv",
#   header=TRUE, sep=","
# )
# response_variable <- "absences"
# 
# # Get best model.
# model_type <- get_best_model(
#   data = school_absences,
#   response_variable = response_variable
# )
