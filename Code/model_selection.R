# LOAD PACKAGES
require(pscl) # Zero inflated poisson model.
require(MASS) # Poisson and negative binomial models.
require(caret) # For cross validation.

# HELPER FUNCTIONS
fit_model <- function(
    model_type, data, response_variable
) {
  ### Given a model type, fits it to the data.  
  ### @param response_variable: Name of response variable
  ###                           in given data.
  ### @param data: Data which the model with fit.
  ### @param model_type: The type of model to be
  ###                    fitted. Options are as follows.
  ###                    * p (poisson)
  ###                    * zip (zero inflated poisson zip)
  ###                    * nb (negative binomial)
  ###                    * zinb (zero inflated negative binomial)
  ###                    * hp (hurdle poisson)
  ###                    * hnb (hurdle negative binomial)
  ### @return m: Fitted model.
  m = NA
  formula_str <- paste(response_variable, " ~ .")
  # POISSON MODEL
  if (model_type == 'p') {
    m <- glm(
      as.formula(formula_str), 
      data=data, 
      family="poisson"
    )
  } 
  # NEGATIVE BINOMIAL MODEL
  else if (model_type == 'nb') {
    m <- glm.nb(
      as.formula(formula_str), 
      data=data
    )
  } 
  # ZERO INFLATED POISSON MODEL
  else if (model_type == 'zip') { 
    m <- zeroinfl(
      as.formula(formula_str), 
      data=data, 
      dist="poisson"
    )
  } 
  # ZERO INFLATED NEGATIVE BINOMIAL MODEL
  else if (model_type == 'zinb') { 
    m <- zeroinfl(
      as.formula(formula_str), 
      data=data, 
      dist="negbin"
    )
  } 
  # POISSON HURDLE MODEL
  else if (model_type == 'hp') {
    m <- hurdle(
      as.formula(formula_str), 
      data=data, 
      dist="poisson"
    )
  } 
  # NEGATIVE BINOMIAL HURDLE MODEL
  else if (model_type == 'hnb') {
    m <- hurdle(
      as.formula(formula_str), 
      data=data,
      dist="negbin"
    )
  } 
  else {
    stop("Invalid model type.")
  }
  return(m)
}

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
  print(paste(model_type, mse))
  
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
    res_cv <- append(res_cv, cv10fold(
      model_type = model_type, data = data, 
      response_variable = response_variable
    ))
    print(paste('Evaluated model:', model_type))
  }
  model_best <- model_types[which.min(res_cv)]
  print(paste(
    'Model resulting in lowest avg.',
    'MSE across 10 folds =', model_best
  ))
  return(model_best)
}

# MAIN

# Set working directory to this one.
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

# Load data.
school_absences <- read.csv(
  "../Data/data_clean.csv", 
  header=TRUE, sep=","
)
response_variable <- "absences"

# Get best model.
model_type <- get_best_model(
  data = school_absences,
  response_variable = response_variable
)

data <- school_absences
data$Mjob <- rep("teacher", length(data$Mjob))

# 80/20 Train/Test split.
index_train <- createDataPartition(
  school_absences[response_variable][,], 
  p = 0.8, list = FALSE
)
data_train <- school_absences[index_train, ]
data_test <- school_absences[-index_train, ]

model_type <- get_best_model(
  data = data_train,
  response_variable = response_variable
)

# m <- glm(
#   as.formula("absences ~ ."), 
#   data=data, 
#   family="poisson"
# )
