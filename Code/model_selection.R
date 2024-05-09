# LOAD PACKAGES
require(pscl) # Zero inflated poisson model.
require(MASS) # Poisson and negative binomial models.
require(caret) # For cross validation.

# HELPER FUNCTIONS
mse <- function(y, y_pred) {
  # Computes mean squared error.
  # @param y: Truth.
  # @param y_pred: Predictions.
  # @return: Mean of squared residuals.
  residuals <- y - y_pred
  squared_error <- residuals^2
  mse <- mean(squared_error)
  return(mse)
}

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
  res_cv = mean(as.numeric(lapply(
    folds, function(index_validate) {
      fold_train <- data[-index_validate, ]
      fold_validate <- data[index_validate, ]
      m <- fit_model(
        model_type = model_type,
        data = fold_train,
        response_variable = response_variable
      )
      y <- fold_validate[response_variable][,]
      y_pred <- predict(
        m, newdata=fold_validate, 
        type = "response"
      )
      error <- mse(y, y_pred)
      return(error)
    }
  ))) # Cross validation result.
  
  return(res_cv)
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


