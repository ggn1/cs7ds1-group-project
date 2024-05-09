# Import libraries.
library(rpart)
library(dplyr)
library(ggplot2) # CART
library(partykit) # CART
library(pscl) # Zero inflated poisson model.
library(MASS) # Poisson and negative binomial models.
library(caret) # For cross validation.

### Utility Functions
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

cv10fold <- function(model_type, data) {
  # Returns average MSE after 10 fold cross validation.
  # @param model_type: Name of the type of model to use.
  # @param data: Data to perform 10 fold cv using.
  # @return: Average of MSE values across 10 folds.
  res_cv = mean(as.numeric(lapply(folds, function(index_validate) {
    fold_train <- data_train[-index_validate, ]
    fold_validate <- data_train[index_validate, ]
    if (model_type == 'poisson') {
      model <- glm(Total.Uninjured ~ ., data=fold_train, family="poisson")
    } else if (model_type == 'negbin') {
      model <- glm.nb(Total.Uninjured ~ ., data=fold_train)
    } else if (model_type == 'zip') {
      model <- zeroinfl(Total.Uninjured ~ ., data=fold_train, dist="poisson")
    } else if (model_type == 'znib') {
      model <- zeroinfl(Total.Uninjured ~ ., data=fold_train, dist="negbin")
    } else if (model_type == 'hurdle_p') {
      model <- zeroinfl(Total.Uninjured ~ ., data=fold_train, dist="negbin")
    } else if (model_type == 'hurdle_negbin') {
      model <- hurdle(Total.Uninjured ~ ., data = data, dist = "negbin")
    } else { # Invalid model.
      return (-1)
    }
    y <- fold_train$Total.Uninjured
    y_pred <- predict(model, newdata=fold_train, type="response")
    error <- mse(y, y_pred)
    return(error)
  }))) # Cross validation result.
  return(res_cv)
}

### Main

# Set working directory to this one.
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

# Load data.
data <- read.csv("../Data/data_clean.csv", header=TRUE, sep=",")

# 1. Poisson Model
model_poisson <- glm(Total.Uninjured ~ ., data=data, family="poisson")

# 2. Negative Binomial Model
model_negbin <- glm.nb(Total.Uninjured ~ ., data=data)

# 3. Zero-Inflated Models
model_zip <- zeroinfl(Total.Uninjured ~ ., data=data, dist="poisson")
model_zinb <- zeroinfl(Total.Uninjured ~ ., data=data, dist="negbin")

# 4. Hurdle Model
model_hurdle_p <- hurdle(Total.Uninjured ~ ., data=data, dist="poisson")
model_hurdle_nb <- hurdle(Total.Uninjured ~ ., data=data, dist="negbin")

### 10 Fold Cross validation.

# 1. Train - test split.
index_train <- createDataPartition(data$Total.Uninjured, p = 0.8, list = FALSE)
data_train <- data[index_train, ]
data_test <- data[-index_train, ]

# 2. Create 10 folds of the train data set.
get_best_model <- function(data) {
  print('Determining best model through 10 Fold CV.')
  model_types <- list('poisson', 'negbin', 'zip', 'zinb', 'hurdle_p', 'hurdle_negbin')
  res_cv <- list()
  for (model_type in model_types) {
    res_cv <- append(res_cv, cv10fold(model_type, data))
    print(paste('Evaluated model:', model_type))
  }
  model_best <- model_types[which.min(res_cv)]
  print(paste('Model resulting in lowest avg. MSE across 10 folds =', model_best))
  return(model_best)
}
best_model <- get_best_model()