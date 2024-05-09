library(data.table)
library(glmertree)
library(glmmTMB)
library(MASS)  

calculate_node_deviance <- function(split_variable, model_type, data_path = "C:/Users/10068/Desktop/Poison/data_clean.csv") {
  
  data <- fread(data_path)
  
  # detect the split variable is continuous or categorical
  if (is.numeric(data[[split_variable]]) || is.integer(data[[split_variable]])) {
    split_type <- "continuous"
  } else {
    data[[split_variable]] <- as.factor(data[[split_variable]])
    split_type <- "categorical"
  }
  
  
  fit_model <- function(data, formula) {
    if (model_type == "Poisson model") {
      return(glm(formula, data = data, family = poisson()))
    } else if (model_type == "Negative binomial model") {
      return(glm.nb(formula, data = data))
    } else if (model_type == "Hurdle model") {
      return(glmmTMB(formula, data = data, family = nbinom2))
    } else if (model_type == "Zero-inflated model") {
      return(glmmTMB(formula, data = data, family = list(family = "poisson", link = "log"), ziformula = ~ 1))
    } else {
      stop("Invalid model type")
    }
  }
  
  # Define the negative log-likelihood calculation
  node_deviance <- function(model) {
    return(-logLik(model))
  }
  
  # Perform all possible binary partitions and compute the deviations
  unique_values <- unique(data[[split_variable]])
  best_split <- NULL
  min_deviance <- Inf
  
  for (value in unique_values) {
    if (split_type == "continuous") {
      left_data <- data[data[[split_variable]] <= value,]
      right_data <- data[data[[split_variable]] > value,]
    } else {
      left_data <- data[data[[split_variable]] == value,]
      right_data <- data[data[[split_variable]] != value,]
    }
    
    # Prints information about each attempted split
    print(paste("Testing split at:", value))
    print(paste("Left count:", nrow(left_data), "Right count:", nrow(right_data)))
    
    # Verify the validity of each subset
    if (nrow(left_data) > 0 && nrow(right_data) > 0 && 
        length(unique(left_data[[split_variable]])) > 1 && length(unique(right_data[[split_variable]])) > 1) {
      
      # fit the model
      left_model <- fit_model(left_data, as.formula(paste0("absences ~ ", split_variable)))
      right_model <- fit_model(right_data, as.formula(paste0("absences ~ ", split_variable)))
      
      # calculate the deviance and print it
      total_deviance <- node_deviance(left_model) + node_deviance(right_model)
      print(paste("Total deviance for split at", value, ":", total_deviance))
      
      # Find the minimum deviance
      if (total_deviance < min_deviance) {
        min_deviance <- total_deviance
        best_split <- value
      }
    } else {
      print("Invalid split due to insufficient categories in one or both subsets.")
    }
  }
  
  
  return(best_split = best_split)
}

# Test
result <- calculate_node_deviance("Mjob", "Hurdle model")
print(result)