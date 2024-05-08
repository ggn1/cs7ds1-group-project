# Import libraries.
library(rpart) # For decision trees.
library(hash) # For hash map data structure.
library(dplyr)
library(ggplot2) # CART
library(partykit) # CART
library(pscl) # Zero inflated poisson model.
library(MASS) # Poisson and negative binomial models.
library(caret) # For cross validation.
library(readr)
library(stats)
library(pscl)
library(ggeffects)

# Set working directory to this one.
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

# UTILITY FUNCTIONS
get_cat_con <- function(data) {
  ### Computes whether each column in the data set is
  ### categorical or non-categorical in nature.
  ### @param data: Data table.
  ### @param target: Name of the target variable.
  ### @return: A list of pairs corresponding to 
  ###          all covariates (except target feature)
  ###          such that the first element in the pair
  ###          is the name of the feature and the second
  ###          element is the type of that feature.
  covariates <- list()
  for (covariate in names(data)) { # Check each covariate.
    if (covariate == "absences") next # Skip target variable.
    if (
      is.factor(data[[covariate]]) || 
      is.character(data[[covariate]])
    ) { # covariate is categorical
      covariates[[length(covariates)+1]] <- c(covariate, "cat")
    } else { # covariate is continuous
      covariates[[length(covariates)+1]] <- c(covariate, "con")
    }
  }
  return(covariates)
}

fit_model <- function(model_type, formula_str) {
  ### Given a model type, fits it to the data.  
  ### @param formula_str: The model formula as a string.
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
  if (model_type == 'p') { # POISSON MODEL
    m <- glm(
      as.formula(formula_str), 
      data=school_absences, 
      family="poisson"
    )
  } else if (model_type == 'nb') { # NEGATIVE BINOMIAL MODEL
    m <- glm.nb(
      as.formula(formula_str), 
      data=school_absences
    )
  } else if (model_type == 'zip') { # ZERO INFLATED POISSON MODEL
    m <- zeroinfl(
      as.formula(formula_str), 
      data=school_absences, 
      dist="poisson"
    )
  } else if (model_type == 'zinb') { # ZERO INFLATED NEGATIVE BINOMIAL MODEL
    m <- zeroinfl(
      as.formula(formula_str), 
      data=school_absences, 
      dist="negbin"
    )
  } else if (model_type == 'hp') { # POISSON HURDLE MODEL
    m <- hurdle(
      as.formula(formula_str), 
      data=school_absences, 
      dist="poisson"
    )
  } else if (model_type == 'hnb') { # NEGATIVE BINOMIAL HURDLE MODEL
    m <- hurdle(
      as.formula(formula_str), 
      data=school_absences,
      dist="negbin"
    )
  } else {
    stop("Invalid model type.")
  }
  return(m)
}

compute_partial_residuals <- function(m, model_type) {
  ### Computes and returns partial residuals.
  ### @param m: The model whose partial residuals
  ###           need to be obtained.
  ### @param model_type: The type of given model which
  ###                    could be any one of 
  ###                    [p, nb, zip, zinb, hp, hnb].
  ### @return partial_residuals: A hash map with 2 keys
  ###                            being "count" and "zero"
  ###                            such that "count" corresponds
  ###                            to a data frame containing
  ###                            count data related residuals
  ###                            and "zero" corresponds 
  ###                            to a data frame containing
  ###                            residuals related to zero
  ###                            counts if applicable.
  
  # Get the names of the predictor variables.
  predictor_names <- names(data)[-1]
  
  # Initialize a list to store partial residuals 
  # for each predictor.
  partial_residuals <- hash()
  partial_residuals[['count']] <- list()
  partial_residuals[['zero']] <- list()
  
  # Get truth values for school absences.
  truth_count <- school_absences$absences
  truth_zero <- as.numeric(
    ifelse(school_absences$absences == 0, 1, 0)
  )
  
  # Get residuals with full model.
  predictions_count_original <- predict(m, type="response")
  residuals_count_original <- (
    predictions_count_original - truth_count
  )
  
  predictions_zero_original <- list()
  residuals_zero_original <- list()
  if (
    model_type == "zip" || 
    model_type == "zinb" ||
    model_type == "hp" ||
    model_type == "hnb"
  ) {
    predictions_zero_original <- predict(m, type="zero")
    residuals_zero_original <- (
      predictions_zero_original - truth_zero
    )
  }
  
  # Iterate over each predictor variable.
  for (predictor in predictor_names) {
    
    # Create reduced model formula that 
    # forces the model to be consider all
    # predictors except the one we're currently
    # interested in.
    formula_reduced_str <- paste(
      "absences ~ . - ", predictor
    )
    
    # Fit the reduced model
    m_reduced <- fit_model(
      model_type = model_type,
      formula_str = formula_reduced_str
    )
    
    # Compute and store partial residuals of the 
    # count component.
    predictions_count_reduced <- predict(
      m_reduced, type="response"
    )
    residuals_count_reduced <- (
      predictions_count_reduced - truth_count
    )
    partial_residual_count <- (
      residuals_count_original - residuals_count_reduced
    )
    partial_residuals[['count']][[predictor]] <- partial_residual_count
    
    # Compute and store partial residuals of the 
    # zero component.
    if (length(residuals_zero_original) > 0) {
      predictions_zero_reduced = predict(
        m_reduced, type="zero"
      )
      residuals_zero_reduced = (
        predictions_zero_reduced - truth_zero
      )
      partial_residual_zero = (
        residuals_zero_original - residuals_zero_reduced
      )
      partial_residuals[['zero']][[predictor]] <- partial_residual_zero
    }
  }
  
  partial_residuals[['count']] <- data.frame(partial_residuals[['count']])
  partial_residuals[['zero']] <- data.frame(partial_residuals[['zero']])
  
  return(partial_residuals)
}

# LOAD DATA
school_absences <- read.csv(
  "../Data/data_clean.csv", 
  header=TRUE, sep=","
)
covariates <- get_cat_con(school_absences)

# SPLIT VARIABLE SELECTION
split_variable <- NA

# 1. Fit model [options = 
#     p (poisson), 
#     zip (zero inflated poisson zip),
#     nb (negative binomial), 
#     zinb (zero inflated negative binomial),
#     hp (hurdle poisson), 
#     hnb (hurdle negative binomial)
#    ]
model_type <- "hnb"
m <- fit_model(
  model_type = model_type, 
  formula_str = "absences ~ ."
)

# 2. Get partial score residuals.
# partial_residuals <- as.data.frame(
#   residuals(m, type = "partial")
# )
partial_residuals <- compute_partial_residuals(m, model_type)

# 3. Procedure 1
print("Performing procedure 1 ...")
procedure1_res <- hash()
for (covariate_name_type in covariates) {
  # Get covariate name and type.
  covariate_name <- covariate_name_type[[1]]
  covariate_type <- covariate_name_type[[2]]
  
  # 3.1. Get specific partial scores.
  part_res_count <- partial_residuals[['count']][covariate_name][,]
  part_res_zero <- list()
  if (length(partial_residuals[['zero']])>0) {
    part_res_zero <- partial_residuals[['zero']][covariate_name][,]
  }
  
  # 3.2. Group by signs of the residuals.
  #      0 => < 0
  #      1 => >= 0
  part_res_count_groups <- rep(
    1, times=length(part_res_count)
  )
  part_res_count_groups[
    which(part_res_count < 0)
  ] = 0
  part_res_count_groups <- factor(
    part_res_count_groups
  )
  part_res_zero_groups <- NA
  if (length(part_res_zero) > 0) {
    part_res_zero_groups <- rep(
      1, times=length(part_res_count)
    )
    part_res_zero_groups[which(part_res_zero < 0)] = 0
  }

  # 3.3. If the covariate is not categorical, 
  #      group by quartiles else, group by categories.
  covariate_data <- school_absences[covariate_name][,]
  covariate_data_groups <- NA
  if (covariate_type == "con") {
    five_num_sum <- fivenum(covariate_data)
    q1 <- five_num_sum[1]
    q2 <- five_num_sum[2]
    q3 <- five_num_sum[3]
    quartile_group <- rep(
      4, times=length(covariate_data)
    )
    quartile_group[which(covariate_data <= q3)] = 3
    quartile_group[which(covariate_data <= q2)] = 2
    quartile_group[which(covariate_data <= q1)] = 1
    covariate_data_groups <- factor(quartile_group)
  } else {
    covariate_data_groups <- factor(covariate_data)
  }
  
  # 3.4. Create a contingency table with
  #      residual signs as rows and groups 
  #      as columns.
  contingency_table = NA
  if ( # For normal count models.
    model_type == "p" ||
    model_type == "nb"
  ) {
    contingency_table <- table(
      part_res_count_groups,
      covariate_data_groups
    )
  } else { # For zero inflated or hurdle models.
    contingency_table <- table(
      paste0(
        part_res_count_groups, 
        part_res_zero_groups
      ), covariate_data_groups
    )
  }

  # 3.5. Drop entries where column total == 0.
  column_totals <- margin.table(
    contingency_table,
    margin = 2
  )
  non_zero_columns <- column_totals > 0
  contingency_table <- contingency_table[, non_zero_columns]

  # 3.6. Compute the Pearson chi-squared test
  #      statistic for independence.
  chi_squared_test <- chisq.test(contingency_table)
  deg_free <- chi_squared_test$parameter
  chi_squared <- chi_squared_test$statistic
  if (deg_free > 1) { # Wilson-Hilferty approximation.
    chi_squared <- max(
      0, (7/9) + sqrt(deg_free) * (
        (chi_squared / deg_free)^(1/3) - 1 + 2/(9*deg_free))
    )^3
  }

  # 3.7. Keep track of computed values for the future.
  procedure1_res[[covariate_name]] = chi_squared
}
print("Procedure 1 complete.")

# 4. Check if can select best feature now.
K <- length(school_absences) - 1 # No. of input features.
alpha1 <- 0.05/K
alpha2 <- 0.1/(K*(K-1))
chi_squared_df1_alpha1 <- qchisq(1 - alpha1, df = 1)
chi_squared_df1_alpha2 <- qchisq(1 - alpha2, df = 1)
#    Check if there exists a variable now,
#    that matches this criterion.
print("Checking for good split variable ...")
condition_met <- hash()
cur_max <- -Inf
arg_max <- NA
for (covariate in keys(procedure1_res)) {
  chi_squared <- procedure1_res[[covariate]]
  if (chi_squared > chi_squared_df1_alpha1) {
    condition_met[[covariate]] <- chi_squared
    if (chi_squared > cur_max) {
      arg_max <- covariate
      cur_max <- chi_squared
    }
  }
}
print("Check complete.")
if (!is.na(arg_max)) {
  split_variable <- arg_max
  print(paste("Split variable found =", split_variable))
} else {
  print("Split variable not found.")
}

# 6. If a split variable was not found, 
#    move on to procedure 2.
if (is.na(split_variable)) {
  print("Performing procedure 2 ...")
  
  # TO DO ...
  
  print("Procedure 2 complete.")
}


