# Import libraries.
library(hash) # For hash map data structure.
library(pscl) # Zero inflated poisson model.
library(MASS) # Poisson and negative binomial models.

# Set working directory to this one.
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

# UTILITY FUNCTIONS
get_cat_con <- function(data, response_variable) {
  ### Computes whether each column in the data set is
  ### categorical or non-categorical in nature.
  ### @param data: Data set
  ### @param response_variable: Name of the response 
  ###                           variable.
  ### @return: A list of pairs corresponding to 
  ###          all covariates (except target feature)
  ###          such that the first element in the pair
  ###          is the name of the feature and the second
  ###          element is the type of that feature.
  covariates <- list()
  for (covariate in names(data)) { # Check each covariate.
    # Skip the response variable.
    if (covariate == response_variable) next 
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
  if (model_type == 'p') { # POISSON MODEL
    m <- glm(
      as.formula(formula_str), 
      data=data, 
      family="poisson"
    )
  } else if (model_type == 'nb') { # NEGATIVE BINOMIAL MODEL
    m <- glm.nb(
      as.formula(formula_str), 
      data=data
    )
  } else if (model_type == 'zip') { # ZERO INFLATED POISSON MODEL
    m <- zeroinfl(
      as.formula(formula_str), 
      data=data, 
      dist="poisson"
    )
  } else if (model_type == 'zinb') { # ZERO INFLATED NEGATIVE BINOMIAL MODEL
    m <- zeroinfl(
      as.formula(formula_str), 
      data=data, 
      dist="negbin"
    )
  } else if (model_type == 'hp') { # POISSON HURDLE MODEL
    m <- hurdle(
      as.formula(formula_str), 
      data=data, 
      dist="poisson"
    )
  } else if (model_type == 'hnb') { # NEGATIVE BINOMIAL HURDLE MODEL
    m <- hurdle(
      as.formula(formula_str), 
      data=data,
      dist="negbin"
    )
  } else {
    stop("Invalid model type.")
  }
  return(m)
}

compute_residual <- function(
  m, model_type, response_variable, data
) {
  ### Computes and returns residuals based on
  ### model predictions v/s true counts.
  ### @param m: Fitted model.
  ### @param model_type: Type of model (p, nb, 
  ###                    zip, zinb, hp, hnb)
  ### @param response_variable: Name of the variable 
  ###                           being predicted.
  ### @param data: The data to be used as input
  ###              to generate predictions.
  # Get ground truth.
  truth <- hash()
  truth[['count']] <- data[response_variable][,]
  truth[['zero']] <- as.numeric(
    ifelse(truth[['count']] == 0, 1, 0)
  )
  
  # Get predictions.
  predictions <- hash()
  predictions[['count']] <- predict(
    m, type="response", newdata = data
  )
  predictions[['zero']] <- list()
  if (
    model_type == 'zip' ||
    model_type == 'zinb' ||
    model_type == 'hp' ||
    model_type == 'hnb'
  ) {
    predictions[['zero']] <- predict(
      m, type="zero", newdata = data
    )
  }
  
  # Compute residual.
  residual <- hash()
  residual[['count']] <- (
    predictions[['count']] - 
    truth[['count']]
  )
  residual[['zero']] <- list()
  if (length(predictions[['zero']]) > 0) {
    residual[['zero']] <- (
      predictions[['zero']] - 
        truth[['zero']]
    )
  }
  
  # Return residual
  return(residual)
}

compute_partial_residual <- function(
    model_type, response_variable, data,
    residual_original, exclude_variables
) {
  ### Computes and returns the partial residual
  ### corresponding to a predictor in the 
  ### given data set w.r.t a provided original
  ### residual.
  ### The idea of partial residual implemented here
  ### is as follows.
  ### * Let P = set of all predictors in given data 
  ###           set, "data".
  ### * Let p = predictor for which the partial 
  ###   residual is to be computed.
  ### * Then, original_residual = residual obtained
  ###   based on predictions generated when all
  ###   predictors in P including p is provided
  ###   as input to the model.
  ### * Then, partial_residual = Difference between
  ###   the original residual and a residual obtained
  ###   based on predictions generated when all 
  ###   predictors except p (P - p) is provided 
  ###   as input to the model.
  ### @param model_type: Type of model to fit.
  ### @param response_variable: The variable to predict.
  ### @param data: The data set to fit.
  ### @param residual_original: The residual to compare
  ###                           against to get the
  ###                           partial residual.
  ### @param exclude_variables: The variable(s) to 
  ###                           exclude.
  ###                           These are the ones 
  ###                           that we are computing
  ###                           the partial residual for.
  ### @return: Partial residual for given predictor.
  
  # Drop the column corresponding to the
  # predictor for which the partial
  # residual is being computed.
  for (ex_var in exclude_variables) {
    data <- data[, -which(names(data) == ex_var)]
  }
  
  # Create and fit new model of given type
  # on data excluding given variable.
  m <- fit_model(
    model_type = model_type,
    data = data,
    response_variable = response_variable
  )
  
  # Get residual if the model was fitted
  # on given data such that all but given
  # predictor was considered as input.
  residual_ex_predictor <- compute_residual(
    m = m, model_type = model_type,
    response_variable = response_variable,
    data = data
  )
  
  # Compute and return partial residual.
  residual_partial <- hash()
  residual_partial[["count"]] <- (
    residual_original[["count"]] - 
    residual_ex_predictor[["count"]]
  )
  residual_partial[["zero"]] <- list()
  if(length(residual_original[["zero"]]) > 0) {
    residual_partial[["zero"]] <- (
      residual_original[["zero"]] - 
      residual_ex_predictor[["zero"]]
    )
  }
  return(residual_partial)
}

# SPLIT VARIABLE SELECTION
get_split_variable <- function(
    data, response_variable, model_type
) {
  ### Determines the best variable to use to split 
  ### the tree. Partial residuals and the Chi-Squaare
  ### test is leveraged repeatedly to do this after
  ### considering the main effect of each predictor
  ### independently w.r.t the response variable as 
  ### well as the join effect of pairs of predictors
  ### on the same.
  ### @param data: Input data set containing the
  ###              response variable. All variables in
  ###              this data set other than the response
  ###              variable, is considered to be a
  ###              predictor.
  ### @param response_variable: The variable whose value
  ###                           is being predicted.
  ### @param model_type: The type of underlying model
  ###                    to use. Options are as follows.
  ###                    * p = Poisson.
  ###                    * nb = Negative Binomial.
  ###                    * zip = Zero Inflated Poisson.
  ###                    * zinb = Zero Inflated 
  ###                             Negative Binomial.
  ###                    * hp = Poisson Hurdle.
  ###                    * hnb = Negative Binomial Hurdle.
  
  split_variable <- NA
  
  # Get covariate (name, type) pairs.
  cov_name_types <- get_cat_con( 
    data = data,
    response_variable = response_variable
  )
  
  # 1. Fit model.
  m <- fit_model(
    model_type = model_type, 
    response_variable = response_variable,
    data = data
  )
  predictors <- c()
  for (cov in cov_name_types) {
    predictors <- c(predictors, cov[1])
  }
  
  # 2. Get partial score residuals.
  
  # Residual obtained if all predictors are
  # used as input to the model.
  residual_original <- compute_residual(
    m = m, model_type = model_type, 
    response_variable = response_variable,
    data = data
  )
  
  # Compute and keep track of partial residual
  # corresponding to every predictor.
  partial_residuals <- list()
  for (predictor in predictors) {
    partial_residual <- compute_partial_residual(
      model_type = model_type, 
      response_variable = response_variable,
      data = data,
      residual_original = residual_original,
      exclude_variable = c(predictor)
    )
    partial_residuals[[predictor]] <- partial_residual
  }
  
  # 3. Procedure 1
  print("Performing procedure 1 ...")
  procedure1_res <- hash()
  for (cov_name_type in cov_name_types) {
    # Get covariate name and type.
    cov_name <- cov_name_type[[1]]
    cov_type <- cov_name_type[[2]]
    
    # 3.1. Get partial residual groups.
    
    # Get specific partial scores.
    part_res <- partial_residuals[[cov_name]]
    
    # Group by signs of the residuals.
    # * 0 => < 0 (negative)
    # * 1 => >= 0 (positive)
    part_res_grp <- hash()
    
    # For count component.
    part_res_grp[["count"]] <- rep(
      1, times=length(part_res[['count']])
    )
    part_res_grp[["count"]][
      which(part_res[['count']] < 0)
    ] = 0 
    part_res_grp[["count"]] <- factor(
      part_res_grp[["count"]]
    )
    
    # For zero component.
    part_res_grp[["zero"]] <- NA
    if (length(part_res[['zero']]) > 0) {
      part_res_grp[["zero"]] <- rep(
        1, times=length(part_res[['zero']])
      )
      part_res_grp[["zero"]][
        which(part_res[['zero']] < 0)
      ] = 0
      part_res_grp[["zero"]] <- factor(
        part_res_grp[["zero"]]
      )
    }
    
    # 3.2. Get data groups.
    
    # If the covariate is not categorical, 
    # group by quartiles else, group by categories.
    cov_data <- data[cov_name][,]
    cov_data_grp <- NA
    if (cov_type == "con") {
      five_num_sum <- fivenum(cov_data)
      q1 <- five_num_sum[1]
      q2 <- five_num_sum[2]
      q3 <- five_num_sum[3]
      quartile_grp <- rep(4, times=length(cov_data))
      quartile_grp[which(cov_data <= q3)] = 3
      quartile_grp[which(cov_data <= q2)] = 2
      quartile_grp[which(cov_data <= q1)] = 1
      cov_data_grp <- factor(quartile_grp)
    } else {
      cov_data_grp <- factor(cov_data)
    }
    
    # 3.3 Get Chi-Square values.
    
    # Create a contingency table with residual
    # signs as rows and groups as columns.
    contingency_table = NA
    if ( # For normal count models.
      model_type == "p" ||
      model_type == "nb"
    ) {
      contingency_table <- table(
        part_res_grp[['count']],
        cov_data_grp
      )
    } else { # For zero inflated or hurdle models.
      contingency_table <- table(
        paste0(
          part_res_grp[['count']], 
          part_res_grp[['zero']]
        ), cov_data_grp
      )
    }
    
    # Drop entries where column total == 0.
    column_totals <- margin.table(
      contingency_table, margin = 2
    )
    non_zero_columns <- column_totals > 0
    contingency_table <- contingency_table[
      , non_zero_columns
    ]
    
    # Compute the Pearson chi-squared test
    # statistic for independence.
    chi_squared_test <- chisq.test(contingency_table)
    deg_free <- chi_squared_test$parameter
    chi_squared <- chi_squared_test$statistic
    if (deg_free > 1) { # Wilson-Hilferty approximation.
      chi_squared <- max(0, (7/9) + sqrt(deg_free) * (
         (chi_squared/deg_free)^(1/3)-1+2/(9*deg_free)
      ))^3
    }
    
    # 3.4. Keep track of computed values for the future.
    procedure1_res[[cov_name]] = chi_squared
  }
  print("Procedure 1 complete.")
  
  # 4. Check if can select best feature now.
  K <- length(data) - 1 # No. of input features.
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
    print(paste(
      "Split variable found =", 
      split_variable
    ))
  } else {
    print("Split variable not found.")
  }
  
  # 5. If a split variable was not found, 
  #    move on to procedure 2.
  if (is.na(split_variable)) {
    print("Performing procedure 2 ...")
    
    # 5.1. Get all possible combinations of predictors.
    combinations <- combn(cov_name_types, 2) # [, col]
    n_combinations <- length(combinations)/2
    
    # Keep track of procedure 2 results.
    procedure2_res <- hash()
    
    # Iterate through each combination.
    for (i in 1:n_combinations) { 
      combination <- combinations[, i]
      
      # 5.2. Get data groupings.
      cov_data_grp <- hash()
      for (p in combination) {
        p_name <- p[1]
        p_type <- p[2]
        
        p_data <- data[p_name][,]
        
        p_data_grp <- NA
        if (p_type == 'con') {
          p_data_med <- median(p_data)
          p_data_grp <- rep(1, times=length(p_data))
          p_data_grp[which(p_data < p_data_med)] = 0
          p_data_grp <- factor(p_data_grp)
        } else { # (p_type == 'cat')
          p_data_grp <- factor(p_data)
        }
        cov_data_grp[[p_name]] <- p_data_grp
      }
      
      # 5.3. Get residual groupings.
      
      # Group by signs of the residuals.
      # * 0 => < 0 (negative)
      # * 1 => >= 0 (positive)
      part_res_grp <- hash()
      
      p_names <- c()
      for (p in combination) {
        p_names <- c(p_names, p[1])
      }
      
      part_res <- compute_partial_residual(
        model_type = model_type, 
        response_variable = response_variable,
        residual_original = residual_original,
        exclude_variables = p_names,
        data = data
      )
      
      # For count component.
      part_res_grp[["count"]] <- rep(
        1, times=length(part_res[['count']])
      )
      part_res_grp[["count"]][
        which(part_res[['count']] < 0)
      ] = 0 
      part_res_grp[["count"]] <- factor(
        part_res_grp[["count"]]
      )
      
      # For zero component.
      part_res_grp[["zero"]] <- NA
      if (length(part_res[['zero']]) > 0) {
        part_res_grp[["zero"]] <- rep(
          1, times=length(part_res[['zero']])
        )
        part_res_grp[["zero"]][
          which(part_res[['zero']] < 0)
        ] = 0
        part_res_grp[["zero"]] <- factor(
          part_res_grp[["zero"]]
        )
      }
      
      # 5.4. Get Chi-Square values.
      
      # Create a contingency table with residual
      # signs as rows and groups as columns.
      contingency_table = NA
      if ( # For normal count models.
        model_type == "p" ||
        model_type == "nb"
      ) {
        contingency_table <- table(
          part_res_grp[['count']],
          paste0(
            cov_data_grp[[p_names[1]]], 
            cov_data_grp[[p_names[2]]]
          )
        )
      } else { # For zero inflated or hurdle models.
        contingency_table <- table(
          paste0(
            part_res_grp[['count']], 
            part_res_grp[['zero']]
          ), paste0(
            cov_data_grp[[p_names[1]]], 
            cov_data_grp[[p_names[2]]]
          )
        )
      }
      
      # Drop entries where column total == 0.
      column_totals <- margin.table(
        contingency_table, margin = 2
      )
      non_zero_columns <- column_totals > 0
      contingency_table <- contingency_table[
        , non_zero_columns
      ]
      
      # Compute the Pearson chi-squared test
      # statistic for independence.
      chi_squared_test <- chisq.test(contingency_table)
      deg_free <- chi_squared_test$parameter
      chi_squared <- chi_squared_test$statistic
      if (deg_free > 1) { # Wilson-Hilferty approximation.
        chi_squared <- max(0, (7/9)+sqrt(deg_free)*(
          (chi_squared/deg_free)^(1/3)-1+2/(9*deg_free)
        ))^3
      }
      
      # 5.5. Keep track of computed values for the future.
      procedure2_res[[
        paste(p_names[1], p_names[2], sep = "+")
      ]] = chi_squared
    }
    
    print("Procedure 2 complete.")
    
    # 6. Pick best split variable.
    print("Checking for good split variable ...")
    condition_met <- hash()
    cur_max <- -Inf
    arg_max <- NA
    for (combination in keys(procedure2_res)) {
      chi_squared <- procedure2_res[[combination]]
      if (chi_squared > chi_squared_df1_alpha2) {
        condition_met[[combination]] <- chi_squared
        if (chi_squared > cur_max) {
          arg_max <- combination
          cur_max <- chi_squared
        }
      }
    }
    print("Check complete.")
    if (!is.na(arg_max)) {
      combination <- arg_max
      p_names <- strsplit(combination, "\\+")
      p1_name <- p_names[[1]][1]
      p2_name <- p_names[[1]][2]
      if (
        procedure1_res[[p1_name]] >
        procedure1_res[[p2_name]]
      ) {
        split_variable = p1_name
      } else {
        split_variable = p2_name
      }
    } else {
      cur_max <- -Inf
      arg_max <- NA
      for (covariate in keys(procedure1_res)) {
        chi_squared <- procedure1_res[[covariate]]
        if (chi_squared > cur_max) {
          arg_max <- covariate
          cur_max <- chi_squared
        }
      }
      split_variable <- arg_max
    }
    print(paste(
      "Split variable found =", 
      split_variable
    ))
  }

  # Return identified best split variable.  
  return(split_variable)
}

# MAIN
school_absences <- read.csv(
  "../Data/data_clean.csv", 
  header=TRUE, sep=","
)
split_variable <- get_split_variable(
  data = school_absences, 
  response_variable = "absences",
  model_type = "hnb"
)


