# LOAD PACKAGES
require(hash) # For hash map data structure.
require(pscl) # Zero inflated poisson model.
require(MASS) # Poisson and negative binomial models.
require(caret) # For cross validation.
require(partykit) # MOB models
require(ggplot2) # Plotting

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
  fold_indices <- cut(1:n, breaks = folds, labels = FALSE)
  
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
  
  return(mse)
}

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
  if (model_type == 'p' || model_type == "nb") {
    predictions[['count']] <- predict(
      m, type="response", newdata = data
    )
    predictions[['zero']] <- list()
  } 
  # model_type == 'zip' || 
  # model_type == 'zinb' ||
  # model_type == 'hp' || 
  # model_type == 'hnb'
  else {
    predictions[['count']] <- predict(
      m, type="count", newdata = data
    )
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

# Set working directory to this one.
setwd("C:/Users/kimno/Documents/TCD/Data Analytics/Project/repos/Code")

# Load data.
school_absences <- read.csv(
  "../Data/data_clean.csv", 
  header=TRUE, sep=","
)
response_variable <- "absences"

# 80/20 Train/Test split.
index_train <- createDataPartition(
  school_absences[response_variable][,], 
  p = 0.8, list = FALSE
)
data_train <- school_absences[index_train, ]
data_test <- school_absences[-index_train, ]

# Model selection.
model_type <- get_best_model(
  data = data_train,
  response_variable = response_variable
)

# Split variable selection.
split_variable <- get_split_variable(
  data = data_train, 
  response_variable = "absences",
  model_type = model_type
)





# EXPERIMENTS
# TODO: Add CORE to evaluation

evaluation_cv <- function(model_type, data) {
  #Randomly shuffle the data
  data_shuffled<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  fold_idx <- cut(seq(1,nrow(data_shuffled)),breaks=10,labels=FALSE)
  
  mse.train <- list()
  mae.train <- list()
  mse.test <- list()
  mae.test <- list()
  dispersion <- list()
  expected.zero.ratio <- list()
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    # Split into train and test data
    testIndexes <- which(fold_idx==i,arr.ind=TRUE)
    testData <- data_shuffled[testIndexes, ]
    trainData <- data_shuffled[-testIndexes, ]
    
    if (model_type == 'poisson') {
      model <- glm(absences ~ ., data=trainData, family="poisson")
    } 
    else if (model_type == 'negbin') {
      model <- glm.nb(absences ~ ., data=trainData)
    } 
    else if (model_type == 'zip') {
      model <- zeroinfl(absences ~ ., data=trainData, dist="poisson")
    } 
    else if (model_type == 'zinb') {
      model <- zeroinfl(absences ~ ., data=trainData, dist="negbin")
    } 
    else if (model_type == 'hurdle_p') {
      model <- hurdle(absences ~ ., data=trainData, dist="poisson")
    } 
    else if (model_type == 'hurdle_negbin') {
      model <- hurdle(absences ~ ., data = trainData, dist = "negbin")
    } 
    else if (model_type == 'mob_p') {
      fit_poisson <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
        glm(formula = y ~ x, family = poisson(link = "log"), start = start, ...)
      }
      model <- mob(absences ~ . | ., data = data, fit = fit_poisson)
    } 
    else if (model_type == 'mob_negbin') {
      fit_negbin <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
        glm.nb(y ~ x, start = start, ...)
      }
      model <- mob(absences ~ . | ., data = data, fit = fit_negbin)
    } 
    else if (model_type == 'core') {
      # TODO: fit CORE model
    } 
    else { # Invalid model.
      return (-1)
    }
    
    # Calculate train accuracy
    mse.train <- append(mse.train, mean(residuals(model, type = "response")^2))
    mae.train <- append(mae.train, mean(abs(residuals(model, type = "response"))))
    
    # Calculate test accuracy
    test.residual <- testData$absences - predict(model, newdata = testData)
    mse.test <- append(mse.test, mean(test.residual^2))
    mae.test <- append(mae.test, mean(abs(test.residual)))
    
    # Calculate dispersion
    if (model_type %in% c("mob_p", "mob_negbin")){
      # If tree model, get mean dispersion of terminal nodes
      dispersion <- mean(unlist(nodeapply(model, ids = nodeids(model, terminal = TRUE), 
                                          FUN = function(x) sum(residuals(x$info$object, type = "pearson")^2)/x$info$object$df.residual)))
    }
    else{
      dispersion <- append(dispersion, sum(residuals(model, type = "pearson")^2) / model$df.residual)
    }
    
    # Calculate ratio of expected/observed zeros
    if (model_type == "poisson"){
      expected.zero <- round(sum(dpois(0, fitted(model))))
    }
    else if (model_type == "negbin"){
      expected.zero <- round(sum(dnbinom(0, mu = fitted(model), size = model$theta)))
    }
    else if (model_type %in% c("zip", "zinb", "hurdle_p", "hurdle_negbin")){
      expected.zero <- round(sum(predict(model, type = "prob")[,1]))
    }
    else if (model_type %in% c("mob_p", "mob_negbin")){
      node_zeros <- function(x){
        model <- x$info$object
        if(model$family$family == "poisson"){
          round(sum(dpois(0, fitted(model))))
        }
        else if (startsWith(model$family$family, "Negative Binomial")){
          round(sum(dnbinom(0, mu = fitted(model), size = model$theta)))
        }
      }
      expected.zero <- sum(unlist(nodeapply(model, ids = nodeids(model, terminal = TRUE),
                                            FUN = node_zeros)))
    }
    observed.zero <- sum(trainData$absences == 0)
    expected.zero.ratio <- append(expected.zero.ratio, expected.zero/observed.zero)
  }
  results <- cbind(unlist(mse.train), unlist(mae.train), unlist(mse.test), unlist(mae.test),
                   unlist(dispersion), unlist(expected.zero), unlist(expected.zero.ratio))
  return(data.frame(results))
}

print("Beginning experiments...")
# Calculate metrics for each model using 10-fold cross validation
res.pois <- evaluation_cv("poisson", school_absences)
res.nb <- evaluation_cv("negbin", school_absences)
res.zip <- evaluation_cv("zip", school_absences)
res.zinb <- evaluation_cv("zinb", school_absences)
res.hurdle.p <- evaluation_cv("hurdle_p", school_absences)
res.hurdle.nb <- evaluation_cv("hurdle_negbin", school_absences)
res.mob.p <- evaluation_cv("mob_p", school_absences)
res.mob.nb <- evaluation_cv("mob_negbin", school_absences)
#res.core <- evaluation_cv("core", school_absences)


# Aggregate results
mean.results <- rbind("Poisson" = colMeans(res.pois),
                      "NB" = colMeans(res.nb),
                      "ZIP" = colMeans(res.zip),
                      "ZINB" = colMeans(res.zinb),
                      "Hurdle Poisson" = colMeans(res.hurdle.p),
                      "Hurdle NB" = colMeans(res.hurdle.nb),
                      "MOB Poisson" = colMeans(res.mob.p),
                      "MOB NB" = colMeans(res.mob.nb))
                      #"CORE" = colMeans(res.core))
colnames(mean.results) <- c("mse.train_mean", "mae.train_mean", "mse.test_mean", "mae.test_mean",
                            "dispersion_mean", "expected.zero_mean","expected.zero.ratio_mean")

sd.results <- rbind("Poisson" = apply(res.pois, 2, sd),
                    "NB" = apply(res.nb, 2, sd),
                    "ZIP" = apply(res.zip, 2, sd),
                    "ZINB" = apply(res.zinb, 2, sd),
                    "Hurdle Poisson" = apply(res.hurdle.p, 2, sd),
                    "Hurdle NB" = apply(res.hurdle.nb, 2, sd),
                    "MOB Poisson" = apply(res.mob.p, 2, sd),
                    "MOB NB" = apply(res.mob.nb, 2, sd))
                    #"CORE" = apply(res.core, 2, sd))
colnames(sd.results) <- c("mse.train_sd", "mae.train_sd", "mse.test_sd", "mae.test_sd",
                          "dispersion_sd", "expected.zero_sd","expected.zero.ratio_sd")

results <- data.frame(cbind(mean.results, sd.results))
results$model <- rownames(results)
results$id <- 1:nrow(results)
results
write.csv(results, "evaluation_results.csv", row.names=FALSE)

# Create plots

# MSE
train <- results[c("model", "id")]
train$mean <- results$mse.train_mean
train$sd <- results$mse.train_sd
train$data <- as.factor("train")

test <- results[c("model", "id")]
test$mean <- results$mse.test_mean
test$sd <- results$mse.test_sd
test$data <- as.factor("test")

ggplot(rbind(train, test)) +
  geom_bar( aes(x=reorder(model, id), y=mean, fill=data), stat="identity", 
            position = "dodge")+
  geom_errorbar(aes(x=reorder(model, id), ymin=mean-sd, ymax=mean+sd, group=data), 
                width=.2, position=position_dodge(.9)) +
  labs(x = "Model", y = "MSE")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))


# MAE
train <- results[c("model", "id")]
train$mean <- results$mae.train_mean
train$sd <- results$mae.train_sd
train$data <- as.factor("train")

test <- results[c("model", "id")]
test$mean <- results$mae.test_mean
test$sd <- results$mae.test_sd
test$data <- as.factor("test")

ggplot(rbind(train, test)) +
  geom_bar( aes(x=reorder(model, id), y=mean, fill=data), stat="identity", 
            position = "dodge")+
  geom_errorbar(aes(x=reorder(model, id), ymin=mean-sd, ymax=mean+sd, group=data), 
                width=.2, position=position_dodge(.9)) +
  labs(x = "Model", y = "MAE")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))

# Dispersion
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=dispersion_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=dispersion_mean-dispersion_sd, 
                    ymax=dispersion_mean+dispersion_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=dispersion_mean-dispersion_sd, label = signif(dispersion_mean)), 
             alpha=0.7, nudge_y=-0.4)+ 
  labs(x = "Model", y = "Dispersion")+
  geom_hline(yintercept=1, color = "red")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))

# Zero-inflation
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=expected.zero.ratio_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=expected.zero.ratio_mean-expected.zero.ratio_sd, 
                    ymax=expected.zero.ratio_mean+expected.zero.ratio_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=expected.zero.ratio_mean-expected.zero.ratio_sd,
                 label = signif(expected.zero.ratio_mean)), 
             alpha=0.7, nudge_y=-0.07)+ 
  labs(x = "Model", y = "Ratio expected/oberved zeros")+
  geom_hline(yintercept=1, color = "red")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))

