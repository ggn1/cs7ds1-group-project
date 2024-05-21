# LOAD PACKAGES
require(rstudioapi) # For dynamic setwd(...)

# SET WORKING DIRECTORY
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print(getwd())

# HELPER FUNCTIONS
source("./tree_building.R")

calculate.model.metrics <- function(model, response_variable, model_type, newdata = newdata){
  # Calculate training accuracy
  squ_err.sum.train <- sum(residuals(model, type = "response")^2)
  abs_err.sum.train <- sum(abs(residuals(model, type = "response")))
  n_residual.train <- length(residuals(model, type = "response"))
  
  # Calculate test accuracy
  tmp <- tryCatch({
    test.residual <- newdata[[response_variable]] - predict(model, newdata = newdata)
    c(sum(test.residual^2), sum(abs(test.residual)), length(test.residual))
  }, error = function(e) {
    c(NA, NA,NA)
  })
  squ_err.sum.test <- tmp[1]
  abs_err.sum.test <- tmp[2]
  n_residual.test <- tmp[3]
  
  # Calculate dispersion
  if (model_type %in% c("mob_p", "mob_nb")){
    # If tree model, get mean dispersion of terminal nodes
    dispersion <- mean(unlist(nodeapply(model, ids = nodeids(model, terminal = TRUE), 
                                        FUN = function(x) sum(residuals(x$info$object, type = "pearson")^2)/x$info$object$df.residual)))
  }
  else{
    dispersion <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  }
  
  # Calculate ratio of expected/observed zeros
  if (model_type == "p"){
    expected.zero <- round(sum(dpois(0, fitted(model))))
  }
  else if (model_type == "nb"){
    expected.zero <- round(sum(dnbinom(0, mu = fitted(model), size = model$theta)))
  }
  else if (model_type %in% c("zip", "zinb", "hp", "hnb")){
    expected.zero <- round(sum(predict(model, type = "prob")[,1]))
  }
  else if (model_type %in% c("mob_p", "mob_nb")){
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
  
  results <- cbind(squ_err.sum.train, abs_err.sum.train, squ_err.sum.test, abs_err.sum.test,
                   n_residual.train, n_residual.test, dispersion, expected.zero)
  return(results)
}

evaluate.tree <- function(node, response_variable, newdata){
  
  # If terminal node
  if (node[["type"]] == "terminal"){
    response <- node[['data']][[response_variable]]
    results <- calculate.model.metrics(node[["model"]], response_variable, 
                                       node[["model_type"]], newdata = newdata)
    print(results)
    return(results)
  }
  else if (node[["type"]] == "intermediate"){
    result_l <- c(NA, NA, NA, NA, NA, NA, NA, NA)
    result_r <- c(NA, NA, NA, NA, NA, NA, NA, NA)
    
    # Split new data according to split condition
    split_var_dtype <- get_cat_con(newdata[[node$split_variable]])
    data_split <- hash()
    if (split_var_dtype == 'con') {
      data_split[['left']] <- newdata[
        newdata[[node$split_variable]] < node$split_set, 
        , drop = FALSE
      ]
      data_split[['right']] <- newdata[
        newdata[[node$split_variable]] >= node$split_set, 
        , drop = FALSE
      ]
    } else {
      data_split[['left']] <- newdata[
        newdata[[node$split_variable]] %in% node$split_set, 
        , drop = FALSE
      ]
      data_split[['right']] <- newdata[
        !newdata[[node$split_variable]] %in% node$split_set, 
        , drop = FALSE
      ]
    }
      
    if(node[['has_left_child']] == TRUE){
      result_l <- evaluate.tree(node[["left_child"]], response_variable, newdata = data_split[['left']])
    }
    
    if(node[['has_right_child']] == TRUE){
      result_r <- evaluate.tree(node[["right_child"]], response_variable, newdata = data_split[['right']])
    }
    
    return(rbind(result_l, result_r))
  }
  
}


evaluate_cv <- function(model_type, data, min_split_pc=0.05, max_depth=-1) {
  #Randomly shuffle the data
  data_shuffled<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  fold_idx <- cut(seq(1,nrow(data_shuffled)),breaks=10,labels=FALSE)
  
  results <- NULL
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    # Split into train and test data
    testIndexes <- which(fold_idx==i,arr.ind=TRUE)
    testData <- data_shuffled[testIndexes, ]
    trainData <- data_shuffled[-testIndexes, ]
    
    # Sanitize data for model fitting.
    data_sanitized <- sanitize_fit_input(
      data_check = trainData,
      data_apply = list(trainData, testData),
      response_variable = RESPONSE_VARIABLE
    )
    trainData <- data_sanitized[[1]]
    testData <- data_sanitized[[2]]
    
    if (model_type == 'p') {
      model <- glm(absences ~ ., data=trainData, family="poisson")
    } 
    else if (model_type == 'nb') {
      model <- glm.nb(absences ~ ., data=trainData)
    } 
    else if (model_type == 'zip') {
      model <- zeroinfl(absences ~ ., data=trainData, dist="poisson")
    } 
    else if (model_type == 'zinb') {
      model <- zeroinfl(absences ~ ., data=trainData, dist="negbin")
    } 
    else if (model_type == 'hp') {
      model <- hurdle(absences ~ ., data=trainData, dist="poisson")
    } 
    else if (model_type == 'hnb') {
      model <- hurdle(absences ~ ., data = trainData, dist = "negbin")
    } 
    else if (model_type == 'mob_p') {
      fit_poisson <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
        glm(formula = y ~ x, family = poisson(link = "log"), start = start, ...)
      }
      model <- mob(absences ~ . | ., data = trainData, fit = fit_poisson)
    } 
    else if (model_type == 'mob_nb') {
      fit_negbin <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
        glm.nb(y ~ x, start = start, ...)
      }
      model <- mob(absences ~ . | ., data = trainData, fit = fit_negbin)
    } 
    else if (model_type == 'core'){
      # Build tree.
      TOTAL_N_ROWS <- nrow(trainData) # global variable.
      model <- create_tree(data = trainData, TOTAL_N_ROWS=TOTAL_N_ROWS,
                           min_split_pc=min_split_pc, max_depth=max_depth)
      print(model)
    }
    else { # Invalid model.
      return (-1)
    }
    
    if (model_type == 'core'){
      metrics <- data.frame(evaluate.tree(model, RESPONSE_VARIABLE, newdata = testData))
    }
    else{
      metrics <- data.frame(calculate.model.metrics(model, RESPONSE_VARIABLE, model_type, newdata = testData))
    }
    
    tmp <- data.frame(rbind(colSums(metrics, na.rm=TRUE)))
    tmp$mse.train <- tmp$squ_err.sum.train/tmp$n_residual.train
    tmp$mae.train <- tmp$abs_err.sum.train/tmp$n_residual.train
    tmp$mse.test <- tmp$squ_err.sum.test/tmp$n_residual.test
    tmp$mae.test <- tmp$abs_err.sum.test/tmp$n_residual.test
    tmp$dispersion <- mean(unlist(metrics["dispersion"]), na.rm=TRUE)

    observed.zero <- sum(trainData$absences == 0)
    tmp$expected.zero.ratio <- tmp$expected.zero/observed.zero
    
    results <- rbind(results, tmp[c("mse.train", "mae.train", "mse.test", "mae.test", 
                                  "dispersion", "expected.zero.ratio")])
  }

  return(results)
}