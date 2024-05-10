# LOAD PACKAGES
require(pscl) # Zero inflated poisson model.
require(MASS) # Poisson and negative binomial models.

sanitize_fit_input <- function(
  data_check, data_apply, response_variable
) {
  ### Determines which columns to drop from the 
  ### given "data_check" dataset and then drops
  ### them from all data sets given in the "data_apply"
  ### list and returns the modified list.
  ### Columns with only one level are not suitable
  ### to provide as input to glm.fit(...) function.
  ### Thus all such columns except the response variable
  ### columns is dropped.
  ### @param data_check: The data set to examine for
  ###                    columns to drop.
  ### @param data_apply: A list of datasets of which
  ###                    all identified drop columns are
  ###                    to be dropped.
  ### @param response_variable: The variable to predict and
  ###                           thus not drop.
  ### @return: Given data_apply list of datasets such that 
  ###          columns to drop have been removed.
  single_level_columns <- sapply(
    data_check, function(x) length(unique(x))
  ) == 1
  single_level_columns[[response_variable]] = FALSE
  for (i in 1:length(data_apply)) {
    data_apply[[i]] <- data_apply[[i]][, !single_level_columns]
  }
  return(data_apply)
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
  
  
  # Drop factors that only have one level
  cols_unique <- sapply(data, function(x) length(unique(x))) > 1
  data <- data[names(cols_unique[cols_unique])]
  #print(data)
  
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

get_cat_con <- function(values) {
  ### Given a vector, will return 'cat'
  ### if the vector contains categorical
  ### data and 'con' if it contains 
  ### continuous data.
  ### @param values: Vector of values.
  ### @return data_type: Type of data in given vector.
  ###                    * "cat" = categorical
  ###                    * "con" = continuous
  data_type <- 'con'
  if (is.factor(values) || is.character(values)) {
    data_type <- 'cat'
  }
  return(data_type)
}