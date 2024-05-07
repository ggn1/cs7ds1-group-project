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

fit_model <- function(model_type) {
  ### Given a model type, fits it to the data.  
  ### @param model_type: The type of model to be
  ###                    fitted. Options are as follows.
  ###                    * p (poisson)
  ###                    * zip (zero inflated poisson zip)
  ###                    * nb (negative binomial)
  ###                    * zinb (zero inflated negative binomial)
  ###                    * hp (hurdle poisson)
  ###                    * hnb (hurdle negative binomial)
  ### @return model: Fitted model.
  model = NA
  if (model_type == 'p') { # POISSON MODEL
    model <- glm(
      absences ~ ., 
      data=school_absences, 
      family="poisson"
    )
  } else if (model_type == 'nb') { # NEGATIVE BINOMIAL MODEL
    model <- glm.nb(
      absences ~ ., 
      data=school_absences
    )
  } else if (model_type == 'zip') { # ZERO INFLATED POISSON MODEL
    model <- zeroinfl(
      absences ~ ., 
      data=school_absences, 
      dist="poisson"
    )
  } else if (model_type == 'zinb') { # ZERO INFLATED NEGATIVE BINOMIAL MODEL
    model <- zeroinfl(
      absences ~ ., 
      data=school_absences, 
      dist="negbin"
    )
  } else if (model_type == 'hp') { # POISSON HURDLE MODEL
    model <- hurdle(
      absences ~ ., 
      data=school_absences, 
      dist="poisson"
    )
  } else if (model_type == 'hnb') { # NEGATIVE BINOMIAL HURDLE MODEL
    model <- hurdle(
      absences ~ ., 
      data=school_absences, 
      dist="negbin"
    )
  } else {
    stop("Invalid model type.")
  }
  return(model)
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
#     p (poisson), zip (zero inflated poisson zip),
#     nb (negative binomial), zinb (zero inflated negative binomial)
#     hp (hurdle poisson), hnb (hurdle negative binomial)
#    ]
model_type <- "zip"
m <- fit_model(model_type)

# 2. Get partial score residuals. ???
part_score_res <- as.data.frame(residuals(
  m, model="count", type = "partial"
))

# Calculate residuals for the count component
count_residuals <- residuals(model, model='zero', type = "partial")

# Calculate residuals for the zero-inflation component
residuals <- residuals(model, type = "pearson")


# 3. Procedure 1
print("Performing procedure 1 ...")
procedure1_res <- hash()
for (covariate_name_type in covariates) {
  # Get covariate name and type.
  covariate_name <- covariate_name_type[[1]]
  covariate_type <- covariate_name_type[[2]]
  
  # 3.1. Get specific partial scores.
  part_res <- part_score_res[covariate_name][,]
  
  # 3.2. Group by signs of the residuals.
  #      0 => < 0
  #      1 => >= 0
  part_res_sign <- rep(1, times=length(part_res))
  part_res_sign[which(part_res < 0)] = 0

  # 3.3. If the covariate is not categorical, 
  #      group by quartiles else, group by categories.
  groups <- NA
  if (covariate_type == "con") {
    five_num_sum <- fivenum(part_res)
    q1 <- five_num_sum[1]
    q2 <- five_num_sum[2]
    q3 <- five_num_sum[3]
    quartile_group <- rep(4, times=length(part_res))
    quartile_group[which(part_res <= q3)] = 3
    quartile_group[which(part_res <= q2)] = 2
    quartile_group[which(part_res <= q1)] = 1
    groups <- factor(quartile_group)
  } else {
    groups <- factor(school_absences[covariate_name][,])
  }
  
  # 3.4. Create a contingency table with
  #      residual signs as rows and groups 
  #      as columns.
  contingency_table <- table(factor(part_res_sign), groups)

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
  
}

print("Procedure 1 complete.")

# PROCEDURE 2
# TO DO ...
