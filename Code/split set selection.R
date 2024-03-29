library(readr)
library(dplyr)
library(MASS) 
data_path <- "D:/Trinity/DA/student-mat-por.csv"
data <- read_csv(data_path)

#suppose the split variable is studytime
split_variable <- 'studytime'
split_values <- sort(unique(data[[split_variable]]))
impurity_reductions <- numeric(length(split_values) - 1)

#Go through every possible split values
for (i in seq_along(split_values)[-length(split_values)]) {
  
  left_data <- data[data[[split_variable]] <= split_values[i], ]
  right_data <- data[data[[split_variable]] > split_values[i], ]
  
  # Poisson Model
  model_left_poisson <- glm(absences ~ studytime, data = left_data, family = poisson())
  model_right_poisson <- glm(absences ~ studytime, data = right_data, family = poisson())
  impurity_reduction_poisson <- -logLik(glm(absences ~ studytime, data = data, family = poisson())) + (logLik(model_left_poisson) + logLik(model_right_poisson))
  
  # Negative Binomial Model
  model_left_negbin <- glm.nb(absences ~ studytime, data = left_data)
  model_right_negbin <- glm.nb(absences ~ studytime, data = right_data)
  impurity_reduction_negbin <- -logLik(glm.nb(absences ~ studytime, data = data)) + (logLik(model_left_negbin) + logLik(model_right_negbin))
  
  # Hurdle Model
  hurdle_model_left <- hurdle(absences ~ studytime, data = left_data, dist = "poisson")
  hurdle_model_right <- hurdle(absences ~ studytime, data = right_data, dist = "poisson")
  hurdle_impurity_reduction <- -logLik(hurdle(absences ~ studytime, data = data, dist = "poisson")) + (logLik(hurdle_model_left) + logLik(hurdle_model_right))
  
  # Zero-inflated Model
  zi_model_left <- zeroinfl(absences ~ studytime | 1, data = left_data, dist = "poisson")
  zi_model_right <- zeroinfl(absences ~ studytime | 1, data = right_data, dist = "poisson")
  zi_impurity_reduction <- -logLik(zeroinfl(absences ~ studytime | 1, data = data, dist = "poisson")) + (logLik(zi_model_left) + logLik(zi_model_right))
  
  # Calculate impurity reduction
  impurity_reductions[i] <- impurity_reduction
}

best_split_index <- which.max(impurity_reductions)
best_split_value <- split_values[best_split_index]

cat("Best split：", best_split_value, "\n")