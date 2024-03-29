# Procedure1
library(readr)
library(stats)
AviationData <- read_csv("data_clean.csv") 

model <- glm(Total.Uninjured ~ Injury.Severity + Aircraft.damage + Aircraft.Category + Amateur.Built + Number.of.Engines + Engine.Type + Weather.Condition + Broad.phase.of.flight, 
             data = AviationData, family = poisson)

# covariates as factor
cols_to_factor <- c("Injury.Severity", "Aircraft.damage", "Aircraft.Category", "Amateur.Built", 
                    "Number.of.Engines", "Engine.Type", "Weather.Condition", "Broad.phase.of.flight")
AviationData[cols_to_factor] <- lapply(AviationData[cols_to_factor], factor)

residuals <- residuals(model, type = "pearson")

calculate_chi_squared <- function(var, residuals) {
  # Create a contingency table for var
  contingency_table <- table(residuals > 0, AviationData[[var]])
  
  # Remove entries with zero column totals
  contingency_table <- contingency_table[, apply(contingency_table, 2, sum) > 0]
  
  # Calculate Pearson chi-squared test statistic
  chi_squared <- chisq.test(contingency_table)$statistic
  
  # Calculate ν
  nu <- min(dim(contingency_table)) - 1
  
  # If ν > 1, use Wilson-Hilferty approximation
  if (nu > 1) {
    # Apply Wilson-Hilferty approximation
    chi_squared <- max(0, (7/9) + sqrt(nu) * ((chi_squared / nu)^(1/3) - 1 + 2/(9*nu)))^3
  }
  
  return(chi_squared)
}


# Define ω_i
omega_i <- c("Injury.Severity", "Aircraft.damage", "Aircraft.Category", "Amateur.Built", "Number.of.Engines", "Engine.Type", "Weather.Condition", "Broad.phase.of.flight")
#print(omega[1])

# Calculate W1(ω)
W1_result <- sapply(omega_i, function(var) {
  calculate_chi_squared(var, residuals)
})
print(W1_result)
##############
# Function to calculate the chi-squared distribution percentile
calculate_chi_percentile <- function(alpha) {
  qchisq(1 - alpha, df = 1)
}

# Calculate chi-square(1, alpha1)
alpha1 <- 0.05 / length(omega_i)
chi_square_alpha1 <- calculate_chi_percentile(alpha1)

# 1: Calculate W1(ω) for each covariate
W1_result <- sapply(omega_i, function(var) {
  calculate_chi_squared(var, residuals)
})

# 2: Check if max W1(ωi) > chi-square(1, alpha1)
max_W1 <- max(W1_result)
if (max_W1 > chi_square_alpha1) {
  # Choose the variable associated with the largest value of W1(ωi) and exit
  chosen_variable <- omega_i[which.max(W1_result)]
  print(paste("Variable associated with the largest value of W1(ωi) is:", chosen_variable))
} else {
  print("None of the variables exceed the threshold chi-square(1, alpha1).")
}





