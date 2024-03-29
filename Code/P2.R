# Procedure2
# We only have categorical covariates, proceed as follows:

#1. **Pair up each categorical covariate**:
#  - For each pair of categorical covariates, form a contingency table with their combinations as columns.

#2. **Construct contingency tables**:
#  - Construct a contingency table with combinations of categorical covariates as columns and calculate the frequency of each combination based on your data.

#3. **Compute the chi-square test statistic for each contingency table.

#4. **Transform into chi-square values with degrees of freedom equal to 1**:
#  - You can use some method to transform the chi-square test statistic into chi-square values with degrees of freedom equal to 1. This may require specific transformation methods, which could be chosen based on your research field or specific circumstances.

#Through these steps, you can analyze the interaction effects between categorical covariates.

#The final result is the result of W(ω_i, ω_j).

library(readr)
library(stats)

# Read data
AviationData <- read_csv("data_clean.csv")

# Convert columns to factor where needed
cols_to_factor <- c("Injury.Severity", "Aircraft.damage", "Aircraft.Category", "Amateur.Built",
                    "Number.of.Engines", "Engine.Type", "Weather.Condition", "Broad.phase.of.flight")
AviationData[cols_to_factor] <- lapply(AviationData[cols_to_factor], factor)

# Fit a Poisson regression model, excluding the target variable
model <- glm(Total.Uninjured ~ . - Total.Uninjured, data = AviationData, family = poisson)

# Generate contingency tables
contingency_tables <- list()
for (i in 1:(length(cols_to_factor) - 1)) {
  for (j in (i + 1):length(cols_to_factor)) {
    table <- table(AviationData[[cols_to_factor[i]]], AviationData[[cols_to_factor[j]]])
    if (sum(table) > 0) {  # Check if there are observations in the contingency table
      contingency_tables[[paste(cols_to_factor[i], cols_to_factor[j], sep = "_")]] <- table
    }
  }
}

# Calculate chi-square test statistics
chisq_values <- sapply(contingency_tables, function(x) chisq.test(x)$statistic)

# Output chi-square test statistics
print(chisq_values)

# Define a function to transform chi-square statistics
transform_chi_square <- function(chi_square, degrees_of_freedom) {
  nu <- degrees_of_freedom
  if (nu > 1) {
    max_value <- max(0, ((7/9) + sqrt(nu) * ((chi_square / nu)^(1/3) - 1 + 2/(9 * nu)))^3)
    return(max_value)
  } else {
    return(chi_square)
  }
}

# Initialize a list to store transformed chi-square values
transformed_chisq_values <- list()

# Calculate and store transformed chi-square values
for (i in seq_along(contingency_tables)) {
  table <- contingency_tables[[i]]
  chi_square <- chisq_values[[i]]
  df <- sum(attr(table, "dim")) - 1  # Degrees of freedom is the sum of dimensions of the contingency table minus 1
  transformed_chisq_values[[i]] <- transform_chi_square(chi_square, df)
}

# Rename transformed chi-square values
names(transformed_chisq_values) <- paste("W2(", names(chisq_values), ")", sep = "")

# Print transformed chi-square test statistics
print(transformed_chisq_values)
