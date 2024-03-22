Aviation Data set
X:
=> Injury.Severity
=> Aircraft.damage
=> Aircraft.Category
=> Amateur.Built
=> Number.of.Engines
=> Engine.Type
=> Weather.Condition
=> Broad.phase.of.flight
Y:
=> Total.Uninjured

Y = Total.Uninjured
X = [Injury.Severity, Aircraft.damage, Aircraft.Category, Amateur.Built, Number.of.Engines, Engine.Type, Weather.Condition, Broad.phase.of.flight]



##Main effect detection

```{r read data}
library(readr)
library(stats)
AviationData <- read_csv("data_clean.csv") 
```

# Step 1: Fit the Poisson regression model
```{r}
model <- glm(Total.Uninjured ~ Injury.Severity + Aircraft.damage + Aircraft.Category + Amateur.Built + Number.of.Engines + Engine.Type + Weather.Condition + Broad.phase.of.flight, 
             data = AviationData, family = poisson)

```

# covariates as factor
```{r}
cols_to_factor <- c("Injury.Severity", "Aircraft.damage", "Aircraft.Category", "Amateur.Built", 
                    "Number.of.Engines", "Engine.Type", "Weather.Condition", "Broad.phase.of.flight")
AviationData[cols_to_factor] <- lapply(AviationData[cols_to_factor], factor)
```

# Step 2: Calculate the residuals of the model
```{r}
residuals <- residuals(model, type = "pearson")
#residuals
```
residuals result example

1            2            3            4 
-0.96360527  -0.50225239  -0.98800010  -0.51496751 
These residuals represent the differences between the predicted values and the observed values in the model. 
They are Pearson residuals, a commonly used type of residual in Poisson regression models.
In this vector of residuals, each number corresponds to the residual of the corresponding observation. 
Positive values indicate that the model underestimated the response variable (Total.Uninjured), while negative values indicate that the model overestimated the response variable. 
The larger the absolute value of the residual, the greater the difference between the model's predicted values and the actual observed values.

#Step3: Split the residuals into two groups: positive and non-positive
(already automatically split in two group in contingency table, so we don't need specify here)
In the paper


#Step4: Contingency table function
Construct a two-way 2 × 4 contingency table if Poisson or negative binomial regression is used.
if the zero-inflated or hurdle model is used, construct a 4 × 4 contingency table with levels as columns and signs of the partial score residuals as rows.

All covariate are categorical variable, then using the categories of the covariates to form the columns of the contingency table.
For Poisson regression, we construct a 2 × 4 contingency table for each covariates
```{r}
# Define a function to calculate Pearson chi-squared test statistic
calculate_chi_squared <- function(var, residuals) {
  # Create a contingency table for the variable
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
    chi_squared_df1 <- max(0, (7/9) + sqrt(nu) * ((chi_squared / nu)^(1/3) - 1 + 2/(9*nu)))^3
  } else {
    chi_squared_df1 <- chi_squared
  }
  
  return(list(contingency_table = contingency_table, chi_squared_df1 = chi_squared_df1))
}
```

Note1: Create a contingency table for var" means to create a contingency table for the given variable (var). A contingency table is a tabular representation used to analyze the relationship between two or more categorical variables.

Note2: Why using  4 × 4 contingency table for zero-inflated or hurdle model?

For a categorical covariate, if we're constructing a contingency table using the residuals from the count part of the model, we may have four possible combinations of signs of residuals: (-, -), (-, +), (+, -), and (+, +). This is because each observation can either have a negative or positive residual for the count part of the model, leading to four possible combinations. Hence, we construct a 4 × 4 contingency table.



# Contingency_table and Chi-squared (df=1) using Wilson-Hilferty approximation
```{r }
#Injury.Severity
injury_severity_result <- calculate_chi_squared("Injury.Severity", residuals)
print("Injury.Severity Contingency Table:")
print(injury_severity_result$contingency_table)
print(injury_severity_result$chi_squared_df1)

#Aircraft.damage
aircraft_damage_result <- calculate_chi_squared("Aircraft.damage", residuals)
print("Aircraft.damage Contingency Table:")
print(aircraft_damage_result$contingency_table)
print(aircraft_damage_result$chi_squared_df1)

#Aircraft.Category
aircraft_category_result <- calculate_chi_squared("Aircraft.Category", residuals)
print("Aircraft.Category Contingency Table:")
print(aircraft_category_result$contingency_table)
print(aircraft_category_result$chi_squared_df1)

#Amateur.Built
amateur_built_result <- calculate_chi_squared("Amateur.Built", residuals)
print("Amateur.Built Contingency Table:")
print(amateur_built_result$contingency_table)
print(amateur_built_result$chi_squared_df1)

# Number.of.Engines
engines_result <- calculate_chi_squared("Number.of.Engines", residuals)
print("Number.of.Engines Contingency Table:")
print(engines_result$contingency_table)
print(engines_result$chi_squared_df1)

#Engine.Type
engine_type_result <- calculate_chi_squared("Engine.Type", residuals)
print("Engine.Type Contingency Table:")
print(engine_type_result$contingency_table)
print(engine_type_result$chi_squared_df1)

# Weather.Condition
weather_result <- calculate_chi_squared("Weather.Condition", residuals)
print("Weather.Condition Contingency Table:")
print(weather_result$contingency_table)
print(weather_result$chi_squared_df1)

# Broad.phase.of.flight
broad_phase_result <- calculate_chi_squared("Broad.phase.of.flight", residuals)
print("Broad.phase.of.flight Contingency Table:")
print(broad_phase_result$contingency_table)
print(broad_phase_result$chi_squared_df1)
```


In the generated contingency table:

- "FALSE" corresponds to the situation where the residual is non-positive.
- "TRUE" corresponds to the situation where the residual is positive.

For instance, in the "Reciprocating Turbo Fan" column:
  
  - The value under the "FALSE" row (29632) represents the count of observations where the residual is non-positive (e.g., 0 or negative).  
  - The value under the "TRUE" row (18993) represents the count of observations where the residual is positive.

By comparing the signs of residuals with the engine types, you can further analyze how the residuals are associated with different types of engines.

