# Set working directory as directory of the R script
# -> so we don't need to manually set the path of the data
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print( getwd() )

#Import data
AviationData <- read.csv("Data/aviation_data.csv", na.strings=c("","NA"))
View(AviationData)

# Select relevant columns
AviationData <- AviationData[, c("Total.Uninjured","Number.of.Engines", "Engine.Type", "Purpose.of.flight", "Weather.Condition", "Broad.phase.of.flight")]

# Remove Nans
AviationData <- na.omit(AviationData)
View(AviationData)

# Load features
X <- AviationData[, c("Number.of.Engines", "Engine.Type", "Purpose.of.flight", "Weather.Condition", "Broad.phase.of.flight")]
Y <- AviationData$Total.Uninjured

library(MASS)
negbin_model <- glm.nb(Y ~ ., data = X)
summary(negbin_model)

print(residuals(negbin_model))

# Calculate MSE
res <- residuals(negbin_model)
mse <- mean(res^2)
print(mse)

# Calculate residuals using predictions
predictions <- predict(negbin_model, X, type = "response")
res2 <- Y - predictions
mse2 <- mean(res2^2)
print(mse2)

# Plot residuals
plot(Y, res, main="Y vs. Residuals", xlab="Y", ylab="residual", pch=19)

plot(Y, res2, main="Y vs. Residuals (using Predictions)", xlab="Y", ylab="residual", pch=19)

# Plot predictions
plot(Y, predictions, main="Y vs. Predictions", xlab="Y", ylab="predicted Y", pch=19)
