# Require packages.
library(rpart)
library(rpart.plot)
library(ggplot2)

# Load data.
data(msleep)
str(msleep)
# Extract useful data from the loaded data set.
df = msleep[, c('vore', 'sleep_total', 'brainwt', 'bodywt')]
str(df)
head(df)

# Build (fit) tree.
m1 <- rpart(sleep_total ~ ., data=df, method="anova")
rpart.plot(m1, type=3, digits=3, fallen.leaves=TRUE)

# Make predictions.
p1 <- predict(m1, df)
p1

# Performance evaluation (MAE)
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
MAE(df$sleep_total, p1)
