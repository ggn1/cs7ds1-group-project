# Import libraries.
library(rpart)
library(dplyr)
library(ggplot2) # CART
library(partykit) # CART
library(pscl) # Zero Inflated Poisson Model

# Set working directory to this one.
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Code")

# Load data.
data <- read.csv("../Data/data_clean.csv", header=TRUE, sep= ",")
# X <- data[, -which(names(data) == "Total.Uninjured")]
# y <- data["Total.Uninjured"]

# Fit a Zero-Inflated Poisson (ZIP) model.
model_zip <- zeroinfl(Total.Uninjured ~ ., data = data, dist = "poisson")
summary(model_zip)