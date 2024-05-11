# Import libraries.
library(dplyr)
library(caret) # For label encoding.
library(ggplot2)

# GETTING DATA

# Set working directory to this one.
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project/Data")

# Load and view data.
data <- read.csv("./data.csv", header=TRUE, sep= ",")
str(data)

# Extract useful features only and view the data.
data <- as.data.frame(data[, c('absences', 'course', 'school', 'sex', 'age', 'address', 'famsize', 'Medu', 'Fedu', 'Mjob', 'Fjob', 'guardian', 'traveltime', 'studytime', 'failures', 'schoolsup', 'famsup', 'activities', 'higher', 'internet', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc', 'Walc', 'health', 'G3', 'nursery')])
head(data)

# EDA

# Check length of columns.
for (column in names(data)) {
  print(paste(column, "= ", length(data[, c(column)])))
}
# Obs. All same length.

# Check for NaN values.
num_rows_na <- sum(rowSums(is.na(data)) > 0) # 0
num_rows_not_na <- nrow(data) - num_rows_na # 1044 
# Obs. No missing values.

# Examining feature category value counts.
for (col in names(data)) {
  print(paste("Value Counts - Column '", col, "'"))
  print(as.data.frame(table(data[col])))
  cat("\n")
}

# OBS. All good.

# Blank value handling.
num_rows_blank <- sum(rowSums(data == "" | data == " ") > 0) # 0
num_rows_not_blank <- nrow(data) - num_rows_blank # 0
# Obs. No blank values.

# View data.
print(summary(data$absences)) # Min = 0, Q1 = 0, Median = 2, Mean = 4.435, Q3 = 6, Max = 75
print(paste("Variance:", var(data$absences))) # Variance = 38.564
print(paste("No. of 0 counts:", sum(data$absences == 0))) # 359
print(paste("No. of non 0 counts:", sum(data$absences != 0))) # 685
# y_bar <- barplot(table(data$absences))
print(paste('No. of rows =', length(data$absences)))
# Obs. 
#   * Since variance >> mean, it's likely that data 
#     is over-dispersed.

# # Save cleaned data.
# write.csv(data, file = "./data_clean.csv", row.names = FALSE)