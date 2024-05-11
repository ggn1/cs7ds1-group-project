# Import libraries.
require(ggplot2)
require(gplots)
require(rstudioapi) # For dynamic setwd(...)

# SET WORKING DIRECTORY
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print(getwd())

# Load and view data.
data <- read.csv("../Data/data.csv", header=TRUE, sep= ",")
str(data)
head(data)

# EXPLORING CONTINUOUS VARIABLES

# Extract continuous variables only.
cov_name_types <- get_cat_con_all(data, response_variable = "abcenses")
cov_con <- c()
for (cov_name_type in cov_name_types) {
  cov_name <- cov_name_type[[1]]
  cov_type <- cov_name_type[[2]]
  if (cov_type == "con") {
    cov_con <- c(cov_con, cov_name)
  }
}
data_con <- data[, cov_con]

# Plot correlation matrix.
correlation_matrix <- cor(data_con)
heatmap.2(
  correlation_matrix,
  col = colorRampPalette(c("blue", "white", "red"))(100),  # Define color palette
  symm = FALSE,  # Do not show symmetric scale
  margins = c(10, 10),  # Add margins for row and column names
  scale = "none",  # Do not scale data
  main = "Correlation Matrix",  # Title of the plot
  cex.main = 1.2,  # Font size of the title
  key = TRUE,  # Show color key (legend)
  keysize = 1.5,  # Size of the color key
  key.title = NA,  # Do not show color key title
  trace = "none",  # Do not show trace lines
  revC = TRUE,  # Reverse color scale
  dendrogram = "none" # Do not calculate dendrograms
)
# OBSERVATIONS:
# * Drop G1, G2, Fedu, goout, Dalc.

# EXPLORING CATEGORICAL VARIABLES

# Extract categorical variables only.
cov_cat <- c()
for (cov_name_type in cov_name_types) {
  cov_name <- cov_name_type[[1]]
  cov_type <- cov_name_type[[2]]
  if (cov_type == "cat") {
    cov_cat <- c(cov_cat, cov_name)
  }
}
data_cat <- data[, cov_cat]
data_cat[['absences_cat']] <- data$absences
data_cat[['absences_cat']][
  which(data$absences >= 8)
] = Inf
data_cat[['absences_cat']] <-as.character(
  data_cat[['absences_cat']]
)
data_cat[['absences_cat']][
  which(data_cat[['absences_cat']] == "Inf")
] = "8+"
data_cat[['absences_cat']] <- factor(
  data_cat[['absences_cat']]
)

# Plot categorical features against absences,
# 5 features + absences at a time.
layout_matrix <- matrix(
  c(1, 2, 3, 4, 5, 6), 
  nrow = 2, byrow = TRUE
)
layout(layout_matrix, heights = c(1, 1))
for (characteristic in c(
  "course", "school", "sex", 
  "address", "famsize", "absences_cat"
)) {
  mosaicplot(
    table(
      data_cat[, characteristic], 
      data_cat$absences_cat
    ), 
    main = NULL,
    xlab = characteristic,
    ylab = "absences",
  )
}
layout(1)

# EXTRACT USEFUL COLUMNS ONLY
data <- as.data.frame(data[, c('absences', 'course', 'school', 'sex', 'age', 'address', 'famsize', 'Medu', 'Fedu', 'Mjob', 'Fjob', 'guardian', 'traveltime', 'studytime', 'failures', 'schoolsup', 'famsup', 'activities', 'higher', 'internet', 'romantic', 'famrel', 'freetime', 'goout', 'Dalc', 'Walc', 'health', 'G3', 'nursery', 'reason')])

# CHECKS

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

# SAVE DATA
# write.csv(data, file = "./data_clean.csv", row.names = FALSE)