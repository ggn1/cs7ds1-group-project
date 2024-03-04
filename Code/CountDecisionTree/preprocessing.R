# Import libraries.
library(rpart)
library(dplyr)
library(ggplot2)
library(partykit)

# GETTING DATA

# Set workinf directory to this one.
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS1_DataAnalytics/Project")

# Load and view data.
data <- read.csv("Data/aviation_data.csv", header=TRUE, sep= ",")
str(data)

# Extract useful features only and view the data.
data <- as.data.frame(data[, c('Injury.Severity', 'Aircraft.damage', 'Aircraft.Category', 'Amateur.Built', 'Number.of.Engines', 'Engine.Type', 'Weather.Condition', 'Broad.phase.of.flight', 'Total.Uninjured')])
head(data)

# PREPROCESSING

# Check length of columns.
for (column in names(data)) {
  print(paste(column, "= ", length(data[, c(column)])))
}
# Obs. All same length.

# Check for NaN values.
num_rows_na <- sum(rowSums(is.na(data)) > 0) # 11140
num_rows_not_na <- nrow(data) - num_rows_na # 77749 
# Obs. 
#   * Dropping na values will result in 77749
#     data points, which is plenty. Hence, we 
#     can drop NAs.
data <- na.omit(data)

# Examining feature category value counts.
vc_injsev <- as.data.frame(table(data["Injury.Severity"]))
vc_airdam <- as.data.frame(table(data["Aircraft.damage"]))
vc_aircat <- as.data.frame(table(data["Aircraft.Category"]))
vc_ambuilt <- as.data.frame(table(data["Amateur.Built"]))
vc_numeng <- as.data.frame(table(data["Number.of.Engines"]))
vc_engtype <- as.data.frame(table(data["Engine.Type"]))
vc_weather <- as.data.frame(table(data["Weather.Condition"]))
vc_flyphase <- as.data.frame(table(data["Broad.phase.of.flight"]))
vc_uninj <- as.data.frame(table(data["Total.Uninjured"]))

# OBS. 
#   * All fields except Number.of.Engines have " " values.
#     There should either be dropped or renamed to "unknown"
#     or "unavailable".
#   * Field Injury.Severity:
#     - Has "Unavailable" values in addition to
#       " " ones. These may mean the same thing.
#     - There are several types of "Fatal".
#       These may be reduced to 1.
#   * Field Aircraft.damage: 
#       - Contains 2 categories " " and "Unknown". 
#         These may be renamed to the same name, "unknown". 
#       - Contains both " " and "Unknown". These may
#         mean the same thing.
#   * Field Aircraft.Category:
#     - Contains 2 categories " " and "Unknown". 
#       These may be renamed to the same name, "unknown".
#     - Contains both " " and "Unknown". These may
#       mean the same thing.
#   * Field Amateur.Built:
#     - Contains both " " values. This might mean "unknown".
#   * Field Number.of.Engines:
#     - All good.
#   * Field Engine.Type:
#     - Contains 2 identifiers for the same category "none".
#     - Contains " ".
#     - Contains values "UNK" and "Unknown" that both mean
#       mean the same thing as "unknown".
#     - Values " ", "unknown" and possibly "none" mean
#       mean the same thing.
#   * Field Weather.Condition:
#     - Of the 5 possible values, 2 are unknown
#       and 1 is " ".
#     - Value "Unk" and "UNK" mean the same as "unknown".
#     - Contains " " values.
#   * Field Broad.phase.of.flight:
#     - Contains " " values.
#     - Contains "Unknown" values.
#   * Field Total.Uninjured:
#     - All good.

# Blank value handling.
num_rows_blank <- sum(rowSums(data == "" | data == " ") > 0) # 71449
num_rows_not_blank <- nrow(data) - num_rows_blank # 6300
# Obs. Dropping empty values still leave us with 6300
#      observations. Hence, this operation is worth trying.
data_no_blank = data[rowSums(data == "" | data == " ") <= 0, ]

# Checking value counts again.
vc_injsev_nb <- as.data.frame(table(data_no_blank["Injury.Severity"]))
vc_airdam_nb <- as.data.frame(table(data_no_blank["Aircraft.damage"]))
vc_aircat_nb <- as.data.frame(table(data_no_blank["Aircraft.Category"]))
vc_ambuilt_nb <- as.data.frame(table(data_no_blank["Amateur.Built"]))
vc_numeng_nb <- as.data.frame(table(data_no_blank["Number.of.Engines"]))
vc_engtype_nb <- as.data.frame(table(data_no_blank["Engine.Type"]))
vc_weather_nb <- as.data.frame(table(data_no_blank["Weather.Condition"]))
vc_flyphase_nb <- as.data.frame(table(data_no_blank["Broad.phase.of.flight"]))
vc_uninj_nb <- as.data.frame(table(data_no_blank["Total.Uninjured"]))

# OBS. 
#   * Field Injury.Severity:
#     - There are several types of "Fatal".
#       These may be reduced to 1.
#   * Field Aircraft.damage: 
#       - All good.
#   * Field Aircraft.Category:
#     - Contains 1 "Unknown" value.
#   * Field Amateur.Built:
#     - All good.
#   * Field Number.of.Engines:
#     - All good.
#   * Field Engine.Type:
#     - Contains 77 "Unknown" values.
#   * Field Weather.Condition:
#     - Contains 58 "UNK".
#   * Field Broad.phase.of.flight:
#     - Contains 52 "Unknown" values.
#   * Field Total.Uninjured:
#     - All good.

# Handle "Unkown" values.
num_rows_unk <- sum(rowSums(data_no_blank == "Unknown" | data_no_blank == "UNK") > 0) # 180
num_rows_not_unk <- nrow(data_no_blank) - num_rows_unk # 6120
# Obs. Dropping unknown values leave us with 6120
#      data points which is a large number. Hence, 
#      rows with at least one unknown value, may be dropped.
data_no_blank_unk <- data_no_blank[rowSums(data_no_blank == "Unknown" | data_no_blank == "UNK") <= 0, ]

# Checking value counts again.
vc_injsev_nbu <- as.data.frame(table(data_no_blank_unk["Injury.Severity"]))
vc_airdam_nbu <- as.data.frame(table(data_no_blank_unk["Aircraft.damage"]))
vc_aircat_nbu <- as.data.frame(table(data_no_blank_unk["Aircraft.Category"]))
vc_ambuilt_nbu <- as.data.frame(table(data_no_blank_unk["Amateur.Built"]))
vc_numeng_nbu <- as.data.frame(table(data_no_blank_unk["Number.of.Engines"]))
vc_engtype_nbu <- as.data.frame(table(data_no_blank_unk["Engine.Type"]))
vc_weather_nbu <- as.data.frame(table(data_no_blank_unk["Weather.Condition"]))
vc_flyphase_nbu <- as.data.frame(table(data_no_blank_unk["Broad.phase.of.flight"]))
vc_uninj_nbu <- as.data.frame(table(data_no_blank_unk["Total.Uninjured"]))

# OBS. 
#   * Field Injury.Severity:
#     - There are several types of "Fatal".
#       These may be reduced to 1.
#   * Field Aircraft.damage: 
#       - All good.
#   * Field Aircraft.Category:
#     - All good.
#   * Field Amateur.Built:
#     - All good.
#   * Field Number.of.Engines:
#     - All good.
#   * Field Engine.Type:
#     - All good.
#   * Field Weather.Condition:
#     - All good.
#   * Field Broad.phase.of.flight:
#     - All good.
#   * Field Total.Uninjured:
#     - All good.
data_clean <- data_no_blank_unk %>%
              mutate(Injury.Severity = gsub("Fatal.*", "Fatal", Injury.Severity))
# Checking value count of Injury.Severity again.
print(table(data_clean["Injury.Severity"])) # All good.

# Splitting data into X and y.
X <- data_clean[, -which(names(data_clean) == "Total.Uninjured")]
y <- data_clean$Total.Uninjured

# View data.
print(summary(y)) # Mean = 3.437
print(paste("Variance:", var(y))) # Variance = 525.528
print(paste("No. of 0s:", sum(y == 0)))
print(paste("No. of non 0s:", sum(y != 0)))
non_numeric_rows <- y[!complete.cases(y) & !sapply(y, is.numeric), ]
print('Y Value Counts:')
print(table(y))
y_bar <- barplot(table(y))
# Obs. 
#   * Since variance >> mean, it's likely that data 
#     is over-dispersed. However, this needs to be 
#     confirmed under models like Poisson.
#   * There are quite a few values of

# # Save cleaned data.
# write.csv(data_clean, file = "data_clean.csv", row.names = FALSE)
