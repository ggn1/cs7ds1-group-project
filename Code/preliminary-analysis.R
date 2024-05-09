library(rstudioapi)
library(MASS)
library(glmmTMB)
library(performance)
library(fitdistrplus)


# Set working directory as directory of the R script
# -> so we don't need to manually set the path of the data
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print( getwd() )

# Load data.
data <- read.csv("../Data/student-mat-por.csv", header=TRUE, sep= ",")

# Drop variables to be excluded
data <- subset(data, select = -c(reason, paid, nursery, Pstatus, G1, G2))
Y <- unlist(data["absences"])

# Calculate data variance
data_var <- var(Y)

barplot(table(Y), xlab='absences', ylab='Frequency')
hist(Y, xlab='absences', ylab='Frequency', breaks=50, main="")

# Test for over-dispersion in Poisson
dist <- fitdistr(Y, densfun="poisson")
expect_var <- dist$estimate["lambda"]
cat("Data variance: ", data_var, "Expected variance: ", expect_var)
if(data_var > expect_var){print("Model is over-dispersed")}else{print("Model is not over-dispersed")}

# Test for zero-inflation in Poisson
m <- glm(absences ~ ., data = data, family = poisson)
check_zeroinflation(m)

# Test for over-dispersion in Negative Binomial
dist <- fitdist(Y, "nbinom")
mu <- dist$estimate[["mu"]]
shape <- dist$estimate[["size"]]
expect_var <- mu*(1+shape*mu)
cat("Data variance: ", data_var, "Expected variance: ", expect_var)
if(data_var > expect_var){print("Model is over-dispersed")}else{print("Model is not over-dispersed")}

# Test for zero-inflation in Negative Binomial
m <- glm.nb(absences ~ ., data = data)
check_zeroinflation(m)
check_overdispersion(m)