# Code based on examples in:
# https://stats.oarc.ucla.edu/wp-content/uploads/2024/03/Zero_inf_2024_2.html#1

library(rstudioapi)
#library(MASS)
#library(glmmTMB)
library(performance)
library(fitdistrplus)
library(partykit)
library(pscl)
library(ggplot2)
library(MASS)

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
# Turn columns into factor where appropriate
cols <- c("course", "school", "sex", "address", "famsize", "Medu", "Fedu",    
          "Mjob", "Fjob", "guardian", "traveltime", "studytime",  "failures",
          "schoolsup", "famsup", "activities", "higher", "internet", "romantic",
          "famrel", "freetime", "goout", "Dalc", "Walc", "health"  )
data[cols] <- lapply(data[cols], as.factor)  ## as.factor() could also be used
View(data)

# Calculate data variance and zeros
Y <- unlist(data["absences"])
data_var <- var(Y)
data_zeros <- sum(Y == 0)

hist(Y, xlab='absences', ylab='Frequency', breaks=50, main="")

# Define a formula
n <- names(data)
formula <- paste("absences ~", paste(n[!n %in% "absences"], collapse = " + "))

# Fit Poisson model
m.pois <- glm(formula = formula, family = poisson(link = "log"), data = data)
summary(m.pois)

# Fit negative binomial model
m.nb <- glm.nb(formula, data = data, link = log)
summary(m.nb)


# Define a formula with count component and zero component
formula2 <- paste("absences ~", paste(n[!n %in% "absences"], collapse = " + "), " | ", paste(n[!n %in% "absences"], collapse = " + "))

# Fit zero-inflated Poisson model
m.zeroinf.pois <- zeroinfl(absences ~ course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3  |  
                                      course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3, 
                           data = data, dist = "poisson")
summary(m.zeroinf.pois)

# Fit zero-inflated negative binomial model
m.zeroinf.nb <- zeroinfl(absences ~ course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3  |  
                           course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3, 
                         data = data, dist = "negbin")
summary(m.zeroinf.nb)


# Fit Poisson hurdle model
m.hurdle.pois <- hurdle(absences ~ course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3  |  
                          course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3, 
                        data = data, dist = "poisson")
summary(m.hurdle.pois)

# Fit negative binomial hurdle model
m.hurdle.nb <- hurdle(absences ~ course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3  |  
                        course + school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3, 
                      data = data, dist = "negbin")
summary(m.hurdle.nb)

# Fit Poisson MOB model
fit_poisson <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  glm(formula = y ~ x, family = poisson(link = "log"), start = start, ...)
}
m.mob.pois <- mob(absences ~ . | .,
                 data = data, fit = fit_poisson)#, control = mob_control(minsize = 30))


# Fit negative binomial MOB model 
fit_negbin <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  glm.nb(y ~ x, start = start, ...)
}
m.mob.nb <- mob(absences ~ course |  school + sex + age + address + famsize + Medu + Fedu + Mjob + Fjob + guardian + traveltime + studytime + failures + schoolsup + famsup + activities + higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + G3,
                 data = data, fit = fit_negbin)#, control = mob_control(minsize = 30))

# Fit CORE model


# Calculate accuracy
MSE <- c(mean(residuals(m.pois, type = "response")^2),
         mean(residuals(m.nb, type = "response")^2),
         mean(residuals(m.zeroinf.pois, type = "response")^2),
         mean(residuals(m.zeroinf.nb, type = "response")^2),
         mean(residuals(m.hurdle.pois, type = "response")^2),
         mean(residuals(m.hurdle.nb, type = "response")^2),
         mean(residuals(m.mob.pois, type = "response")^2),
         mean(residuals(m.mob.nb, type = "response")^2)#,
         #NULL
         )

MAE <- c(mean(abs(residuals(m.pois, type = "response"))),
         mean(abs(residuals(m.nb, type = "response"))),
         mean(abs(residuals(m.zeroinf.pois, type = "response"))),
         mean(abs(residuals(m.zeroinf.nb, type = "response"))),
         mean(abs(residuals(m.hurdle.pois, type = "response"))),
         mean(abs(residuals(m.hurdle.nb, type = "response"))),
         mean(abs(residuals(m.mob.pois, type = "response"))),
         mean(abs(residuals(m.mob.nb, type = "response")))#,
         #NULL
)

# Compute dispersion statistics for different models

Dispersion <- c(sum(residuals(m.pois, type = "pearson")^2) / m.pois$df.residual,
                sum(residuals(m.nb, type = "pearson")^2) / m.nb$df.residual,
                sum(residuals(m.zeroinf.pois, type = "pearson")^2) / m.zeroinf.pois$df.residual,
                sum(residuals(m.zeroinf.nb, type = "pearson")^2) / m.zeroinf.nb$df.residual,
                sum(residuals(m.hurdle.pois, type = "pearson")^2) / m.hurdle.pois$df.residual,
                sum(residuals(m.hurdle.nb, type = "pearson")^2) / m.hurdle.nb$df.residual,
                mean(unlist(nodeapply(m.mob.pois, ids = nodeids(m.mob.pois, terminal = TRUE), 
                               FUN = function(x) sum(residuals(x$info$object, type = "pearson")^2)/x$info$object$df.residual))),
                mean(unlist(nodeapply(m.mob.nb, ids = nodeids(m.mob.nb, terminal = TRUE), 
                               FUN = function(x) sum(residuals(x$info$object, type = "pearson")^2)/x$info$object$df.residual)))
                #sum(residuals(m.mob.nb, type = "pearson")^2) / m.mob.nb$df.residual
                #NULL
                )

# Dispersion for each end node
# nodeapply(m.mob.nb, ids = nodeids(m.mob.nb, terminal = TRUE), FUN = function(x) sum(residuals(x$info$object, type = "pearson")^2)/x$info$object$df.residual)


#Predicted number of zeros

Observed.zero = sum(data$absences == 0)

f_zeros <- function(x){
  model <- x$info$object
  # If Poisson
  if(model$family$family == "poisson"){
    round(sum(dpois(0, fitted(model))))
  }
  else if (startsWith(model$family$family, "Negative Binomial")){
    round(sum(dnbinom(0, mu = fitted(model), size = model$theta)))
  }
}

Expected.zero <- round(c("Pois" = sum(dpois(0, fitted(m.pois))),
                         "NB" = sum(dnbinom(0, mu = fitted(m.nb), size = m.nb$theta)),
                         "Zero-inflated Poisson" = sum(predict(m.zeroinf.pois, type = "prob")[,1]),
                         "Zero-inflated NB" = sum(predict(m.zeroinf.nb, type = "prob")[,1]),
                         "Hurdle Poisson" = sum(predict(m.hurdle.pois, type = "prob")[,1]),
                         "Hurdle NB" = sum(predict(m.hurdle.nb, type = "prob")[,1]),
                         "MOB Poisson" = sum(unlist(nodeapply(m.mob.nb, ids = nodeids(m.mob.nb, terminal = TRUE), 
                                                              FUN = f_zeros))),
                         "MOB NB" = sum(unlist(nodeapply(m.mob.nb, ids = nodeids(m.mob.nb, terminal = TRUE), 
                                                         FUN = f_zeros)))
                         #"CORE" = NULL
                       ))

# Expected zeros for each end node
# nodeapply(m.mob.nb, ids = nodeids(m.mob.nb, terminal = TRUE), FUN = function(x) round(sum(dpois(0, fitted(x$info$object)))))

Zeros.ratio <- Expected.zero / Observed.zero  

results <- data.frame(cbind(MSE, MAE, Expected.zero, Zeros.ratio, Dispersion))
results$model <- rownames(results)
results$id <- 1:nrow(results)

results

ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=Expected.zero), stat="identity", fill="darkgray")+
  geom_text(aes(x=reorder(model, id), y=Expected.zero,label = signif(Expected.zero)), nudge_y = -25)+
  geom_hline(yintercept=Observed.zero, color = "red")

ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=Zeros.ratio), stat="identity", fill="darkgray")+
  geom_text(aes(x=reorder(model, id), y=Zeros.ratio,label = signif(Zeros.ratio)), nudge_y = -0.05)+
  geom_hline(yintercept=1, color = "red")

ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=Dispersion), stat="identity", fill="darkgray")+
  geom_text(aes(x=reorder(model, id), y=Dispersion,label = signif(Dispersion)), nudge_y = 0.35)+
  geom_hline(yintercept=1, color = "red")

ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=MSE), stat="identity", fill="darkgray")+
  geom_text(aes(x=reorder(model, id), y=MSE,label = signif(MSE)), nudge_y = 0.3)

ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=MAE), stat="identity", fill="darkgray")+
  geom_text(aes(x=reorder(model, id), y=MAE,label = signif(MAE)), nudge_y = 0.1)

