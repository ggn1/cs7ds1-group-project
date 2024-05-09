# Evaluate the models using 10-fold cross validation
# ------------------------------------------------------------------------------

library(rstudioapi)
library(fitdistrplus)
library(partykit)
library(pscl)
library(ggplot2)

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

cv10fold <- function(model_type, data) {
  #Randomly shuffle the data
  data_shuffled<-data[sample(nrow(data)),]
  
  #Create 10 equally size folds
  fold_idx <- cut(seq(1,nrow(data_shuffled)),breaks=10,labels=FALSE)
  
  mse.train <- list()
  mae.train <- list()
  mse.test <- list()
  mae.test <- list()
  dispersion <- list()
  expected.zero.ratio <- list()
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    # Split into train and test data
    testIndexes <- which(fold_idx==i,arr.ind=TRUE)
    testData <- data_shuffled[testIndexes, ]
    trainData <- data_shuffled[-testIndexes, ]
    
    if (model_type == 'poisson') {
      model <- glm(absences ~ ., data=trainData, family="poisson")
    } 
    else if (model_type == 'negbin') {
      model <- glm.nb(absences ~ ., data=trainData)
    } 
    else if (model_type == 'zip') {
      model <- zeroinfl(absences ~ ., data=trainData, dist="poisson")
    } 
    else if (model_type == 'zinb') {
      model <- zeroinfl(absences ~ ., data=trainData, dist="negbin")
    } 
    else if (model_type == 'hurdle_p') {
      model <- hurdle(absences ~ ., data=trainData, dist="poisson")
    } 
    else if (model_type == 'hurdle_negbin') {
      model <- hurdle(absences ~ ., data = trainData, dist = "negbin")
    } 
    else if (model_type == 'mob_p') {
      fit_poisson <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
        glm(formula = y ~ x, family = poisson(link = "log"), start = start, ...)
      }
      model <- mob(absences ~ . | ., data = data, fit = fit_poisson)
    } 
    else if (model_type == 'mob_negbin') {
      fit_negbin <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
        glm.nb(y ~ x, start = start, ...)
      }
      model <- mob(absences ~ . | ., data = data, fit = fit_negbin)
    } 
    else { # Invalid model.
      return (-1)
    }
    
    # Calculate train accuracy
    mse.train <- append(mse.train, mean(residuals(model, type = "response")^2))
    mae.train <- append(mae.train, mean(abs(residuals(model, type = "response"))))
    
    # Calculate test accuracy
    test.residual <- testData$absences - predict(model, newdata = testData)
    mse.test <- append(mse.test, mean(test.residual^2))
    mae.test <- append(mae.test, mean(abs(test.residual)))
    
    # Calculate dispersion
    if (model_type %in% c("mob_p", "mob_negbin")){
      # If tree model, get mean dispersion of terminal nodes
      dispersion <- mean(unlist(nodeapply(model, ids = nodeids(model, terminal = TRUE), 
                                          FUN = function(x) sum(residuals(x$info$object, type = "pearson")^2)/x$info$object$df.residual)))
    }
    else{
      dispersion <- append(dispersion, sum(residuals(model, type = "pearson")^2) / model$df.residual)
    }
    
    # Calculate ratio of expected/observed zeros
    if (model_type == "poisson"){
      expected.zero <- round(sum(dpois(0, fitted(model))))
    }
    else if (model_type == "negbin"){
      expected.zero <- round(sum(dnbinom(0, mu = fitted(model), size = model$theta)))
    }
    else if (model_type %in% c("zip", "zinb", "hurdle_p", "hurdle_negbin")){
      expected.zero <- round(sum(predict(model, type = "prob")[,1]))
    }
    else if (model_type %in% c("mob_p", "mob_negbin")){
      node_zeros <- function(x){
        model <- x$info$object
        if(model$family$family == "poisson"){
          round(sum(dpois(0, fitted(model))))
        }
        else if (startsWith(model$family$family, "Negative Binomial")){
          round(sum(dnbinom(0, mu = fitted(model), size = model$theta)))
        }
      }
      expected.zero <- sum(unlist(nodeapply(model, ids = nodeids(model, terminal = TRUE),
                                            FUN = node_zeros)))
    }
    observed.zero <- sum(trainData$absences == 0)
    expected.zero.ratio <- append(expected.zero.ratio, expected.zero/observed.zero)
  }
  results <- cbind(unlist(mse.train), unlist(mae.train), unlist(mse.test), unlist(mae.test),
                   unlist(dispersion), unlist(expected.zero), unlist(expected.zero.ratio))
  return(data.frame(results))
}

# Calculate metrics for each model using 10-fold cross validation
res.pois <- cv10fold("poisson", data)
res.nb <- cv10fold("negbin", data)
res.zip <- cv10fold("zip", data)
res.zinb <- cv10fold("zinb", data)
res.hurdle.p <- cv10fold("hurdle_p", data)
res.hurdle.nb <- cv10fold("hurdle_negbin", data)
res.mob.p <- cv10fold("mob_p", data)
res.mob.nb <- cv10fold("mob_negbin", data)

# Aggregate results
mean.results <- rbind("Poisson" = colMeans(res.pois),
                      "NB" = colMeans(res.nb),
                      "ZIP" = colMeans(res.zip),
                      "ZINB" = colMeans(res.zinb),
                      "Hurdle Poisson" = colMeans(res.hurdle.p),
                      "Hurdle NB" = colMeans(res.hurdle.nb),
                      "MOB Poisson" = colMeans(res.mob.p),
                      "MOB NB" = colMeans(res.mob.nb))
colnames(mean.results) <- c("mse.train_mean", "mae.train_mean", "mse.test_mean", "mae.test_mean",
                            "dispersion_mean", "expected.zero_mean","expected.zero.ratio_mean")

sd.results <- rbind("Poisson" = apply(res.pois, 2, sd),
                      "NB" = apply(res.nb, 2, sd),
                      "ZIP" = apply(res.zip, 2, sd),
                      "ZINB" = apply(res.zinb, 2, sd),
                      "Hurdle Poisson" = apply(res.hurdle.p, 2, sd),
                      "Hurdle NB" = apply(res.hurdle.nb, 2, sd),
                      "MOB Poisson" = apply(res.mob.p, 2, sd),
                      "MOB NB" = apply(res.mob.nb, 2, sd))
colnames(sd.results) <- c("mse.train_sd", "mae.train_sd", "mse.test_sd", "mae.test_sd",
                            "dispersion_sd", "expected.zero_sd","expected.zero.ratio_sd")

results <- data.frame(cbind(mean.results, sd.results))
results$model <- rownames(results)
results$id <- 1:nrow(results)
results

# Create plots

# MSE training data
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=mse.train_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=mse.train_mean-mse.train_sd, 
                    ymax=mse.train_mean+mse.train_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=mse.train_mean, label = signif(mse.train_mean)), 
             size=3, alpha=0.7)+ 
  labs(x = "Model", y = "MSE (train)")

# MSE test data
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=mse.test_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=mse.test_mean-mse.test_sd, 
                    ymax=mse.test_mean+mse.test_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=mse.test_mean, label = signif(mse.test_mean)), 
             size=3, alpha=0.7)+ 
  labs(x = "Model", y = "MSE (test)")

# MAE training data
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=mae.train_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=mae.train_mean-mae.train_sd, 
                    ymax=mae.train_mean+mae.train_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=mae.train_mean/2, label = signif(mae.train_mean)), 
             size=3, alpha=0.7)+ 
  labs(x = "Model", y = "MAE (train)")

# MSE test data
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=mae.test_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=mae.test_mean-mae.test_sd, 
                    ymax=mae.test_mean+mae.test_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=mae.test_mean/2, label = signif(mae.test_mean)), 
             size=3, alpha=0.7)+ 
  labs(x = "Model", y = "MAE (test)")

# Dispersion
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=dispersion_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=dispersion_mean-dispersion_sd, 
                    ymax=dispersion_mean+dispersion_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=dispersion_mean/2, label = signif(dispersion_mean)), 
             size=3, alpha=0.7)+ 
  labs(x = "Model", y = "Dispersion")+
  geom_hline(yintercept=1, color = "red")

# Zero-inflation
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=expected.zero.ratio_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=expected.zero.ratio_mean-expected.zero.ratio_sd, 
                    ymax=expected.zero.ratio_mean+expected.zero.ratio_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=expected.zero.ratio_mean/2, label = signif(expected.zero.ratio_mean)), 
             size=3, alpha=0.7)+ 
  labs(x = "Model", y = "Ratio expected/oberved zeros")+
  geom_hline(yintercept=1, color = "red")

