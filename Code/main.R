# LOAD PACKAGES
require(rstudioapi) # For dynamic setwd(...)
require(partykit)
require(ggplot2)

# SET WORKING DIRECTORY
current_path = rstudioapi::getActiveDocumentContext()$path 
print(current_path)
setwd(dirname(current_path ))
print(getwd())

# HELPER FUNCTIONS
source("./evaluation.R")

# MAIN

# Load data.
school_absences <- read.csv(
  "../Data/data_clean.csv",
  header=TRUE, sep=","
)
RESPONSE_VARIABLE <- "absences"

# Select subset of data
#colnames(school_absences)
#school_absences <- school_absences[c("absences", "sex", "age", "health", "G3")]


# Shuffle
set.seed(32)
shuffled_data <- school_absences[
  sample(nrow(school_absences)), 
]

# Reset index.
rownames(shuffled_data) <- NULL

# 80/20 train/test split.
index_train <- createDataPartition(
  shuffled_data[RESPONSE_VARIABLE][,], 
  p = 0.8, list = FALSE
)
data_train <- shuffled_data[index_train, ]
data_test <- shuffled_data[-index_train, ]

# Sanitize data for model fitting.
data_sanitized <- sanitize_fit_input(
  data_check = data_train,
  data_apply = list(data_train, data_test),
  response_variable = RESPONSE_VARIABLE
)
data_train <- data_sanitized[[1]]
data_test <- data_sanitized[[2]]



# Calculate metrics for each model using 10-fold cross validation
res.pois <- evaluate_cv("p", data_train)
res.nb <- evaluate_cv("nb", data_train)
res.zip <- evaluate_cv("zip", data_train)
res.zinb <- evaluate_cv("zinb", data_train)
res.hurdle.p <- evaluate_cv("hp", data_train)
res.hurdle.nb <- evaluate_cv("hnb", data_train)
res.mob.p <- evaluate_cv("mob_p", data_train)
res.mob.nb <- evaluate_cv("mob_nb", data_train)
res.core <- evaluate_cv("core", data_train, min_split_pc=0.1, max_depth=-1)

# Aggregate results
mean.results <- data.frame(rbind("Poisson" = colMeans(res.pois, na.rm = TRUE),
                      "NB" = colMeans(res.nb, na.rm = TRUE),
                      "ZIP" = colMeans(res.zip, na.rm = TRUE),
                      "ZINB" = colMeans(res.zinb, na.rm = TRUE),
                      "Hurdle Poisson" = colMeans(res.hurdle.p, na.rm = TRUE),
                      "Hurdle NB" = colMeans(res.hurdle.nb, na.rm = TRUE),
                      "MOB Poisson" = colMeans(res.mob.p, na.rm = TRUE),
                      "MOB NB" = colMeans(res.mob.nb, na.rm = TRUE),
                      "CORE" = colMeans(res.core, na.rm = TRUE)))
colnames(mean.results) <- c("mse.train_mean", "mae.train_mean", "mse.test_mean", "mae.test_mean",
                            "dispersion_mean","expected.zero.ratio_mean")

sd.results <- data.frame(rbind("Poisson" = apply(res.pois, 2, function(x) sd(na.omit(x))),
                    "NB" = apply(res.nb, 2, function(x) sd(na.omit(x))),
                    "ZIP" = apply(res.zip, 2, function(x) sd(na.omit(x))),
                    "ZINB" = apply(res.zinb, 2, function(x) sd(na.omit(x))),
                    "Hurdle Poisson" = apply(res.hurdle.p, 2, function(x) sd(na.omit(x))),
                    "Hurdle NB" = apply(res.hurdle.nb, 2, function(x) sd(na.omit(x))),
                    "MOB Poisson" = apply(res.mob.p, 2, function(x) sd(na.omit(x))),
                    "MOB NB" = apply(res.mob.nb, 2, function(x) sd(na.omit(x))),
                    "CORE" = apply(res.core, 2, function(x) sd(na.omit(x)))))
colnames(sd.results) <- c("mse.train_sd", "mae.train_sd", "mse.test_sd", "mae.test_sd",
                          "dispersion_sd","expected.zero.ratio_sd")

results <- data.frame(cbind(mean.results, sd.results))
results$model <- rownames(results)
results$id <- 1:nrow(results)
results

write.csv(results, "../Results/evaluation_results.csv", row.names=FALSE)

# Create plots

# MSE
train <- results[c("model", "id")]
train$mean <- results$mse.train_mean
train$sd <- results$mse.train_sd
train$data <- as.factor("train")

test <- results[c("model", "id")]
test$mean <- results$mse.test_mean
test$sd <- results$mse.test_sd
test$data <- as.factor("test")

ggplot(rbind(train, test)) +
  geom_bar( aes(x=reorder(model, id), y=mean, fill=data), stat="identity", 
            position = "dodge")+
  geom_errorbar(aes(x=reorder(model, id), ymin=mean-sd, ymax=mean+sd, group=data), 
                width=.2, position=position_dodge(.9)) +
  labs(x = "Model", y = "MSE")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,60))


# MAE
train <- results[c("model", "id")]
train$mean <- results$mae.train_mean
train$sd <- results$mae.train_sd
train$data <- as.factor("train")

test <- results[c("model", "id")]
test$mean <- results$mae.test_mean
test$sd <- results$mae.test_sd
test$data <- as.factor("test")

ggplot(rbind(train, test)) +
  geom_bar( aes(x=reorder(model, id), y=mean, fill=data), stat="identity", 
            position = "dodge")+
  geom_errorbar(aes(x=reorder(model, id), ymin=mean-sd, ymax=mean+sd, group=data), 
                width=.2, position=position_dodge(.9)) +
  labs(x = "Model", y = "MAE")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,5))

# Dispersion
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=dispersion_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=dispersion_mean-dispersion_sd, 
                    ymax=dispersion_mean+dispersion_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=dispersion_mean-dispersion_sd, label = signif(dispersion_mean,4)), 
             alpha=0.7, nudge_y=-0.4)+ 
  labs(x = "Model", y = "Dispersion")+
  geom_hline(yintercept=1, color = "red")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))

# Zero-inflation
ggplot(results) +
  geom_bar( aes(x=reorder(model, id), y=expected.zero.ratio_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(model, id),
                    ymin=expected.zero.ratio_mean-expected.zero.ratio_sd, 
                    ymax=expected.zero.ratio_mean+expected.zero.ratio_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(model, id), y=expected.zero.ratio_mean-expected.zero.ratio_sd,
                 label = signif(expected.zero.ratio_mean,4)), 
             alpha=0.7, nudge_y=-0.07)+ 
  labs(x = "Model", y = "Ratio expected/oberved zeros")+
  geom_hline(yintercept=1, color = "red")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------------------------------------------
# Test different depth limitation
res.core.7 <- evaluate_cv("core", data_train, min_split_pc=0.1, max_depth=7)
res.core.5 <- evaluate_cv("core", data_train, min_split_pc=0.1, max_depth=5)
res.core.3 <- evaluate_cv("core", data_train, min_split_pc=0.1, max_depth=3)

# Aggregate results
mean.results2 <- data.frame(rbind("3" = colMeans(res.core.3),
                                 "5" = colMeans(res.core.5),
                                 "7" = colMeans(res.core.7),
                                 "None" = colMeans(res.core)))
colnames(mean.results2) <- c("mse.train_mean", "mae.train_mean", "mse.test_mean", "mae.test_mean",
                            "dispersion_mean","expected.zero.ratio_mean")

sd.results2 <- data.frame(rbind("3" = apply(res.core.3, 2, sd),
                               "5" = apply(res.core.5, 2, sd),
                               "7" = apply(res.core.7, 2, sd),
                               "None" = apply(res.core, 2, sd)))
colnames(sd.results2) <- c("mse.train_sd", "mae.train_sd", "mse.test_sd", "mae.test_sd",
                          "dispersion_sd","expected.zero.ratio_sd")

results2 <- data.frame(cbind(mean.results2, sd.results2))
results2$depth.limit <- rownames(results2)
results2$id <- 1:nrow(results2)
results2

write.csv(results, "../Results/evaluation_depthlim_results.csv", row.names=FALSE)

# Create plots

# MSE
train <- results2[c("depth.limit", "id")]
train$mean <- results2$mse.train_mean
train$sd <- results2$mse.train_sd
train$data <- as.factor("train")

test <- results2[c("depth.limit", "id")]
test$mean <- results2$mse.test_mean
test$sd <- results2$mse.test_sd
test$data <- as.factor("test")

ggplot(rbind(train, test)) +
  geom_bar( aes(x=reorder(depth.limit, id), y=mean, fill=data), stat="identity", 
            position = "dodge")+
  geom_errorbar(aes(x=reorder(depth.limit, id), ymin=mean-sd, ymax=mean+sd, group=data), 
                width=.2, position=position_dodge(.9)) +
  labs(x = "Depth limit", y = "MSE")+
  theme(text=element_text(size=15))+
  coord_cartesian(ylim=c(0,60))+
  geom_label(aes(x=reorder(depth.limit, id), y=mean-sd-3,
                 label = signif(mean,4)), 
             alpha=0.7, nudge_x=c(-1, -1, -1, -1, 1, 1, 1, 1)*0.22)


# MAE
train <- results2[c("depth.limit", "id")]
train$mean <- results2$mae.train_mean
train$sd <- results2$mae.train_sd
train$data <- as.factor("train")

test <- results2[c("depth.limit", "id")]
test$mean <- results2$mae.test_mean
test$sd <- results2$mae.test_sd
test$data <- as.factor("test")

ggplot(rbind(train, test)) +
  geom_bar( aes(x=reorder(depth.limit, id), y=mean, fill=data), stat="identity", 
            position = "dodge")+
  geom_errorbar(aes(x=reorder(depth.limit, id), ymin=mean-sd, ymax=mean+sd, group=data), 
                width=.2, position=position_dodge(.9)) +
  labs(x = "Depth limit", y = "MAE")+
  theme(text=element_text(size=15), axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim=c(0,5))+
  geom_label(aes(x=reorder(depth.limit, id), y=mean-sd-0.3,
                 label = signif(mean,4)), 
             alpha=0.7, nudge_x=c(-1, -1, -1, -1, 1, 1, 1, 1)*0.22)

# Dispersion
ggplot(results2) +
  geom_bar( aes(x=reorder(depth.limit, id), y=dispersion_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(depth.limit, id),
                    ymin=dispersion_mean-dispersion_sd, 
                    ymax=dispersion_mean+dispersion_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(depth.limit, id), y=dispersion_mean-dispersion_sd, 
                 label = signif(dispersion_mean,4)), 
             alpha=0.7, nudge_y=-0.4)+ 
  labs(x = "Depth limit", y = "Dispersion")+
  geom_hline(yintercept=1, color = "red")+
  theme(text=element_text(size=15))

# Zero-inflation
ggplot(results2) +
  geom_bar( aes(x=reorder(depth.limit, id), y=expected.zero.ratio_mean), stat="identity", fill="darkgray")+
  geom_errorbar(aes(x=reorder(depth.limit, id),
                    ymin=expected.zero.ratio_mean-expected.zero.ratio_sd, 
                    ymax=expected.zero.ratio_mean+expected.zero.ratio_sd), width=.2,
                position=position_dodge(.9)) +
  geom_label(aes(x=reorder(depth.limit, id), y=expected.zero.ratio_mean-expected.zero.ratio_sd,
                 label = signif(expected.zero.ratio_mean,4)), 
             alpha=0.7, nudge_y=-0.07)+ 
  labs(x = "Depth limit", y = "Ratio expected/oberved zeros")+
  geom_hline(yintercept=1, color = "red")+
  theme(text=element_text(size=15))

