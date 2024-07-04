# Upload readxl to import data from Excel to R
library(readxl)

# Upload datasets
data_salary <- read_excel("./data/NBA_players_salaries_HH.xlsx")
data_traditional_per48 <- read.csv("./data/RS_traditional_per48.csv")
data_traditional_tot <- read.csv("./data/RS_traditional_TOTALS.csv")
data_advanced <- read.csv("./data/RS_advanced_per48.csv")
data_miscellaneous <- read.csv("./data/RS_miscellaneous_per48.csv")
data_vorp <- read_excel("./data/vorp.xlsx")

# creating a new column in data_traditional_tot: MIN_G (minutes played per game)
# min/gp
MIN_G <- data_traditional_tot$MIN/data_traditional_tot$GP
data_traditional_tot <- cbind(data_traditional_tot, MIN_G)
rm(MIN_G) # remove the variable MIN_G, we won't use it anymore

# Select the features (columns) of interest
data_salary <- data_salary[, c(2, 3)]
data_traditional_per48 <- data_traditional_per48[, c(3, 7, 8, 15, 18, 21:32)]
data_traditional_tot <- data_traditional_tot[, c(3, 12, 68)]
data_advanced <- data_advanced[, c(3, 14, 17, 20, 23, 31, 32, 38)]
data_miscellaneous <- data_miscellaneous[, c(3, 24)]
data_vorp <- data_vorp[, c(2, 3, 23, 31, 32)]

# Rename a column to prepare the dataset to do a merge
names(data_salary)[names(data_salary) == "Player"] <- "PLAYER_NAME"
data_traditional_tot <- data_traditional_tot[data_traditional_tot$MIN > 480, ]  # considering players with at least 480 minutes played along the season (better measure compared to games played)

# Merge the datasets applying a full join
final_dataset <- merge(data_salary, data_traditional_per48, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_advanced, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_miscellaneous, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_traditional_tot, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(final_dataset, data_vorp, by = "PLAYER_NAME", all = TRUE)

# Facing NA values problem
final_dataset <- final_dataset[!is.na(final_dataset$AGE), ] # NA's we need to remove because no stats on NBA.com
final_dataset <- final_dataset[!is.na(final_dataset$MIN), ] # removing NA's for MIN (players with less than 480 minutes played)
final_dataset <- final_dataset[!is.na(final_dataset$VORP), ]

# Ensure that there aren't NA values
colSums(is.na(final_dataset))

# removing PFD.x and renaming PFD.y as PFD
final_dataset <- final_dataset[, -17]
colnames(final_dataset)[colnames(final_dataset) == 'PFD.y'] <- 'PFD'

# renaming the column 2023/2024 in "Salary", removing the "$" sign and changing the data class in numeric
colnames(final_dataset)[colnames(final_dataset) == '2023/24'] <- 'Salary'
final_dataset$Salary <- as.numeric(gsub("[\\$\\,]", "", final_dataset$Salary))
class(final_dataset$Salary)

# finally, we set the players' name as the row names
# thus, we can eliminate the column PLAYER_NAME
rownames(final_dataset) <- final_dataset$PLAYER_NAME
final_dataset <- final_dataset[, -1]

rm(data_salary, data_advanced, data_miscellaneous, data_traditional_per48, data_traditional_tot, data_vorp)

###########
### EDA ###
###########
attach(final_dataset)

# only numeric columns for EDA (removes the position)
numeric_cols <- sapply(final_dataset, is.numeric)
fd_numeric <- final_dataset[, numeric_cols]
summary(fd_numeric)


# variable Salary (dependent)
summary(Salary)
boxplot(Salary, main="Boxplot of the salary")
hist(Salary, main="Histogram of the salary")
boxplot(log(Salary), main="Boxplot of the logarithmic salary")
hist(log(Salary), main="Histogram of the logarithmic salary") # forma più regolare, ulteriore motivo per usare il logaritmo

par(mfrow = c(2, 2))
boxplot(Salary, main="Boxplot of the salary")
hist(Salary, main="Histogram of the salary")
boxplot(log(Salary),  main="Logarithmic salary", ylab="Salary in millions")
hist(log(Salary),  main="Logarithmic salary", xlab="Salary in millions")
par(mfrow = c(1, 1))


# Independent variables
boxplot(AGE, main="AGE", names=c("AGE"), show.names=TRUE, ylab="years")
boxplot(GP, main="Games played", names=c("GP"), show.names=TRUE, ylab="number of GP")
boxplot(MIN, main="MiN played per season", names=c("MIN"), show.names=TRUE, ylab="minutes played")
boxplot(MIN_G, PTS, main="MIN played and PTS scored per game", names=c("MIN_G", "PTS"), ylab="units number")
boxplot(OREB, DREB, REB, AST, main="OREB, DREB, REB, AST per game", names=c("OREB", "DREB", "REB", "AST"), ylab="units number")
boxplot(TOV, STL, BLK, BLKA, PF, PFD, main="TOV, STL, BLK, BLKA, PF, PFD per game", names=c("TOV", "STL", "BLK", "BLKA", "PF", "PFD"), ylab="units number")
boxplot(FG_PCT, FG3_PCT, FT_PCT, TS_PCT, main="FG_PCT, FG3_PCT, FT_PCT, TS_PCT", names=c("FG_PCT", "FG3_PCT", "FT_PCT", "TS_PCT"), ylab="percentage")
boxplot(OFF_RATING, DEF_RATING, main="OFF_RATING, DEF_RATING", names=c("OFF_RATING", "DEF_RATING"), ylab="rating")
boxplot(NET_RATING, main="NET_RATING", names=c("NET_RATING"), show.names=TRUE, ylab="rating")
boxplot(AST_TO, main="AST_TO", names=c("AST_TO"), show.names=TRUE, ylab="units number")
boxplot(PIE, USG_PCT, main="PIE, USG_PCT", names=c("PIE", "USG_PCT"), ylab="percentage")
boxplot(WS, BPM, VORP, main="WS, BPM, VORP", names=c("WS", "BPM", "VORP"), ylab="score")


## Analyze correlations

# covariance and correlation matrices
cov_mat <- round(cov(fd_numeric),2)
cov_mat
cor_mat <- round(cor(fd_numeric),2)
cor_mat

library(corrplot)

corrplot(cor(fd_numeric), method = 'color')
corrplot(cor(fd_numeric), method = 'ellipse')


############
## MODELS ## 
############

## we will start creating a linear model in order to predict the salaries and
## then we'll perform a stepwise regression to remove the less significative
## variables.
## After that, we'll use ridge regression in order to reduce the effect of
## multicollinearity. Then, we'll compare the performances of the models.
## Lastly, we'll compare models' results with the actual salaries earned by
## the players during the 2023/2024 season. 

## LINEAR REGRESSION MODEL
lm.mod <- lm(Salary~+., data=fd_numeric)
summary(lm.mod)

# MSE
lm.mod.pred <- predict(lm.mod)
y <- fd_numeric$Salary
mse.lm.mod <- mean((lm.mod.pred-y)^2)
mse.lm.mod

# Residual analysis
par(mfrow = c(2, 2))
plot(lm.mod)
par(mfrow = c(1, 1))

## QQ plot ok, check linearity residuals vs fitted; check eteroschedasticity scale-location

# trying to transform the response variable
lm.log <- lm(log(Salary)~+., data=fd_numeric)
summary(lm.log)
par(mfrow = c(2, 2))
plot(lm.log)
par(mfrow = c(1, 1))

#MSE
lm.log.pred <- predict(lm.log)
mse.lm.log <- mean((exp(lm.log.pred)-y)^2)
mse.lm.log

## better linearity residuals vs fitted(1st plot), better with omoschedasticity(3rd plot)


###############################
# EXHAUSTIVE SUBSET SELECTION #
###############################
library(leaps)

regfit.full <- regsubsets(log(Salary)~., data=fd_numeric, nvmax=(ncol(fd_numeric)-1))
reg.summary <- summary(regfit.full)
reg.summary$outmat
reg.summary$which
reg.summary$rsq

par(mfrow=c(2,2))

# residual sum of squares
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
i <- which.max(reg.summary$adjr2)
points(i,reg.summary$adjr2[i], col="red",cex=2,pch=20)
text(i,reg.summary$adjr2[i], i, pos=1)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
i <- which.min(reg.summary$cp)#return the index of the minimum
points(i,reg.summary$cp[i],col="red",cex=2,pch=20)
text(i,reg.summary$cp[i], i, pos=3)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
i <- which.min(reg.summary$bic)
points(i,reg.summary$bic[i],col="red",cex=2,pch=20)
text(i,reg.summary$bic[i], i, pos=3)

par(mfrow = c(1,1))

# It seems that selecting 12 or 13 parameters gives us the best 
# balance between model simplicity and precision. 

#let's get the list of selected parameters:
covariates = 12

selected.model <- reg.summary$which[covariates,]
selected.parameters <- names(selected.model[selected.model])[-1] #-1 to lose the intercept
print(selected.parameters)

selected.formula <- as.formula(paste("log(Salary) ~", paste(selected.parameters, collapse = " + ")))

lm.ess <- lm(selected.formula, data=fd_numeric)
summary(lm.ess)

# correlation between dependent variables
corrplot(cor(fd_numeric[c(selected.parameters)]), method = 'number')

# residual analysis
par(mfrow=c(2,2))
plot(lm.ess)
par(mfrow=c(1,1))

# model performances
lm.ess.pred <- predict(lm.ess)
mse.lm.ess <- mean((exp(lm.ess.pred)-y)^2)
mse.lm.ess

###  function that returns the top_N overpaid and top-N underpaid players tables ###
create_tables <- function(real_values, pred_values, N) {
  res <- real_values - pred_values
  res <- as.vector(res)
  
  overpaid_indices <- order(res, decreasing=TRUE)[1:N]
  underpaid_indices <- order(res, decreasing=FALSE)[1:N]
  
  over_diff <- res[overpaid_indices]
  under_diff <- res[underpaid_indices]
  
  over_pred <- pred_values[overpaid_indices]
  under_pred <- pred_values[underpaid_indices]
  
  ## actual salary and player names
  fd_over <- final_dataset[overpaid_indices, ][c('Salary')]
  fd_under <- final_dataset[underpaid_indices, ][c('Salary')]
  
  overpaid_table <- cbind(fd_over, over_pred, over_diff)
  colnames(overpaid_table) <- c("Salary", "Predicted salary", "Difference")
  underpaid_table <- cbind(fd_under, under_pred, -under_diff)
  colnames(underpaid_table) <- c("Salary", "Predicted salary", "Difference")
  
  return(list(overpaid_table, underpaid_table))
}

lm.ess.tables <- create_tables(y, exp(lm.ess.pred), 10)
lm.ess.tables[[1]]
lm.ess.tables[[2]]

# correlation between dependent variables
corrplot(cor(fd_numeric[c(selected.parameters)]), method = 'number')


## we have to analyse the correlation between independent variables in this
## model: we expect a lot of correlations, so multicollinearity.
## For this reason, we will try ridge and lasso that perform good 
## in case of multicollinearity 


#############################
#     RIDGE REGRESSION      #
#############################
library(glmnet)

# linear model
lm.mod <- lm(Salary~+., data=fd_numeric)
summary(lm.mod)

# design matrix not considering the intercept
X <- model.matrix(Salary~., data=fd_numeric)
X <- X[,-1]
# y already defined before

### Ten Fold Cross Validation function to select the best lambda ###
ten_fold_cv <- function(X, y, a) {
  print("### 10-FOLD CROSS-VALIDATION ###")
  n <- nrow(X)
  
  set.seed(1)
  train <- sample(1:n, n/2)
  test  <- setdiff(1:n, train)
  
  cv.out <- cv.glmnet(X[train, ], y[train], alpha = a, nfold = 10)
  
  # This plots the cross-validation curve (red dotted line) along with upper and
  # lower standard deviation curves along the lambda sequence (error bars).
  # Two special values along the lambda sequence are indicated by the vertical
  # dotted lines. lambda.min is the value of lambda that gives minimum mean
  # cross-validated error, while lambda.1se is the value of lambda that gives
  # the most regularized model such that the cross-validated error is within one
  # standard error of the minimum.
  plot(cv.out)
  
  ## selecting the lambda that minimizes test MSE
  best_lambda <- cv.out$lambda.min
  print(paste("The best lambda is = ", round(best_lambda)))
  
  # estimated test MSE with bestlambda value
  mod <- glmnet(X[train, ], y[train], alpha = a)
  pred <- predict(mod, s = best_lambda, newx = X[test,])
  mse <- mean((pred-y[test])^2)
  print(paste("The estimated test MSE with the best lambda is = ", format(mse, scientific = TRUE)))
  
  return <- best_lambda
}

best_lambda <- ten_fold_cv(X, y, 0)

## final model with best lambda on all data
lm.rid <- glmnet(X, y, alpha = 0)
coef(lm.rid, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.rid, xvar = "lambda", label = TRUE)
abline(v=log(best_lambda), lty = 3, lwd = 2)

# R2

#use fitted best model to make predictions
lm.rid.pred <- predict(lm.rid, s = best_lambda, X)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((lm.rid.pred - y)^2)

#find R-Squared
R2 <- 1 - sse/sst
R2
# R2 better than Exhaustive Subset Selection

# final MSE
mse.lm.rid <- mean((lm.rid.pred-y)^2)
mse.lm.rid
## mse better than Exhaustive Subset Selection


### 10 most overpaid and 10 most underpaid players table ###
lm.rid.tables <- create_tables(y, lm.rid.pred, 10)
lm.rid.tables[[1]]
lm.rid.tables[[2]]

#### the ridge regression shows better results compared to the Exhaustive
#### Subset Selection. One of the reasons is the fact that ridge regression
#### works well with collinear variables, even with a small lambda.


#############################
#     LASSO REGRESSION      #
#############################

best_lambda <- ten_fold_cv(X, y, 1)

# final model with best lambda on all data
lm.las <- glmnet(X, y, alpha = 1)
coef(lm.las, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.pred <- predict(lm.las, s = best_lambda, X)

# SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((lm.las.pred - y)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# slightly better R2 than ridge

# final MSE
lm.las.pred <- predict(lm.las, s=best_lambda, X)
mse.lm.las <- mean((lm.las.pred-y)^2)
mse.lm.las
# slightly better MSE than ridge


### 10 most overpaid and 10 most underpaid players table ###
lm.las.tables <- create_tables(y, lm.las.pred, 10)
lm.las.tables[[1]]
lm.las.tables[[2]]


############################################
# LASSO REGRESSION FOR DIFFERENT POSITIONS #
############################################

# function to transform PG and SG into G, and PF and SF into F
recode_pos <- function(x) {
  ifelse(x %in% c("PG", "SG"), "G", 
         ifelse(x %in% c("PF", "SF"), "F", x))
}

final_dataset$Pos <- recode_pos(final_dataset$Pos)

# split the dataset based on position
fd_list <- split(final_dataset, final_dataset$Pos)
fd_center <- fd_list$C
fd_forward <- fd_list$F
fd_guard <- fd_list$G
rm(fd_list)

# remove position
fd_center <- fd_center[, numeric_cols]
fd_forward <- fd_forward[, numeric_cols]
fd_guard <- fd_guard[, numeric_cols]


#### LASSO FOR CENTER POSITION

lm.mod.c <- lm(Salary~+., data=fd_center)
summary(lm.mod.c)

# design matrix not considering the intercept
X.c <- model.matrix(Salary~., data=fd_center)
X.c <- X.c[,-1]
y.c <- fd_center$Salary

### Cross validation to select the best lambda ###
best_lambda <- ten_fold_cv(X.c, y.c, 1)

# final model with best lambda on all data
lm.las.c <- glmnet(X.c, y.c, alpha = 1)
coef(lm.las.c, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.c, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.c.pred <- predict(lm.las.c, s = best_lambda, X.c)

# SST and SSE
sst <- sum((y.c - mean(y.c))^2)
sse <- sum((lm.las.c.pred - y.c)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# very high R2

# final MSE
mse.lm.las.c <- mean((lm.las.c.pred - y.c)^2)
mse.lm.las.c
# better than before

### 3 most overpaid and 3 most underpaid centers table ###
lm.las.c.tables <- create_tables(y.c, lm.las.c.pred, 3)
lm.las.c.tables[[1]]
lm.las.c.tables[[2]]


#### LASSO FOR FORWARD POSITION

lm.mod.f <- lm(Salary~+., data=fd_forward)
summary(lm.mod.f)

# design matrix not considering the intercept
X.f <- model.matrix(Salary~., data=fd_forward)
X.f <- X.f[,-1]

# vector of responses
y.f <- fd_forward$Salary

### Cross validation to select the best lambda ###
best_lambda <- ten_fold_cv(X.f, y.f, 1)

# final model with best lambda on all data
lm.las.f <- glmnet(X.f, y.f, alpha = 1)
coef(lm.las.f, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.f, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.f.pred <- predict(lm.las.f, s = best_lambda, X.f)

# SST and SSE
sst <- sum((y.f - mean(y.f))^2)
sse <- sum((lm.las.f.pred - y.f)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# very high R2

# final MSE
mse.lm.las.f <- mean((lm.las.f.pred-y.f)^2)
mse.lm.las.f
# better than before

### 3 most overpaid and 3 most underpaid forwards table ###
lm.las.f.tables <- create_tables(y.f, lm.las.f.pred, 3)
lm.las.f.tables[[1]]
lm.las.f.tables[[2]]


#### LASSO FOR GUARD POSITION

lm.mod.g <- lm(Salary~+., data=fd_guard)
summary(lm.mod.g)

# design matrix not considering the intercept
X.g <- model.matrix(Salary~., data=fd_guard)
X.g <- X.g[,-1]

# vector of responses
y.g <- fd_guard$Salary

### Cross validation to select the best lambda ###
best_lambda <- ten_fold_cv(X.g, y.g, 1)

# final model with best lambda on all data
lm.las.g <- glmnet(X.g, y.g, alpha = 1)
coef(lm.las.g, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.g, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.g.pred <- predict(lm.las.g, s = best_lambda, X.g)

# SST and SSE
sst <- sum((y.g - mean(y.g))^2)
sse <- sum((lm.las.g.pred - y.g)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# worse R2 than centers and forwards

# final MSE
mse.lm.las.g <- mean((lm.las.g.pred-y.g)^2)
mse.lm.las.g
# worse than centers and forwards

### 3 most overpaid and 3 most underpaid centers table ###
lm.las.g.tables <- create_tables(y.g, lm.las.g.pred, 3)
lm.las.g.tables[[1]]
lm.las.g.tables[[2]]

