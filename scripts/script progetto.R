# Upload packages
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


# Select the features (columns) of interest
data_salary <- data_salary[, c(2, 3)]
data_traditional_per48 <- data_traditional_per48[, c(3, 7, 8, 15, 18, 21:32)]
data_traditional_tot <- data_traditional_tot[, c(3, 12, 68)]
data_advanced <- data_advanced[, c(3, 14, 17, 20, 23, 31, 32, 38)]
data_miscellaneous <- data_miscellaneous[, c(3, 24)]
data_vorp <- data_vorp[, c(2, 3, 23, 31, 32)]


# Rename a column to prepare the dataset to do a merge
names(data_salary)[names(data_salary) == "Player"] <- "PLAYER_NAME"
data_trad_tot <- data_traditional_tot[data_traditional_tot$MIN > 480, ]  # considering players with at least 480 minutes played along the season (better measure compared to games played)


# Merge the datasets applying a full join
data_st <- merge(data_salary, data_traditional_per48, by = "PLAYER_NAME", all = TRUE)
data_ast <- merge(data_st, data_advanced, by = "PLAYER_NAME", all = TRUE)
data_mast <- merge(data_ast, data_miscellaneous, by = "PLAYER_NAME", all = TRUE)
data_mastt <- merge(data_mast, data_trad_tot, by = "PLAYER_NAME", all = TRUE)
final_dataset <- merge(data_mastt, data_vorp, by = "PLAYER_NAME", all = TRUE)


# Facing NA values problem
# data_mast <- data_mast[!is.na(data_mast$2023/24), ]  ## not necessary, names correspond now
final_dataset <- final_dataset[!is.na(final_dataset$AGE), ]     ## NA's we need to remove because no stats on NBA.com
final_dataset <- final_dataset[!is.na(final_dataset$MIN), ]
final_dataset <- final_dataset[!is.na(final_dataset$VORP), ] ## removing NA's for MIN (players with less than 480 minutes played)
final_dataset


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
hist(log(Salary), main="Histogram of the logarithmic salary")      # forma più regolare, ulteriore motivo per usare il logaritmo

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

## QQ plot ok, check linearity residuals vs fitted; check eteroschedasticity scale-location

# trying to transform the response variable
lm.log <- lm(log(Salary)~+., data=fd_numeric)
summary(lm.log)
par(mfrow = c(2, 2))
plot(lm.log)

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
#print(selected.parameters)

selected.formula <- as.formula(paste("log(Salary) ~", paste(selected.parameters, collapse = " + ")))

lm.exhaustive <- lm(selected.formula, data=fd_numeric)
summary(lm.exhaustive)

# correlation between dependent variables
par(mfrow=c(1,1))
corrplot(cor(fd_numeric[c(selected.parameters)]), method = 'number')

# residual analysis
par(mfrow=c(2,2))
plot(lm.exhaustive)

# model performances
lmex.final.pred <- predict(lm.exhaustive)
mse.lmex <- mean((exp(lmex.final.pred)-y)^2)
mse.lmex

### 10 most overpaid and 10 most underpaid players table ###
res <- y - exp(lmex.final.pred)

overpaid_indices <- order(res, decreasing=TRUE)[1:10]
underpaid_indices <- order(res, decreasing=FALSE)[1:10]

over_diff <- res[overpaid_indices]
under_diff <- res[underpaid_indices]

over_pred <- exp(lmex.final.pred)[overpaid_indices]
under_pred <- exp(lmex.final.pred)[underpaid_indices]

## actual salary and player names
fd_over <- final_dataset[overpaid_indices, ][c('Salary')]
fd_under <- final_dataset[underpaid_indices, ][c('Salary')]

overpaid_tab_ex <- cbind(fd_over, over_pred, over_diff)
colnames(overpaid_tab_ex) <- c("Salary", "Predicted salary", "Difference")
underpaid_tab_ex <- cbind(fd_under, under_pred, -under_diff)
colnames(underpaid_tab_ex) <- c("Salary", "Predicted salary", "Difference")

# correlation between dependent variables
par(mfrow=c(1,1))
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

# vector of responses
y <- fd_numeric$Salary

### Cross validation to select the best lambda ###

# select n/2 observations for training set
n <- nrow(X)
n/2

set.seed(1)
train <- sample(1:n, 180)
test  <- setdiff(1:n, train)

cv.out <- cv.glmnet(X[train, ], y[train], alpha = 0, nfold=10)

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
best_lambda
 
# estimated test MSE with bestlambda value
ridge.mod <- glmnet(X[train, ], y[train], alpha=0)
ridge.pred <- predict(ridge.mod, s=best_lambda, newx=X[test,])
test.mse.ridge <- mean((ridge.pred-y[test])^2)
test.mse.ridge

## final model with best lambda on all data
ridge.final <- glmnet(X, y, alpha = 0)
coef(ridge.final, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(ridge.final, xvar="lambda", label=TRUE)
abline(v=log(best_lambda), lty=3, lwd=2)

# R2

#use fitted best model to make predictions
y_predicted <- predict(ridge.final, s = best_lambda, newx = X)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
R2 <- 1 - sse/sst
R2

## R2 better than stepwise regression

# final MSE
ridge.final.pred <- predict(ridge.final, s=best_lambda, X)
mse.ridge <- mean((ridge.final.pred-y)^2)
mse.ridge

## mse meglio della exhaustive subset search


### 10 most overpaid and 10 most underpaid players table ###

diffs <- y - ridge.final.pred
diffs <- as.vector(diffs)

overpaid_indices <- order(diffs, decreasing=TRUE)[1:10]
underpaid_indices <- order(diffs, decreasing=FALSE)[1:10]

over_diff <- diffs[overpaid_indices]
under_diff <- diffs[underpaid_indices]

over_pred <- ridge.final.pred[overpaid_indices]
under_pred <- ridge.final.pred[underpaid_indices]

## actual salary and player names
fd_over <- final_dataset[overpaid_indices, ][c('Salary')]
fd_under <- final_dataset[underpaid_indices, ][c('Salary')]

## final table for ridge regression
overpaid_tab_ridge <- cbind(fd_over, over_pred, over_diff)
colnames(overpaid_tab_ridge) <- c("Salary", "Predicted salary", "Difference")
underpaid_tab_ridge <- cbind(fd_under, under_pred, -under_diff)
colnames(underpaid_tab_ridge) <- c("Salary", "Predicted salary", "Difference")

#### the ridge regression shows better results compared to stepwise regression.
#### One of the reasons is the fact that ridge regression works well with
#### collinear variables, even with a small lambda.


#############################
#     LASSO REGRESSION      #
#############################

cv.out <- cv.glmnet(X[train, ], y[train], alpha = 1, nfold=10)
plot(cv.out)

best_lambda <- cv.out$lambda.min
best_lambda

# estimated test MSE with bestlambda value
lasso.mod <- glmnet(X[train, ], y[train], alpha=1)
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=X[test,])
test.mse.lasso <- mean((lasso.pred-y[test])^2)
test.mse.lasso

# final model with best lambda on all data
lasso.final <- glmnet(X, y, alpha = 1)
coef(lasso.final, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lasso.final, xvar="lambda", label=TRUE)
abline(v=log(best_lambda), lty=3, lwd=2)

# use fitted best model to make predictions
y_predicted <- predict(lasso.final, s = best_lambda, newx = X)

# SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# slightly better R2 than ridge

# final MSE
lasso.final.pred <- predict(lasso.final, s=best_lambda, X)
mse.lasso <- mean((lasso.final.pred-y)^2)
mse.lasso
# slightly better MSE than ridge


### 10 most overpaid and 10 most underpaid players table ###

diffs <- y - lasso.final.pred
diffs <- as.vector(diffs)

overpaid_indices <- order(diffs, decreasing=TRUE)[1:10]
underpaid_indices <- order(diffs, decreasing=FALSE)[1:10]

over_diff <- diffs[overpaid_indices]
under_diff <- diffs[underpaid_indices]

over_pred <- lasso.final.pred[overpaid_indices]
under_pred <- lasso.final.pred[underpaid_indices]

## actual salary and player names
fd_over <- final_dataset[overpaid_indices, ][c('Salary')]
fd_under <- final_dataset[underpaid_indices, ][c('Salary')]

## final table for ridge regression
overpaid_tab_lasso <- cbind(fd_over, over_pred, over_diff)
colnames(overpaid_tab_lasso) <- c("Salary", "Predicted salary", "Difference")
underpaid_tab_lasso <- cbind(fd_under, under_pred, -under_diff)
colnames(underpaid_tab_lasso) <- c("Salary", "Predicted salary", "Difference")


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
center_fd <- fd_list$C
forward_fd <- fd_list$F
guard_fd <- fd_list$G

# remove names and position
center_fd_numeric <- center_fd[, numeric_cols]
forward_fd_numeric <- forward_fd[, numeric_cols]
guard_fd_numeric <- guard_fd[, numeric_cols]


#### LASSO FOR CENTER POSITION

lm.mod <- lm(Salary~+., data=center_fd_numeric)
summary(lm.mod)

# design matrix not considering the intercept
X <- model.matrix(Salary~., data=center_fd_numeric)
X <- X[,-1]

# vector of responses
y <- center_fd_numeric$Salary

### Cross validation to select the best lambda ###

# select n/2 observations for training set
n <- nrow(X)
n/2

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

cv.out <- cv.glmnet(X[train, ], y[train], alpha = 1, nfold=10)
plot(cv.out)

best_lambda <- cv.out$lambda.min
best_lambda

# estimated test MSE with bestlambda value
lasso.mod <- glmnet(X[train, ], y[train], alpha=1)
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=X[test,])
test.mse.lasso <- mean((lasso.pred-y[test])^2)
test.mse.lasso

# final model with best lambda on all data
lasso.final <- glmnet(X, y, alpha = 1)
coef(lasso.final, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lasso.final, xvar="lambda", label=TRUE)
abline(v=log(best_lambda), lty=3, lwd=2)

# use fitted best model to make predictions
y_predicted <- predict(lasso.final, s = best_lambda, newx = X)

# SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# very high R2

# final MSE
lasso.final.pred <- predict(lasso.final, s=best_lambda, X)
mse.lasso <- mean((lasso.final.pred-y)^2)
mse.lasso
# better than before

### 3 most overpaid and 3 most underpaid centers table ###

diffs <- center_fd_numeric$Salary - lasso.final.pred
diffs <- as.vector(diffs)

overpaid_indices <- order(diffs, decreasing=TRUE)[1:3]
underpaid_indices <- order(diffs, decreasing=FALSE)[1:3]

over_diff <- diffs[overpaid_indices]
under_diff <- diffs[underpaid_indices]

over_pred <- lasso.final.pred[overpaid_indices]
under_pred <- lasso.final.pred[underpaid_indices]

## actual salary and player names
fd_over <- center_fd[overpaid_indices, ][c('Salary')]
fd_under <- center_fd[underpaid_indices, ][c('Salary')]

## final table for ridge regression
overpaidcenters_tab_lasso <- cbind(fd_over, over_pred, over_diff)
colnames(overpaidcenters_tab_lasso) <- c("Salary", "Predicted salary", "Difference")
underpaidcenters_tab_lasso <- cbind(fd_under, under_pred, -under_diff)
colnames(underpaidcenters_tab_lasso) <- c("Salary", "Predicted salary", "Difference")


#### LASSO FOR FORWARD POSITION

lm.mod <- lm(Salary~+., data=forward_fd_numeric)
summary(lm.mod)

# design matrix not considering the intercept
X <- model.matrix(Salary~., data=forward_fd_numeric)
X <- X[,-1]

# vector of responses
y <- forward_fd_numeric$Salary

### Cross validation to select the best lambda ###

# select n/2 observations for training set
n <- nrow(X)
n/2

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

cv.out <- cv.glmnet(X[train, ], y[train], alpha = 1, nfold=10)
plot(cv.out)

best_lambda <- cv.out$lambda.min
best_lambda

# estimated test MSE with bestlambda value
lasso.mod <- glmnet(X[train, ], y[train], alpha=1)
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=X[test,])
test.mse.lasso <- mean((lasso.pred-y[test])^2)
test.mse.lasso

# final model with best lambda on all data
lasso.final <- glmnet(X, y, alpha = 1)
coef(lasso.final, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lasso.final, xvar="lambda", label=TRUE)
abline(v=log(best_lambda), lty=3, lwd=2)

# use fitted best model to make predictions
y_predicted <- predict(lasso.final, s = best_lambda, newx = X)

# SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# very high R2

# final MSE
lasso.final.pred <- predict(lasso.final, s=best_lambda, X)
mse.lasso <- mean((lasso.final.pred-y)^2)
mse.lasso
# better than before

### 3 most overpaid and 3 most underpaid forwards table ###

diffs <- forward_fd_numeric$Salary - lasso.final.pred
diffs <- as.vector(diffs)

overpaid_indices <- order(diffs, decreasing=TRUE)[1:3]
underpaid_indices <- order(diffs, decreasing=FALSE)[1:3]

over_diff <- diffs[overpaid_indices]
under_diff <- diffs[underpaid_indices]

over_pred <- lasso.final.pred[overpaid_indices]
under_pred <- lasso.final.pred[underpaid_indices]

## actual salary and player names
fd_over <- forward_fd[overpaid_indices, ][c('Salary')]
fd_under <- forward_fd[underpaid_indices, ][c('Salary')]

## final table for ridge regression
overpaidforwards_tab_lasso <- cbind(fd_over, over_pred, over_diff)
colnames(overpaidforwards_tab_lasso) <- c("Salary", "Predicted salary", "Difference")
underpaidforwards_tab_lasso <- cbind(fd_under, under_pred, under_diff)
colnames(underpaidforwards_tab_lasso) <- c("Salary", "Predicted salary", "Difference")


#### LASSO FOR GUARD POSITION

lm.mod <- lm(Salary~+., data=guard_fd_numeric)
summary(lm.mod)

# design matrix not considering the intercept
X <- model.matrix(Salary~., data=guard_fd_numeric)
X <- X[,-1]

# vector of responses
y <- guard_fd_numeric$Salary

### Cross validation to select the best lambda ###

# select n/2 observations for training set
n <- nrow(X)
n/2

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

cv.out <- cv.glmnet(X[train, ], y[train], alpha = 1, nfold=10)
plot(cv.out)

best_lambda <- cv.out$lambda.min
best_lambda

# estimated test MSE with bestlambda value
lasso.mod <- glmnet(X[train, ], y[train], alpha=1)
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=X[test,])
test.mse.lasso <- mean((lasso.pred-y[test])^2)
test.mse.lasso

# final model with best lambda on all data
lasso.final <- glmnet(X, y, alpha = 1)
coef(lasso.final, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lasso.final, xvar="lambda", label=TRUE)
abline(v=log(best_lambda), lty=3, lwd=2)

# use fitted best model to make predictions
y_predicted <- predict(lasso.final, s = best_lambda, newx = X)

# SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# worse R2 than centers and forwards

# final MSE
lasso.final.pred <- predict(lasso.final, s=best_lambda, X)
mse.lasso <- mean((lasso.final.pred-y)^2)
mse.lasso
# worse than centers and forwards

### 3 most overpaid and 3 most underpaid centers table ###

diffs <- guard_fd_numeric$Salary - lasso.final.pred
diffs <- as.vector(diffs)

overpaid_indices <- order(diffs, decreasing=TRUE)[1:3]
underpaid_indices <- order(diffs, decreasing=FALSE)[1:3]

over_diff <- diffs[overpaid_indices]
under_diff <- diffs[underpaid_indices]

over_pred <- lasso.final.pred[overpaid_indices]
under_pred <- lasso.final.pred[underpaid_indices]

## actual salary and player names
fd_over <- guard_fd[overpaid_indices, ][c('Salary')]
fd_under <- guard_fd[underpaid_indices, ][c('Salary')]

## final table for ridge regression
overpaidguards_tab_lasso <- cbind(fd_over, over_pred, over_diff)
colnames(overpaidguards_tab_lasso) <- c("Salary", "Predicted salary", "Difference")
underpaidguards_tab_lasso <- cbind(fd_under, under_pred, under_diff)
colnames(underpaidguards_tab_lasso) <- c("Salary", "Predicted salary", "Difference")
