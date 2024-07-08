#########
# MODIFICHE DA FARE
#
# 1a) eventualmente cambiare interpretazioni
#
# 2) utilizzare MSE su test per comparare i modelli:
# 2a) creare train e test set e calcolare test error su regressione logaritmica iniziale e su subset selection
# 2b) confrontare di nuovo tutti i modelli utilizzando la divisione train e test (lui usa la stessa)
# 2c) mantenere comunque MSE su tutti i dati per completezza di informazione
########


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
boxplot(Salary, main="Salary")
hist(Salary, main="Salary")

# log transformation of Salary
boxplot(log(Salary), main="Logarithmic salary")
hist(log(Salary), main="Logarithmic salary") 

# sqrt transformation of Salary
boxplot(sqrt(Salary), main="Square rooted Salary")
hist(sqrt(Salary), main="Square rooted Salary")  # forma più regolare, ulteriore motivo per usare sqrt


par(mfrow = c(2, 2))
boxplot(Salary, main="Salary")
hist(Salary, main="Salary")
boxplot(sqrt(Salary), main="Square rooted Salary")
hist(sqrt(Salary), main="Square rooted Salary")
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
format(sqrt(mse.lm.mod), scientific = TRUE)

# Residual analysis
par(mfrow = c(2, 2))
plot(lm.mod)
par(mfrow = c(1, 1))

## QQ plot ok, check linearity residuals vs fitted;
# check eteroschedasticity scale-location

# trying to transform the response variable (log)
# lm.log <- lm(log(Salary)~+., data=fd_numeric)
# summary(lm.log)

# par(mfrow = c(2, 2))
# plot(lm.log)
# par(mfrow = c(1, 1))

#MSE
# lm.log.pred <- predict(lm.log)
# mse.lm.log <- mean((exp(lm.log.pred)-y)^2)
# mse.lm.log


# transform the response variable (sqrt)
lm.sqrt <- lm(sqrt(Salary)~., data = fd_numeric)
summary(lm.sqrt)

par(mfrow = c(2, 2))
plot(lm.sqrt)
par(mfrow = c(1, 1))

lm.sqrt.pred <- predict(lm.sqrt)
mse.lm.sqrt <- mean((lm.sqrt.pred^2 - y)^2)
mse.lm.sqrt
format(sqrt(mse.lm.sqrt), scientific = TRUE)

# better linearity residuals vs fitted(1st plot),
# better with omoschedasticity(3rd plot)

X <- model.matrix(sqrt(Salary)~., data=fd_numeric)
X <- X[,-1]
n <- nrow(X)

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

# look at the performances of the complete model with sqrt of the Salary
# in a test set in order to compare it to the other models
library(glmnet)
lm.sqrt.test <- glmnet(X[train, ], sqrt(y[train]), alpha = 0, lambda = 0)
lm.sqrt.test.pred <- predict(lm.sqrt.test, s = 0, newx = X[test, ], exact = TRUE, x = X[train, ], y = y[train])
lm.sqrt.test.mse <- mean((lm.sqrt.test.pred^2-y[test])^2)
print(paste("Estimated test MSE = ", format(lm.sqrt.test.mse, scientific = TRUE)))
format(sqrt(lm.sqrt.test.mse), scientific = TRUE)


###############################
# EXHAUSTIVE SUBSET SELECTION
###############################
library(leaps)

regfit.full.sqrt <- regsubsets(sqrt(Salary)~., data=fd_numeric, nvmax=(ncol(fd_numeric)-1))
reg.summary.sqrt <- summary(regfit.full.sqrt)
reg.summary.sqrt$outmat
reg.summary.sqrt$which
reg.summary.sqrt$rsq

par(mfrow=c(2,2))

# residual sum of squares
plot(reg.summary.sqrt$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(reg.summary.sqrt$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
i <- which.max(reg.summary.sqrt$adjr2)
points(i,reg.summary.sqrt$adjr2[i], col="red",cex=2,pch=20)
text(i,reg.summary.sqrt$adjr2[i], i, pos=1)

# Mallow's Cp with its smallest value
plot(reg.summary.sqrt$cp,xlab="Number of Variables",ylab="Cp",type='l')
i <- which.min(reg.summary.sqrt$cp)#return the index of the minimum
points(i,reg.summary.sqrt$cp[i],col="red",cex=2,pch=20)
text(i,reg.summary.sqrt$cp[i], i, pos=3)

# BIC with its smallest value
plot(reg.summary.sqrt$bic,xlab="Number of Variables",ylab="BIC",type='l')
i <- which.min(reg.summary.sqrt$bic)
points(i,reg.summary.sqrt$bic[i],col="red",cex=2,pch=20)
text(i,reg.summary.sqrt$bic[i], i, pos=3)

par(mfrow = c(1,1))

# It seems that selecting 14 parameters gives us the best 
# balance between model simplicity and precision. 

#let's get the list of selected parameters:
covariates = 14

selected.model.sqrt <- reg.summary.sqrt$which[covariates,]
selected.parameters.sqrt <- names(selected.model.sqrt[selected.model.sqrt])[-1] #-1 to lose the intercept
print(selected.parameters.sqrt)

selected.formula.sqrt <- as.formula(paste("sqrt(Salary)~", paste(selected.parameters.sqrt, collapse = " + ")))

lm.ess.sqrt <- lm(selected.formula.sqrt, data=fd_numeric)
summary(lm.ess.sqrt)

# correlation between dependent variables
corrplot(cor(fd_numeric[c(selected.parameters.sqrt)]), method = 'color')

# residual analysis
par(mfrow=c(2,2))
plot(lm.ess.sqrt)
par(mfrow=c(1,1))

# model performances
lm.ess.pred.sqrt <- predict(lm.ess.sqrt)
mse.lm.ess.sqrt <- mean((lm.ess.pred.sqrt)^2-y)^2
mse.lm.ess.sqrt
format(mse.lm.ess.sqrt, scientific=TRUE)
format(sqrt(mse.lm.ess.sqrt), scientific=TRUE)

# performances on a test set
X <- model.matrix(selected.formula.sqrt, data=fd_numeric)
X <- X[,-1]

lm.ess.sqrt.test <- glmnet(X[train, ], sqrt(y[train]), alpha = 0, lambda = 0)
lm.ess.sqrt.test.pred <- predict(lm.ess.sqrt.test, s = 0, newx = X[test, ], exact = TRUE, x = X[train, ], y = y[train])
lm.ess.sqrt.test.mse <- mean((lm.ess.sqrt.test.pred^2-y[test])^2)
print(paste("Estimated test MSE = ", format(lm.ess.sqrt.test.mse, scientific = TRUE)))
format(sqrt(lm.ess.sqrt.test.mse), scientific = TRUE)


###  function that returns the top_N overpaid and top-N underpaid players tables ###
create_tables <- function(real_values, pred_values, df, N) {
  res <- real_values - pred_values
  res <- as.vector(res)
  
  overpaid_indices <- order(res, decreasing=TRUE)[1:N]
  underpaid_indices <- order(res, decreasing=FALSE)[1:N]
  
  over_diff <- res[overpaid_indices]
  under_diff <- res[underpaid_indices]
  
  over_pred <- pred_values[overpaid_indices]
  under_pred <- pred_values[underpaid_indices]
  
  ## actual salary and player names
  fd_over <- df[overpaid_indices, ][c('Salary')]
  fd_under <- df[underpaid_indices, ][c('Salary')]
  
  overpaid_table <- cbind(fd_over, over_pred, over_diff)
  colnames(overpaid_table) <- c("Salary", "Predicted salary", "Difference")
  underpaid_table <- cbind(fd_under, under_pred, -under_diff)
  colnames(underpaid_table) <- c("Salary", "Predicted salary", "Difference")
  
  return(list(overpaid_table, underpaid_table))
}

lm.ess.sqrt.tables <- create_tables(y, lm.ess.pred.sqrt^2, final_dataset, 10)
lm.ess.sqrt.tables[[1]]
lm.ess.sqrt.tables[[2]]

# correlation between dependent variables
corrplot(cor(fd_numeric[c(selected.parameters.sqrt)]), method = 'color')

#############################
#     RIDGE REGRESSION
#############################

# linear model
lm.mod.sqrt <- lm(sqrt(Salary)~., data=fd_numeric)
summary(lm.mod.sqrt)

# design matrix not considering the intercept
X <- model.matrix(sqrt(Salary)~., data=fd_numeric)
X <- X[,-1]

### Ten Fold Cross Validation function to select the best lambda ###
ten_fold_cv <- function(X, y, a) {
  print("### 10-FOLD CROSS-VALIDATION ###")
  n <- nrow(X)
  
  set.seed(1)
  train <- sample(1:n, n/2)
  test  <- setdiff(1:n, train)
  
  cv.out <- cv.glmnet(X[train, ], sqrt(y[train]), alpha = a, nfold = 10)
  
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
  mod <- glmnet(X[train, ], sqrt(y[train]), alpha = a)
  pred <- predict(mod, s = best_lambda, newx = X[test,])
  mse <- mean((pred^2-y[test])^2)
  print(paste("The estimated test MSE with the best lambda is = ", format(mse, scientific = TRUE)))
  
  return <- best_lambda
}

best_lambda <- ten_fold_cv(X, y, 0)


## final model with best lambda on all data
lm.rid <- glmnet(X, sqrt(y), alpha = 0)
coef(lm.rid, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.rid, xvar = "lambda", label = TRUE)
abline(v=log(best_lambda), lty = 3, lwd = 2)

# R2

#use fitted best model to make predictions
lm.rid.pred <- predict(lm.rid, s = best_lambda, X)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((lm.rid.pred^2 - y)^2)

#find R-Squared
R2 <- 1 - sse/sst
R2
# R2 better than Exhaustive Subset Selection

# final MSE
mse.lm.rid <- mean((lm.rid.pred^2-y)^2)
mse.lm.rid
## mse worse than Exhaustive Subset Selection


### 10 most overpaid and 10 most underpaid players table ###
lm.rid.tables <- create_tables(y, lm.rid.pred^2, final_dataset, 10)
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
lm.las <- glmnet(X, sqrt(y), alpha = 1)
coef(lm.las, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.pred <- predict(lm.las, s = best_lambda, X)

# SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((lm.las.pred^2 - y)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# slightly better R2 than ridge

# final MSE
mse.lm.las <- mean((lm.las.pred^2-y)^2)
mse.lm.las
# slightly better MSE than ridge


### 10 most overpaid and 10 most underpaid players table ###
lm.las.tables <- create_tables(y, lm.las.pred^2, final_dataset, 10)
lm.las.tables[[1]]
lm.las.tables[[2]]


############################################
# LASSO REGRESSION FOR DIFFERENT POSITIONS #
############################################

#########
# ANOVA # 
#########

# Is there, on average, a difference between salaries of players with different positions?

fd_guard_df <- as.data.frame(fd_guard)
fd_forward_df <- as.data.frame(fd_forward)
fd_center_df <- as.data.frame(fd_center)
fd_roles_df <- rbind(fd_guard_df, fd_forward_df, fd_center_df)

bartlett.test(Salary ~ Pos, data = fd_roles_df)

aov.roles <- aov(Salary ~ Pos, data = fd_roles_df)
summary(aov.roles)


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

nrow(fd_center)
nrow(fd_forward)
nrow(fd_guard)


# remove position
fd_center_nopos <- fd_center[, numeric_cols]
fd_forward_nopos <- fd_forward[, numeric_cols]
fd_guard_nopos <- fd_guard[, numeric_cols]


#### LASSO FOR CENTER POSITION

regfit.center <- regsubsets(sqrt(Salary)~., data=fd_guard_nopos, nvmax=(ncol(fd_guard_nopos)-1))
reg.summary.c <- summary(regfit.center)

par(mfrow=c(2,2))

# residual sum of squares
plot(reg.summary.c$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(reg.summary.c$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
i <- which.max(reg.summary.c$adjr2)
points(i,reg.summary.c$adjr2[i], col="red",cex=2,pch=20)
text(i,reg.summary.c$adjr2[i], i, pos=1)

# Mallow's Cp with its smallest value
plot(reg.summary.c$cp,xlab="Number of Variables",ylab="Cp",type='l')
i <- which.min(reg.summary.c$cp)#return the index of the minimum
points(i,reg.summary.c$cp[i],col="red",cex=2,pch=20)
text(i,reg.summary.c$cp[i], i, pos=3)

# BIC with its smallest value
plot(reg.summary.c$bic,xlab="Number of Variables",ylab="BIC",type='l')
i <- which.min(reg.summary.c$bic)
points(i,reg.summary.c$bic[i],col="red",cex=2,pch=20)
text(i,reg.summary.c$bic[i], i, pos=3)

par(mfrow = c(1,1))

# It seems that selecting 14 parameters gives us the best 
# balance between model simplicity and precision. 

#let's get the list of selected parameters:
covariates = 6

selected.model.c <- reg.summary.c$which[covariates,]
selected.parameters.c <- names(selected.model.c[selected.model.c])[-1] #-1 to lose the intercept
print(selected.parameters.c)

selected.formula.c <- as.formula(paste("sqrt(Salary)~", paste(selected.parameters.c, collapse = " + ")))

lm.ess.center <- lm(selected.formula.c, data=fd_guard_nopos)
summary(lm.ess.center)

# correlation between dependent variables
corrplot(cor(fd_guard_nopos[c(selected.parameters.c)]), method = 'color')

# residual analysis
par(mfrow=c(2,2))
plot(lm.ess.center)
par(mfrow=c(1,1))

# model performances
y.c <- fd_guard_nopos$Salary
lm.ess.center.pred <- predict(lm.ess.center)
mse.lm.ess.c <- mean((lm.ess.center.pred)^2-y.c)^2
format(mse.lm.ess.c, scientific = TRUE)
format(sqrt(mse.lm.ess.c), scientific=TRUE)

# performances on a test set
X.c <- model.matrix(sqrt(Salary)~., data=fd_guard_nopos)
X.c <- X.c[,-1]
n <- nrow(X.c)

set.seed(1)
train <- sample(1:n, n/2)
test  <- setdiff(1:n, train)

lm.ess.center.test <- glmnet(X.c[train, ], sqrt(y.c[train]), alpha = 0, lambda = 0)
lm.ess.center.test.pred <- predict(lm.ess.center.test, s = 0, newx = X.c[test, ], exact = TRUE, x = X.c[train, ], y = y.c[train])
lm.ess.center.test.mse <- mean((lm.ess.center.test.pred^2 - y.c[test])^2)
print(paste("Estimated test MSE = ", format(lm.ess.center.test.mse, scientific = TRUE)))
format(sqrt(lm.ess.center.test.mse), scientific = TRUE)

lm.mod.c <- lm(sqrt(Salary)~., data=fd_center_nopos)
summary(lm.mod.c)


### Cross validation to select the best lambda ###
best_lambda <- ten_fold_cv(X.c, y.c, 1)

# final model with best lambda on all data
lm.las.c <- glmnet(X.c, sqrt(y.c), alpha = 1)
coef(lm.las.c, s=best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.c, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.c.pred <- predict(lm.las.c, s = best_lambda, X.c)

# SST and SSE
sst <- sum((y.c - mean(y.c))^2)
sse <- sum((lm.las.c.pred^2 - y.c)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# very high R2

# final MSE
mse.lm.las.c <- mean((lm.las.c.pred^2 - y.c)^2)
mse.lm.las.c
# better than before

### 3 most overpaid and 3 most underpaid centers table ###
lm.las.c.tables <- create_tables(y.c, lm.las.c.pred^2, fd_center_nopos, 3)
lm.las.c.tables[[1]]
lm.las.c.tables[[2]]


#### LASSO FOR FORWARD POSITION

lm.mod.f <- lm(sqrt(Salary)~., data=fd_forward_nopos)
summary(lm.mod.f)

# design matrix not considering the intercept
X.f <- model.matrix(sqrt(Salary)~., data=fd_forward_nopos)
X.f <- X.f[,-1]

# vector of responses
y.f <- fd_forward_nopos$Salary

### Cross validation to select the best lambda ###
best_lambda <- ten_fold_cv(X.f, y.f, 1)

# final model with best lambda on all data
lm.las.f <- glmnet(X.f, sqrt(y.f), alpha = 1)
coef(lm.las.f, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.f, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.f.pred <- predict(lm.las.f, s = best_lambda, X.f)

# SST and SSE
sst <- sum((y.f - mean(y.f))^2)
sse <- sum((lm.las.f.pred^2 - y.f)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# very high R2

# final MSE
mse.lm.las.f <- mean((lm.las.f.pred^2-y.f)^2)
mse.lm.las.f
# better than before

### 3 most overpaid and 3 most underpaid forwards table ###
lm.las.f.tables <- create_tables(y.f, lm.las.f.pred^2, fd_forward_nopos, 3)
lm.las.f.tables[[1]]
lm.las.f.tables[[2]]


#### LASSO FOR GUARD POSITION

lm.mod.g <- lm(sqrt(Salary)~., data=fd_guard_nopos)
summary(lm.mod.g)

# design matrix not considering the intercept
X.g <- model.matrix(sqrt(Salary)~., data=fd_guard_nopos)
X.g <- X.g[,-1]

# vector of responses
y.g <- fd_guard_nopos$Salary

### Cross validation to select the best lambda ###
best_lambda <- ten_fold_cv(X.g, y.g, 1)

# final model with best lambda on all data
lm.las.g <- glmnet(X.g, sqrt(y.g), alpha = 1)
coef(lm.las.g, s = best_lambda)

# Trace plot to visualize how the coefficient estimates changed as a result of increasing lambda
plot(lm.las.g, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), lty = 3, lwd = 2)

# use fitted best model to make predictions
lm.las.g.pred <- predict(lm.las.g, s = best_lambda, X.g)

# SST and SSE
sst <- sum((y.g - mean(y.g))^2)
sse <- sum((lm.las.g.pred^2 - y.g)^2)

# R-Squared
R2 <- 1 - sse/sst
R2
# worse R2 than centers and forwards

# final MSE
mse.lm.las.g <- mean((lm.las.g.pred^2-y.g)^2)
mse.lm.las.g
# worse than centers and forwards

### 3 most overpaid and 3 most underpaid centers table ###
lm.las.g.tables <- create_tables(y.g, lm.las.g.pred^2, fd_guard_nopos, 3)
lm.las.g.tables[[1]]
lm.las.g.tables[[2]]






