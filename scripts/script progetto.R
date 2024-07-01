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
NA_number <- colSums(is.na(final_dataset))
print(NA_number)


# removing PFD.x and renaming PFD.y as PFD
final_dataset <- final_dataset[, -17]
colnames(final_dataset)[colnames(final_dataset) == 'PFD.y'] <- 'PFD'

# renaming the column 2023/2024 in "Salary", removing the "$" sign and changing the data class in numeric
colnames(final_dataset)[colnames(final_dataset) == '2023/24'] <- 'Salary'
final_dataset$Salary <- as.numeric(gsub("[\\$\\,]", "", final_dataset$Salary))
class(final_dataset$Salary)

###########
### EDA ###
###########

attach(final_dataset)

# only numeric columns for EDA
numeric_cols <- sapply(final_dataset, is.numeric)
fd_numeric <- final_dataset[, numeric_cols]
summary(fd_numeric)

# Variables EDA

# variable Salary (dipendent)
boxplot(Salary, main="Boxplot of the salary")
summary(Salary)
hist(Salary, main="Histogram of the salary")
boxplot(log(Salary), main="Boxplot of the logarithmic salary")
hist(log(Salary), main="Histogram of the logarithmic salary")      # forma più regolare, ulteriore motivo per usare il logaritmo

par(mfrow = c(2, 2))
boxplot(Salary, main="Boxplot of the salary")
hist(Salary, main="Histogram of the salary")
boxplot(log(Salary), main="Boxplot of the logarithmic salary")
hist(log(Salary), main="Histogram of the logarithmic salary")


# Independent variables
boxplot(AGE, names=c("AGE"), show.names=TRUE)
boxplot(GP, names=c("GP"), show.names=TRUE)
boxplot(MIN, names=c("MIN"), show.names=TRUE)
boxplot(MIN_G, PTS, names=c("MIN_G", "PTS"))
boxplot(OREB, DREB, REB, AST, names=c("OREB", "DREB", "REB", "AST"))
boxplot(TOV, STL, BLK, BLKA, PF, PFD, names=c("TOV", "STL", "BLK", "BLKA", "PF", "PFD"))
boxplot(FG_PCT, FG3_PCT, FT_PCT, TS_PCT, names=c("FG_PCT", "FG3_PCT", "FT_PCT", "TS_PCT"))
boxplot(OFF_RATING, DEF_RATING, names=c("OFF_RATING", "DEF_RATING"))
boxplot(NET_RATING, names=c("NET_RATING"), show.names=TRUE)
boxplot(AST_TO, names=c("AST_TO"), show.names=TRUE)
boxplot(PIE, USG_PCT, names=c("PIE", "USG_PCT"))
boxplot(WS, BPM, VORP, names=c("WS", "BPM", "VORP"))


### Analyze correlations

# covariance and correlation matrices
cov_mat <- round(cov(fd_numeric),2)
cov_mat
cor_mat <- round(cor(fd_numeric),2)
cor_mat

library(corrplot)

corrplot(cor(fd_numeric), method = 'color')
corrplot(cor(fd_numeric), method = 'ellipse')

# FUNCTION "pairs" for matrix plot

# define the functions "panel.hist" and "panel.cor"
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# execute pairs function
# dev.new(width=10, height=10)
pairs(fd_numeric, diag.panel=panel.hist, upper.panel=panel.cor)
pairs(fd_numeric, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)

## we can notice a lot of correlations between the variables;

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

# interpretation 

# Residual analysis
par(mfrow = c(2, 2))
plot(lm.mod)

## QQ plot ok, check linearity residuals vs fitted; check eteroschedasticity scale-location

# trying to transform the response variable
lm.log <- lm(log(Salary)~+., data=fd_numeric)
summary(lm.log)

plot(lm.log)

## better linearity residuals vs fitted(1st plot), better with omoschedasticity(3rd plot)

## stepwise regression
lm.step <- step(lm.mod)
summary(lm.step)  # model with the lowest AIC, interpretation

lm.step$coefficients
cor(fd_numeric[c("AGE", "FG_PCT", "DREB", "TOV", "BLKA", "PF", 
                  "PTS", "OFF_RATING", "DEF_RATING", "NET_RATING", "TS_PCT", "PIE", 
                  "MIN", "MIN_G", "WS")])


lm.step.coeff <- fd_numeric[c("AGE", "FG_PCT", "DREB", "TOV", "BLKA", "PF", 
                              "PTS", "OFF_RATING", "DEF_RATING", "NET_RATING", "TS_PCT", "PIE", 
                              "MIN", "MIN_G", "WS")]  # interpretation

par(mfrow = c(1,1))
corrplot(cor(lm.step.coeff), method = 'number')  # we can observe strong correlations (commentare) so...go with ridge

#############################################################################
## for the ridge reg, we can interpret the coefficients of the last model
## (at least the signs of the better one) but the main idea is to compare the
## performances (MSE) of the two models and compare them to the real salaries
#############################################################################

# let's try to evaluate the performances of the stepwise model

# Define the number of folds for cross-validation
num_folds <- 10

# Set the seed for reproducibility
set.seed(123)

# Initialize vectors to store performance metrics
mse <- numeric(num_folds)
rsquared <- numeric(num_folds)

# Perform k-fold cross-validation
for (i in 1:num_folds) {
  # Create indices for train and test sets
  test_indices <- ((i - 1) * nrow(fd_numeric) / num_folds + 1):(i * nrow(fd_numeric) / num_folds)
  train_data <- fd_numeric[-test_indices, ]
  test_data <- fd_numeric[test_indices, ]
  
  # Fit the linear regression model on the training data
  model <- lm(Salary ~ AGE + FG_PCT + DREB + TOV + BLKA + PF + 
                PTS + OFF_RATING + DEF_RATING + NET_RATING + TS_PCT + PIE + 
                MIN + MIN, data = train_data)
  
  # Make predictions on the test data
  predictions <- predict(model, newdata = test_data)
  
  # Calculate performance metrics
  mse[i] <- mean((test_data$Salary - predictions)^2)
  rsquared[i] <- summary(model)$r.squared
}

# Calculate average performance metrics
avg_mse <- mean(mse)
avg_rsquared <- mean(rsquared)

# Print average performance metrics
cat("Average MSE:", avg_mse, "\n")
cat("Average R-squared:", avg_rsquared, "\n")


### si può provare ad utilizzare la stepwise sul modello con il logaritmo perchè le assunzioni 
### del modello lineare sono meglio rispettate; nel for qui sopra, in "calculate performance metrics"
### bisogna sostituire predictions con e^predictions

lm.step.log <- step(lm.log)

summary(lm.step.log)
lm.step.log$coefficients

cor(fd_numeric[c("AGE", "FG_PCT", "BLK", "BLKA", "PTS", 
                   "OFF_RATING", "DEF_RATING", "NET_RATING", "TS_PCT", "PIE", "MIN_G", 
                   "WS")])

# Define the number of folds for cross-validation
num_folds <- 10

# Set the seed for reproducibility
set.seed(123)

# Initialize vectors to store performance metrics
mse_log <- numeric(num_folds)
rsquared_log <- numeric(num_folds)


# Perform k-fold cross-validation
for (i in 1:num_folds) {
  # Create indices for train and test sets
  test_indices <- ((i - 1) * nrow(fd_numeric) / num_folds + 1):(i * nrow(fd_numeric) / num_folds)
  train_data <- fd_numeric[-test_indices, ]
  test_data <- fd_numeric[test_indices, ]
  
  # Fit the linear regression model on the training data
  model <- lm(log(Salary) ~ AGE + FG_PCT + BLK + BLKA + PTS + 
                OFF_RATING + DEF_RATING + NET_RATING + TS_PCT + PIE + MIN_G + 
                WS, data = train_data)
  
  # Make predictions on the test data
  predictions <- predict(model, newdata = test_data)
  
  # Calculate performance metrics
  mse_log[i] <- mean((test_data$Salary - 2.71^predictions)^2)
  rsquared_log[i] <- summary(model)$r.squared
}

# Calculate average performance metrics
avg_mse_log <- mean(mse_log)
avg_rsquared_log <- mean(rsquared_log)


# Print average performance metrics
cat("Average MSE:", avg_mse_log, "\n")
cat("Average R-squared:", avg_rsquared_log, "\n")

## usando il logaritmo MSE è più alto, dobbiamo valutare quale modello stepsize tenere

## compare models prediction with actual salaries (plot model salaries vs real salaries)

pred_values <- predict(lm.step)
plot(pred_values, Salary)

## highlight the biggest differences (different colors for + and -)

res <- lm.step$residuals

largest_residuals_indices <- order(abs(res), decreasing=TRUE)

salaries <- fd_numeric[c(32,191,310,349,96,115,184,78,158,334),c("Salary","MIN_G")]

pred_values1 <- pred_values[c(32,191,310,349,96,115,184,78,158,334)]

res_values <- res[c(32,191,310,349,96,115,184,78,158,334)]

matching <- final_dataset[final_dataset$Salary %in% salaries$Salary & final_dataset$MIN_G %in% salaries$MIN_G, ]
matching_sort <- matching[order(matching$Salary),]

sal_pred_res <- cbind(salaries$Salary,pred_values1,res_values)
sal_pred_res_sort <- sal_pred_res[order(sal_pred_res[,1]),]

tab1 <- cbind(matching_sort$PLAYER_NAME,sal_pred_res_sort)

## improve: 10 overpaid and 10 underpaid


#############################
# RIDGE REGRESSION          #
#############################

## linear model
lm.mod <- lm(Salary~+., data=fd_numeric)
summary(lm.mod)

# design matrix without the first column 
# because we do not consider the intercept
X <- model.matrix(Salary~., data=fd_numeric)
X <- X[,-1]

# vector of responses
y <- fd_numeric$Salary


# package for ridge regression
library(glmnet)

### Cross validation to select the best lambda ###

# select n/2 observations for training set
n <- nrow(X)
n/2

set.seed(1)
train <- sample(1:n, 180)
test  <- setdiff(1:n, train)

cv.out <- cv.glmnet(X[train, ], y[train], alpha = 0, nfold=10)

# This plots the cross-validation curve (red dotted line) along with upper and lower standard deviation curves
# along the lambda sequence (error bars). Two special values along the lambda sequence are indicated by the vertical
# dotted lines. lambda.min is the value of lambda that gives minimum mean cross-validated error, while lambda.1se
# is the value of lambda that gives the most regularized model such that the cross-validated error is within one
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

test.mse.ridge <- mean((ridge.final.pred-y)^2)
test.mse.ridge

## mse meglio della stepwise


## fare prediction e confrontarle con stipendi reali


diffs <- ridge.final.pred - fd_numeric$Salary
class(diffs)

ord_indexes <- order(abs(diffs), decreasing = TRUE)
big_diffs_ind <- ord_indexes[1:10]

## predicted salary (ridge)
pred_sal_ridge <- ridge.final.pred[big_diffs_ind]

## actual salary and player names
fd_ridge <- final_dataset[c(big_diffs_ind), ]
fd_ridge_cut <- fd_ridge[c('PLAYER_NAME', 'Salary')]

## final table for ridge regression
tab2 <- cbind(fd_ridge_cut, pred_sal_ridge, fd_ridge_cut$Salary-pred_sal_ridge)
tab2

#### the ridge regression shows better results compared to stepwise regression. One of 
#### the reasons is the fact that ridge regression works well with collinear variables,
#### even with a small lambda

#### analyze the differences between tab1 and tab2 ####


############################################
# MODELLO MIGLIORE FOR DIFFERENT POSITIONS #
############################################
