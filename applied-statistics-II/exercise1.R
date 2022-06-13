# Exercise 1

install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("repr")
install.packages("glmnet")

library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

load("./imoveiscwbav.RData")

dat <- imoveiscwbav
glimpse(dat)
gc()

# Data partioning

set.seed(302) 

index = sample(1:nrow(dat), 0.8*nrow(dat)) 

train = dat[index,] # Create the training data 
test = dat[-index,] # Create the test data

dim(train)
dim(test)

# Scaling variables

cols = c('price', 'age', 'parea', 'tarea', 'bath', 'ensuit', 'garag', 'plaz', 'park',
         'trans', 'kidca', 'school', 'health', 'bike', 'barb', 'balc', 'elev', 'fitg',
         'party', 'categ')


pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))

train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])

summary(train)
summary(test)

# Regressão Ridge
# Regressão ridge reduz os coeficientes

# head(dat)

cols_reg = c('price', 'age', 'parea', 'tarea', 'bath', 'ensuit', 'garag', 'plaz', 'park',
             'trans', 'kidca', 'school', 'health', 'bike', 'barb', 'balc', 'elev', 'fitg',
             'party', 'categ')

dummies <- dummyVars(price ~ age+parea+tarea+bath+ensuit+garag+plaz+park+trans+
                     kidca+school+health+bike+barb+balc+elev+fitg+party+categ, 
                     data = dat[,cols_reg])

dummies

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))

# The data for model

x = as.matrix(train_dummies)
y_train = train$price

x_test = as.matrix(test_dummies)
y_test = test$price

# lambda

lambdas <- 10^seq(2, -3, by = -.1)
ridge_lamb <- cv.glmnet(x, y_train, alpha = 0, 
                        lambda = lambdas)
best_lambda_ridge <- ridge_lamb$lambda.min
best_lambda_ridge

ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, 
                   family = 'gaussian', 
                   lambda = best_lambda_ridge)

summary(ridge_reg)

# Parameters

ridge_reg[["beta"]]

# Compute R^2 from true and predicted values

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data

predictions_train <- predict(ridge_reg, s = best_lambda_ridge,
                             newx = x)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data

predictions_test <- predict(ridge_reg, s = best_lambda_ridge, 
                            newx = x_test)
eval_results(y_test, predictions_test, test)

# Prediction for our example

# Ordenary value

summary(dat) # Median 880000

#Para: 880000
# age = 9 anos
age = (9-pre_proc_val[["mean"]][["age"]])/
  pre_proc_val[["std"]][["age"]]

# parea = 120
parea = (120-pre_proc_val[["mean"]][["parea"]])/
  pre_proc_val[["std"]][["parea"]]

# tarea = 183
tarea = (183-pre_proc_val[["mean"]][["tarea"]])/
  pre_proc_val[["std"]][["tarea"]]

# bath = 3
bath = (3-pre_proc_val[["mean"]][["bath"]])/
  pre_proc_val[["std"]][["bath"]]

# ensuit = 1
ensuit = (1-pre_proc_val[["mean"]][["ensuit"]])/
  pre_proc_val[["std"]][["ensuit"]]

# garag = 2
garag = (2-pre_proc_val[["mean"]][["garag"]])/
  pre_proc_val[["std"]][["garag"]]

# plaz = 0.20671
plaz = (0.20671-pre_proc_val[["mean"]][["plaz"]])/
  pre_proc_val[["std"]][["plaz"]]

# park = 1.884
park = (1.884-pre_proc_val[["mean"]][["park"]])/
  pre_proc_val[["std"]][["park"]]

# trans = 2.0567
trans = (2.0567-pre_proc_val[["mean"]][["trans"]])/
  pre_proc_val[["std"]][["trans"]]

# kidca = 1.400
kidca = (1.400-pre_proc_val[["mean"]][["kidca"]])/
  pre_proc_val[["std"]][["kidca"]]

# school = 0.5675
school = (0.5675-pre_proc_val[["mean"]][["school"]])/
  pre_proc_val[["std"]][["school"]]

# health = 0.35788
health = (0.35788-pre_proc_val[["mean"]][["health"]])/
  pre_proc_val[["std"]][["health"]]

# bike = 0.3489
bike = (0.3489-pre_proc_val[["mean"]][["bike"]])/
  pre_proc_val[["std"]][["bike"]]

# barb = 0
barb = 0

balc = 0

elev = 0

fitg = 0

party = 1

categ = 1

# Predict Matrix

our_pred = as.matrix(data.frame(age=age,
                                parea=parea,
                                tarea=tarea,
                                bath=bath,
                                ensuit=ensuit,
                                garag=garag,
                                plaz=plaz,
                                park=park,
                                trans=trans,
                                kidca=kidca,
                                school=school,
                                health=health,
                                bike=bike,
                                barb=barb,
                                balc=balc,
                                elev=elev,
                                fitg=fitg,
                                party=party,
                                categ=categ))
# Preditc 

predict_our_ridge <- predict(ridge_reg, s = best_lambda_ridge, 
                             newx = our_pred)
predict_our_ridge

# Ordenary value

head(pre_proc_val)

price_pred_ridge=(predict_our_ridge*pre_proc_val[["std"]][["price"]])+pre_proc_val[["mean"]][["price"]]
price_pred_ridge

# Confident intervals for our example

n <- nrow(train)
m <- price_pred_ridge
s <- pre_proc_val[["std"]][["price"]]
dam <- s/sqrt(n)
CIlwr_ridge <- m + (qnorm(0.025))*dam
CIupr_ridge <- m - (qnorm(0.025))*dam 

CIlwr_ridge
CIupr_ridge


# Regressão Lasso

# best lambda

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_lamb <- cv.glmnet(x, y_train, alpha = 1, 
                        lambda = lambdas, 
                        standardize = TRUE, nfolds = 5)

# Best 
best_lambda_lasso <- lasso_lamb$lambda.min 
best_lambda_lasso

lasso_model <- glmnet(x, y_train, alpha = 1, 
                      lambda = best_lambda_lasso, 
                      standardize = TRUE)

# Parameters view

lasso_model[["beta"]]

# Predict train and test

predictions_train <- predict(lasso_model, s = best_lambda_lasso,
                             newx = x)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = best_lambda_lasso, 
                            newx = x_test)
eval_results(y_test, predictions_test, test)

## Prediction for our example

# our_pred from Ridge model

predict_our_lasso <- predict(lasso_model, s = best_lambda_lasso, 
                             newx = our_pred)
predict_our_lasso

# Ordenary value

price_pred_lasso=(predict_our_lasso*
                   pre_proc_val[["std"]][["price"]])+
  pre_proc_val[["mean"]][["price"]]
price_pred_lasso

# Confident intervals for our example

n <- nrow(train)
m <- price_pred_lasso
s <- pre_proc_val[["std"]][["price"]]
dam <- s/sqrt(n)
CIlwr_lasso <- m + (qnorm(0.025))*dam
CIupr_lasso <- m - (qnorm(0.025))*dam 

CIlwr_lasso
CIupr_lasso


# Elasticnet

# Set training control

train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

# Train the model

elastic_reg <- train(price ~ age+parea+tarea+bath+ensuit+garag+plaz+park+trans+
                     kidca+school+health+bike+barb+balc+elev+fitg+party+categ,
                     data = train,
                     method = "glmnet",
                     tuneLength = 10,
                     trControl = train_cont)

# Best tuning parameter

elastic_reg$bestTune

# And the parameters are:

elastic_reg[["finalModel"]][["beta"]]

# Make predictions on training set

predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set

predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

## Prediction for our example

predict_our_elastic <- predict(elastic_reg,our_pred)
predict_our_elastic

# Ordenary value

price_pred_elastic=(predict_our_elastic*
                     pre_proc_val[["std"]][["price"]])+
  pre_proc_val[["mean"]][["price"]]
price_pred_elastic

# Confident intervals for our example

n <- nrow(train)
m <- price_pred_elastic
s <- pre_proc_val[["std"]][["price"]]
dam <- s/sqrt(n)
CIlwr_elastic <- m + (qnorm(0.025))*dam
CIupr_elastic <- m - (qnorm(0.025))*dam 

CIlwr_elastic
CIupr_elastic
