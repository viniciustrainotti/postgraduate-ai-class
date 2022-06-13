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

# A regress?o Ridge ? uma extens?o da regress?o linear em 
# que a fun??o de perda ? modificada para minimizar a 
# complexidade do modelo. Essa modifica??o ? feita 
# adicionando um par?metro de penalidade equivalente ao 
# quadrado da magnitude dos coeficientes.

# Uma das principais diferen?as entre os modelos de regress?o
# linear e regularizada ? que o ?ltimo envolve o ajuste de
# um hiperpar?metro, lambda. O c?digo executa o modelo 
# glmnet() v?rias vezes para diferentes valores de lambda. 
# Podemos automatizar essa tarefa de encontrar o valor lambda
# ideal usando a fun??o cv.glmnet(). Isso ? feito usando as
# linhas de c?digo abaixo.

# A fun??o perda ? dada por:
# Loss function = OLS+lambda*summation(squared coefficient
# values)
# Lambda ? o par?metro de penalidade que selecionamos

# The data for model

x = as.matrix(train_dummies)
y_train = train$price

x_test = as.matrix(test_dummies)
y_test = test$price

### The optimal lambda value ########
lambdas <- 10^seq(2, -3, by = -.1)
ridge_lamb <- cv.glmnet(x, y_train, alpha = 0, 
                        lambda = lambdas)
best_lambda_ridge <- ridge_lamb$lambda.min
best_lambda_ridge

ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, 
                   family = 'gaussian', 
                   lambda = best_lambda_ridge)

summary(ridge_reg)

# Obtendo os valores dos par?metros

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

# Como os valores de entrada no modelo est?o normalizados,
# temos de normalizar tamb?m os dados que queremos prever
# Observe que as vari?veis dummies n?o sofrem normaliza??o

summary(dat)

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

# ensuit = 3
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

# Construindo uma matriz com os dados para predi??o

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
# Fazendo a predi??o

predict_our_ridge <- predict(ridge_reg, s = best_lambda_ridge, 
                             newx = our_pred)
predict_our_ridge

# O resultado ? uma informa??o normalizada, vamos 
# convert?-la em valor nominal, compat?vel com a base de 
# dados original
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
