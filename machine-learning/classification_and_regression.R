# Classificação - Íris

install.packages("e1071")
install.packages("caret")
library("caret")

data("iris")
dados <- iris
View(dados)

set.seed(1912)
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,]
teste <- dados[-ran,]

# valores para K para treinamento
tuneGrid <- expand.grid(k = c(1,3,5,7,9))
set.seed(1912)
knn <- train(Species ~ ., data = treino, method = "knn", tuneGrid=tuneGrid)
knn

# final value for the model was k = 1 

predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$Species))

# Accuracy : 0.9667

# --

# Câncer de Mama - Classificação

install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
library("kernlab")
library("e1071")
library("caret")

install.packages("mice")
library("mice")

dados <- read.csv("Material 02 - 2 - Cancer de Mama - Dados.csv")
View(dados)

dados$Id <- NULL

set.seed(1912)
indices <- createDataPartition(dados$Class, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

## SVM Hold-out
set.seed(1912)
svm <- train(Class ~ ., data=treino, method="svmRadial")
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$Class))

## SVM CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
svm <- train(Class ~ ., data=treino, method="svmRadial", trControl=ctrl)
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$Class))

## SVM Testes para melhor Modelo

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(1912)
svm <- train(Class ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$Class))

## SVM Predição novos casos

dados_novos_casos <- read.csv("Material 02 - 2 - Cancer de Mama - Dados - Novos Casos.csv")
dados_novos_casos$Id <- NULL
View(dados_novos_casos)

predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
resultado$Class <- NULL
View(resultado)

## Salvar Modelo e leitura a qualquer momento

getwd()
# Melhor acuracia 0.9659717, foi com C=10 e sigma=0.015
saveRDS(svm, "Material 04 - 2 - Cancer de Mama - R - Melhor Modelo.rds")

modelo_lido <- readRDS("Material 04 - 2 - Cancer de Mama - R - Melhor Modelo.rds")
novas_predicoes <- predict(modelo_lido, teste)
confusionMatrix(novas_predicoes, as.factor(teste$Class))

# Câncer de Mama - Regressão

install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
library("kernlab")
library("e1071")
library("caret")

install.packages("Metrics")
library("Metrics")

dados <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados.csv")
View(dados)

set.seed(1912)
indices <- createDataPartition(dados$Volume, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

## SVM Hold-out

set.seed(1912)
svm <- train(Volume ~ ., data=treino, method="svmRadial")
svm
predicoes.svm <- predict(svm, teste)

## Calcular metricas RMSE e R2

library("Metrics")
library("caret")
rmse(teste$Volume, predicoes.svm)

r2 <- function(predito, observado){
  return(1 - (sum((predito-observado)^2 / sum((predito-mean(observado))^2))))
}
r2(predicoes.svm, teste$Volume)

## SVM CV 
crtl <- trainControl(method = "cv", number = 10)

set.seed(1912)
svm <- train(Volume ~ . , data=treino, method="svmRadial", trControl=ctrl)
svm
predicoes.svm <- predict(svm, teste)

## Calcular metricas

rmse(teste$Volume, predicoes.svm)

r2 <- function(predito, observado){
  return(1 - (sum((predito-observado)^2 / sum((predito-mean(observado))^2))))
}
r2(predicoes.svm, teste$Volume)

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(1912)
svm <- train(Volume ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predict.svm <- predict(svm, teste)

rmse(teste$Volume, predicoes.svm)

r2 <- function(predito, observado){
  return(1 - (sum((predito-observado)^2 / sum((predito-mean(observado))^2))))
}
r2(predicoes.svm, teste$Volume)

## Predicoes de novos casos

dados_novos_casos <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados - Novos Casos.csv")
View(dados_novos_casos)

dados_novos_casos$Volume <- NULL
predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
View(resultado)

## Random Forest - Classificação - Cancer de Mama

install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
install.packages("randomForest")
library("caret")

install.packages("mice")
library("mice")

temp_dados <- read.csv("Material 02 - 2 - Cancer de Mama - Dados.csv")
View(temp_dados)

temp_dados$Id <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)

set.seed(1912)
indices <- createDataPartition(dados$Class, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

set.seed(1912)
rf <- train(Class ~ ., data=treino, method="rf")
rf

predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$Class))

ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
rf <- train(Class ~., data=treino, method="rf", trControl=ctrl)
rf

predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$Class))

tuneGrid = expand.grid(mtry=c(2,5,7,9))

set.seed(1912)
rf <- train(Class~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)
rf

predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$Class))

## Predição de Novos Casos

dados_novos_casos <- read.csv("Material 02 - 2 - Cancer de Mama - Dados - Novos Casos.csv")
dados_novos_casos$Id <- NULL
View(dados_novos_casos)

predict.rf <- predict(rf, dados_novos_casos)
dados_novos_casos$Class <- NULL
resultado <- cbind(dados_novos_casos, predict.rf)
View(resultado)

## Random Forest - Regressão - Cancer de Mama

dados <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados.csv")
View(dados)

set.seed(1912)
indices <- createDataPartition(dados$Volume, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

set.seed(1912)
rf <- train(Volume~., data=treino, method="rf")
rf

predicoes.rf <- predict(rf, teste)

install.packages("Metrics")
library("Metrics")

rmse(teste$Volume, predicoes.rf)

r2 <- function(predito, observado) {
  return (1 - (sum((predito-observado)^2) / sum((observado-mean(observado))^2)))
}

r2(predicoes.rf, teste$Volume)

ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
rf <- train(Volume ~ ., data=treino, method = "rf", trControl=ctrl)
rf
predicoes.rf <- predict(rf, teste)

rmse(teste$Volume, predicoes.rf)

r2(predicoes.rf, teste$Volume)

tuneGrid = expand.grid(mtry=c(2,5,7,9))

set.seed(1912)
rf <- train(Volume ~ ., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)
rf

predicoes.rf <- predict(rf, teste)
rmse(teste$Volume, predicoes.rf)
r2(predicoes.rf, teste$Volume)

dados_novos_casos <- read.csv("Material 02 - 3 – Estimativa de Volume - Dados - Novos Casos.csv")
View(dados_novos_casos)


dados_novos_casos$Volume <- NULL
predict.rf <- predict(rf, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.rf)
View(resultado)
