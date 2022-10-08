# Regressão

## Biomassa

install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
install.packages("randomForest")
library("kernlab")
library("e1071")
library("randomForest")
library("caret")

install.packages("mice")
library("mice")

install.packages("Metrics")
library("Metrics")

set.seed(0758662190)

dados_tmp <- read.csv("Material 02 - 4 - R - Biomassa - Dados.csv")
imp <- mice(dados_tmp) 
dados <- complete(imp, 1)
View(dados)

set.seed(0758662190)
indices <- createDataPartition(dados$biomassa, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

## RF - Hold-out

set.seed(0758662190)
rf <- train(biomassa~., data=treino, method="rf")
rf

predicoes.rf <- predict(rf, teste)

rmse(teste$biomassa, predicoes.rf)

r2 <- function(predito, observado) {
  return (1 - (sum((predito-observado)^2) / sum((observado-mean(observado))^2)))
}

r2(predicoes.rf, teste$biomassa)

syx <- function(observados, estimados, n, p)
{
  sqrt((sum((observados-estimados)^2))/(n-p-1))
}
syx(teste$biomassa, predicoes.rf, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.rf, method = "pearson")
res

MAE(teste$biomassa, predicoes.rf)

## RF - CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
rf <- train(biomassa ~ ., data=treino, method = "rf", trControl=ctrl)
rf
predicoes.rf <- predict(rf, teste)

rmse(teste$biomassa, predicoes.rf)

r2(predicoes.rf, teste$biomassa)

syx(teste$biomassa, predicoes.rf, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.rf, method = "pearson")
res

MAE(teste$biomassa, predicoes.rf)

## RF - Melhor Modelo

tuneGrid = expand.grid(mtry=c(2,5,7,9))

set.seed(0758662190)
rf <- train(biomassa ~ ., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)
rf

predicoes.rf <- predict(rf, teste)

rmse(teste$biomassa, predicoes.rf)

r2(predicoes.rf, teste$biomassa)

syx(teste$biomassa, predicoes.rf, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.rf, method = "pearson")
res

MAE(teste$biomassa, predicoes.rf)

## RNA - Hold-out

set.seed(0758662190)
rna <- train(biomassa ~ ., data=treino, method="nnet", trace=FALSE)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$biomassa, predicoes.rna)

r2(predicoes.rna, teste$biomassa)

syx(teste$biomassa, predicoes.rna, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.rna, method = "pearson")
res

MAE(teste$biomassa, predicoes.rna)

## RNA - CV

ctrl <- trainControl(method = "cv", number = 10)
set.seed(0758662190)
rna <- train(biomassa ~ ., data=treino, method="nnet", trace=FALSE, trControl=ctrl)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$biomassa, predicoes.rna)

r2(predicoes.rna, teste$biomassa)

syx(teste$biomassa, predicoes.rna, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.rna, method = "pearson")
res

MAE(teste$biomassa, predicoes.rna)

## RNA - Melhor Modelo

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10), decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(0758662190)
rna <- train(form= biomassa~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$biomassa, predicoes.rna)

r2(predicoes.rna, teste$biomassa)

syx(teste$biomassa, predicoes.rna, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.rna, method = "pearson")
res

MAE(teste$biomassa, predicoes.rna)

## KNN

tuneGrid <- expand.grid(k = c(1,3,5,7,9))
set.seed(0758662190)
knn <- train(biomassa ~ .,data = treino, method = "knn", tuneGrid=tuneGrid)
knn

predicoes.knn <- predict(knn, teste)

rmse(teste$biomassa, predicoes.knn)

r2(predicoes.knn, teste$biomassa)

syx(teste$biomassa, predicoes.knn, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.knn, method = "pearson")
res

MAE(teste$biomassa, predicoes.knn)

## SVM - Hold-out

set.seed(0758662190)
svm <- train(biomassa ~ ., data=treino, method="svmRadial")
svm
predicoes.svm <- predict(svm, teste)

rmse(teste$biomassa, predicoes.svm)

r2(predicoes.svm, teste$biomassa)

syx(teste$biomassa, predicoes.svm, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.svm, method = "pearson")
res

MAE(teste$biomassa, predicoes.svm)

## SVM - CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
svm <- train(biomassa ~ ., data=treino, method="svmRadial", trControl=ctrl)
svm

predicoes.svm <- predict(svm, teste)

rmse(teste$biomassa, predicoes.svm)

r2(predicoes.svm, teste$biomassa)

syx(teste$biomassa, predicoes.svm, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.svm, method = "pearson")
res

MAE(teste$biomassa, predicoes.svm)

## SVM - Melhor Modelo

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(0758662190)
svm <- train(biomassa ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predicoes.svm <- predict(svm, teste)

rmse(teste$biomassa, predicoes.svm)

r2(predicoes.svm, teste$biomassa)

syx(teste$biomassa, predicoes.svm, length(teste$biomassa), length(dados))

res <- cor.test(teste$biomassa, predicoes.svm, method = "pearson")
res

MAE(teste$biomassa, predicoes.svm)

## Novos casos

## RF - CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
rf <- train(biomassa ~ ., data=treino, method = "rf", trControl=ctrl)
rf
predicoes.rf <- predict(rf, teste)

dados_novos_casos <- read.csv("Material 02 - 4 - R - Biomassa - Dados - Novos Casos.csv")
View(dados_novos_casos)

dados_novos_casos$biomassa <- NULL
predicoes.rf <- predict(rf, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predicoes.rf)
View(resultado)
write.csv(resultado, "resultado-biomassa.csv")

resid <- resid(rf)

plot(fitted(rf), resid, main="Residuos Biomassa")
abline(0,0)
