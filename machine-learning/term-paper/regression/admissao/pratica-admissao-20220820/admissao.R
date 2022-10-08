# Regressão

## Admissão

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

dados_tmp <- read.csv("Material 03 - 8 – R - Admissao - Dados.csv")
dados_tmp$Serial.No. <- NULL
View(dados_tmp)
imp <- mice(dados_tmp)
dados <- complete(imp, 1)
View(dados)

set.seed(0758662190)
indices <- createDataPartition(dados$ChanceOfAdmit, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

## RF - Hold-out

set.seed(0758662190)
rf <- train(ChanceOfAdmit~., data=treino, method="rf")
rf

predicoes.rf <- predict(rf, teste)

rmse(teste$ChanceOfAdmit, predicoes.rf)

r2 <- function(predito, observado) {
  return (1 - (sum((predito-observado)^2) / sum((observado-mean(observado))^2)))
}

r2(predicoes.rf, teste$ChanceOfAdmit)

syx <- function(observados, estimados, n, p)
{
  sqrt((sum((observados-estimados)^2))/(n-p-1))
}
syx(teste$ChanceOfAdmit, predicoes.rf, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.rf, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.rf)

## RF - CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
rf <- train(ChanceOfAdmit ~ ., data=treino, method = "rf", trControl=ctrl)
rf
predicoes.rf <- predict(rf, teste)

rmse(teste$ChanceOfAdmit, predicoes.rf)

r2(predicoes.rf, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.rf, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.rf, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.rf)

## RF - Melhor Modelo

tuneGrid = expand.grid(mtry=c(2,5,7,9))

set.seed(0758662190)
rf <- train(ChanceOfAdmit ~ ., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)
rf

predicoes.rf <- predict(rf, teste)

rmse(teste$ChanceOfAdmit, predicoes.rf)

r2(predicoes.rf, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.rf, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.rf, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.rf)

## RNA - Hold-out

set.seed(0758662190)
rna <- train(ChanceOfAdmit ~ ., data=treino, method="nnet", trace=FALSE)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$ChanceOfAdmit, predicoes.rna)

r2(predicoes.rna, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.rna, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.rna, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.rna)

## RNA - CV

ctrl <- trainControl(method = "cv", number = 10)
set.seed(0758662190)
rna <- train(ChanceOfAdmit ~ ., data=treino, method="nnet", trace=FALSE, trControl=ctrl)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$ChanceOfAdmit, predicoes.rna)

r2(predicoes.rna, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.rna, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.rna, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.rna)

## RNA - Melhor Modelo

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10), decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(0758662190)
rna <- train(form= ChanceOfAdmit~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$ChanceOfAdmit, predicoes.rna)

r2(predicoes.rna, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.rna, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.rna, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.rna)

## KNN

tuneGrid <- expand.grid(k = c(1,3,5,7,9))
set.seed(0758662190)
knn <- train(ChanceOfAdmit ~ .,data = treino, method = "knn", tuneGrid=tuneGrid)
knn

predicoes.knn <- predict(knn, teste)

rmse(teste$ChanceOfAdmit, predicoes.knn)

r2(predicoes.knn, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.knn, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.knn, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.knn)

## SVM - Hold-out

set.seed(0758662190)
svm <- train(ChanceOfAdmit ~ ., data=treino, method="svmRadial")
svm
predicoes.svm <- predict(svm, teste)

rmse(teste$ChanceOfAdmit, predicoes.svm)

r2(predicoes.svm, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.svm, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.svm, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.svm)

## SVM - CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
svm <- train(ChanceOfAdmit ~ ., data=treino, method="svmRadial", trControl=ctrl)
svm

predicoes.svm <- predict(svm, teste)

rmse(teste$ChanceOfAdmit, predicoes.svm)

r2(predicoes.svm, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.svm, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.svm, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.svm)

## SVM - Melhor Modelo

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(0758662190)
svm <- train(ChanceOfAdmit ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predicoes.svm <- predict(svm, teste)

rmse(teste$ChanceOfAdmit, predicoes.svm)

r2(predicoes.svm, teste$ChanceOfAdmit)

syx(teste$ChanceOfAdmit, predicoes.svm, length(teste$ChanceOfAdmit), length(dados))

res <- cor.test(teste$ChanceOfAdmit, predicoes.svm, method = "pearson")
res

MAE(teste$ChanceOfAdmit, predicoes.svm)

## Novos casos

## SVM - Melhor Modelo

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(0758662190)
svm <- train(ChanceOfAdmit ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predicoes.svm <- predict(svm, teste)

dados_novos_casos <- read.csv("Material 06 - 8 – R - Admissao - Novos Casos.csv")
View(dados_novos_casos)

dados_novos_casos$num <- NULL
dados_novos_casos$ChanceOfAdmit <- NULL
predicoes.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predicoes.svm)
View(resultado)
write.csv(resultado, "resultado-admissao.csv")
