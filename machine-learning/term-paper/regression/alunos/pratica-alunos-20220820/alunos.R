# Regressão

## Alunos

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

dados_tmp <- read.csv("Material 03 - 10 – Alunos - Dados.csv")
View(dados_tmp)
imp <- mice(dados_tmp)
dados <- complete(imp, 1)
View(dados)

set.seed(0758662190)
indices <- createDataPartition(dados$G3, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

## RF - Hold-out

set.seed(0758662190)
rf <- train(G3~., data=treino, method="rf")
rf

predicoes.rf <- predict(rf, teste)

rmse(teste$G3, predicoes.rf)

r2 <- function(predito, observado) {
  return (1 - (sum((predito-observado)^2) / sum((observado-mean(observado))^2)))
}

r2(predicoes.rf, teste$G3)

syx <- function(observados, estimados, n, p)
{
  sqrt((sum((observados-estimados)^2))/(n-p-1))
}
syx(teste$G3, predicoes.rf, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.rf, method = "pearson")
res

MAE(teste$G3, predicoes.rf)

## RF - CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
rf <- train(G3 ~ ., data=treino, method = "rf", trControl=ctrl)
rf
predicoes.rf <- predict(rf, teste)

rmse(teste$G3, predicoes.rf)

r2(predicoes.rf, teste$G3)

syx(teste$G3, predicoes.rf, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.rf, method = "pearson")
res

MAE(teste$G3, predicoes.rf)

## RF - Melhor Modelo

tuneGrid = expand.grid(mtry=c(2,5,7,9))

set.seed(0758662190)
rf <- train(G3 ~ ., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)
rf

predicoes.rf <- predict(rf, teste)

rmse(teste$G3, predicoes.rf)

r2(predicoes.rf, teste$G3)

syx(teste$G3, predicoes.rf, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.rf, method = "pearson")
res

MAE(teste$G3, predicoes.rf)

## RNA - Hold-out

set.seed(0758662190)
rna <- train(G3 ~ ., data=treino, method="nnet", trace=FALSE)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$G3, predicoes.rna)

r2(predicoes.rna, teste$G3)

syx(teste$G3, predicoes.rna, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.rna, method = "pearson")
res

MAE(teste$G3, predicoes.rna)

## RNA - CV

ctrl <- trainControl(method = "cv", number = 10)
set.seed(0758662190)
rna <- train(G3 ~ ., data=treino, method="nnet", trace=FALSE, trControl=ctrl)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$G3, predicoes.rna)

r2(predicoes.rna, teste$G3)

syx(teste$G3, predicoes.rna, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.rna, method = "pearson")
res

MAE(teste$G3, predicoes.rna)

## RNA - Melhor Modelo

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10), decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(0758662190)
rna <- train(form= G3~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna
predicoes.rna <- predict(rna, teste)

rmse(teste$G3, predicoes.rna)

r2(predicoes.rna, teste$G3)

syx(teste$G3, predicoes.rna, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.rna, method = "pearson")
res

MAE(teste$G3, predicoes.rna)

## KNN

tuneGrid <- expand.grid(k = c(1,3,5,7,9))
set.seed(0758662190)
knn <- train(G3 ~ .,data = treino, method = "knn", tuneGrid=tuneGrid)
knn

predicoes.knn <- predict(knn, teste)

rmse(teste$G3, predicoes.knn)

r2(predicoes.knn, teste$G3)

syx(teste$G3, predicoes.knn, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.knn, method = "pearson")
res

MAE(teste$G3, predicoes.knn)

## SVM - Hold-out

set.seed(0758662190)
svm <- train(G3 ~ ., data=treino, method="svmRadial")
svm
predicoes.svm <- predict(svm, teste)

rmse(teste$G3, predicoes.svm)

r2(predicoes.svm, teste$G3)

syx(teste$G3, predicoes.svm, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.svm, method = "pearson")
res

MAE(teste$G3, predicoes.svm)

## SVM - CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
svm <- train(G3 ~ ., data=treino, method="svmRadial", trControl=ctrl)
svm

predicoes.svm <- predict(svm, teste)

rmse(teste$G3, predicoes.svm)

r2(predicoes.svm, teste$G3)

syx(teste$G3, predicoes.svm, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.svm, method = "pearson")
res

MAE(teste$G3, predicoes.svm)

## SVM - Melhor Modelo

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(0758662190)
svm <- train(G3 ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predicoes.svm <- predict(svm, teste)

rmse(teste$G3, predicoes.svm)

r2(predicoes.svm, teste$G3)

syx(teste$G3, predicoes.svm, length(teste$G3), length(dados))

res <- cor.test(teste$G3, predicoes.svm, method = "pearson")
res

MAE(teste$G3, predicoes.svm)

## Novos casos

## RF - Hold-out - Melhor Modelo

set.seed(0758662190)
rf <- train(G3~., data=treino, method="rf")
rf

predicoes.rf <- predict(rf, teste)

dados_novos_casos <- read.csv("Material 03 - 10 – Alunos - Dados - Novos Casos.csv")
View(dados_novos_casos)

dados_novos_casos$G3 <- NULL
predicoes.rf <- predict(rf, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predicoes.rf)
View(resultado)
write.csv(resultado, "resultado-aluno.csv")
