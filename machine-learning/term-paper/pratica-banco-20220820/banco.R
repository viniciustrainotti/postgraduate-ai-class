# Classificacao

## Banco

install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
library("kernlab")
library("e1071")
library("caret")

install.packages("mice")
library("mice")
# setar a sessão para onde checkoutar o código
# setwd("/opt/pos/r-ai-class/machine-learning/term-paper/pratica-banco-20220820")
dados <- read.csv("Material 03 - 11 – Banco - Dados.csv")
View(dados)

#dados$num <- NULL
#temp_dados <- dados
#imp <- mice(temp_dados)
#dados <- complete(imp,1)

set.seed(0758662190)
indices <- createDataPartition(dados$y, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

tuneGrid <- expand.grid(k = c(1,3,5,7,9))

### KNN 
set.seed(0758662190)
knn <- train(y ~ .,data = treino, method = "knn", tuneGrid=tuneGrid)
knn

predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$y))

### SVM Hold-out
set.seed(0758662190)
svm <- train(y ~ ., data=treino, method="svmRadial")
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$y))

### SVM CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
svm <- train(y ~ ., data=treino, method="svmRadial", trControl=ctrl)
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$y))

## SVM - Melhor Modelo

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(0758662190)
svm <- train(y ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$y))

## RNA - Hold-out

set.seed(0758662190)
rna <- train(y ~ ., data=treino, method="nnet", trace=FALSE)
rna
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$y))

## RNA - CV

ctrl <- trainControl(method = "cv", number = 10)
set.seed(0758662190)
rna <- train(y ~ ., data=treino, method="nnet", trace=FALSE, trControl=ctrl)
rna
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$y))

## RNA - Melhor Modelo

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10), decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(0758662190)
rna <- train(form= y~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$y))

## RF - Hold-out

set.seed(0758662190)
rf <- train(y ~ ., data=treino, method="rf", trace=FALSE)
rf
predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$y))

## RF - CV
ctrl <- trainControl(method = "cv", number = 10)
set.seed(0758662190)
rf <- train(y ~ ., data=treino, method="rf", trace=FALSE, trControl=ctrl)
rf
predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$y))

## RF - Melhor Modelo

tuneGrid <- expand.grid(mtry=c(2,5,7,9))

set.seed(0758662190)
rf <- train(y ~ ., data=treino, method="rf", trace=FALSE, trControl=ctrl, tuneGrid=tuneGrid)
rf
predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$y))

## SVM - Melhor Modelo - Predição novos casos

dados_novos_casos <- read.csv("Material 03 - 11 – Banco - Dados - Novos Casos.csv")
#dados_novos_casos$num <- NULL
View(dados_novos_casos)

predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
resultado$y <- NULL
View(resultado)
write.csv(resultado, "resultado-banco.csv")

## Salvar Modelo

getwd()
saveRDS(rf, "Material 03 - 11 – Banco - Dados - Melhor Modelo.rds")

modelo_lido <- readRDS("Material 03 - 11 – Banco - Dados - Melhor Modelo.rds")
novas_predicoes <- predict(modelo_lido, teste)
confusionMatrix(novas_predicoes, as.factor(teste$y))
