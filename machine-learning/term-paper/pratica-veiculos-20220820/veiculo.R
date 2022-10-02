# Classificacao

## Veiculo

install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
library("kernlab")
library("e1071")
library("caret")

install.packages("mice")
library("mice")

dados <- read.csv("./Prática Veículos-20220820/Material 02 - 5 - C - Veiculos - Dados.csv")
View(dados)

dados$a <- NULL
temp_dados <- dados
imp <- mice(temp_dados)
dados <- complete(imp,1)

set.seed(0758662190)
indices <- createDataPartition(dados$tipo, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

tuneGrid <- expand.grid(k = c(1,3,5,7,9))

### KNN 
set.seed(0758662190)
knn <- train(tipo ~ .,data = treino, method = "knn", tuneGrid=tuneGrid)
knn

predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$tipo))

### SVM Hold-out
set.seed(0758662190)
svm <- train(tipo ~ ., data=treino, method="svmRadial")
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$tipo))

### SVM CV

ctrl <- trainControl(method = "cv", number = 10)

set.seed(0758662190)
svm <- train(tipo ~ ., data=treino, method="svmRadial", trControl=ctrl)
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$tipo))

## SVM - Melhor Modelo

tuneGrid <- expand.grid(C=c(1,2,10,50,100), sigma=c(.01, .015, 0.2))

set.seed(0758662190)
svm <- train(tipo ~ ., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$tipo))

## RNA - Hold-out

set.seed(0758662190)
rna <- train(tipo ~ ., data=treino, method="nnet", trace=FALSE)
rna
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$tipo))

## RNA - CV

ctrl <- trainControl(method = "cv", number = 10)
set.seed(0758662190)
rna <- train(tipo ~ ., data=treino, method="nnet", trace=FALSE, trControl=ctrl)
rna
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$tipo))

## RNA - Melhor Modelo

grid <- expand.grid(size = seq(from = 1, to = 45, by = 10), decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(0758662190)
rna <- train(form= tipo~., data=treino, method="nnet", tuneGrid=grid, trControl=ctrl, maxit=2000, trace=FALSE)
rna
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$tipo))

## RF - CV

set.seed(0758662190)
rf <- train(tipo ~ ., data=treino, method="rf", trace=FALSE)
rf
predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$tipo))

## RF - Hold-out
ctrl <- trainControl(method = "cv", number = 10)
set.seed(0758662190)
rf <- train(tipo ~ ., data=treino, method="rf", trace=FALSE, trControl=ctrl)
rf
predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$tipo))

## RF - Melhor Modelo

tuneGrid <- expand.grid(mtry=c(2,5,7,9))

set.seed(0758662190)
rf <- train(tipo ~ ., data=treino, method="rf", trace=FALSE, trControl=ctrl, tuneGrid=tuneGrid)
rf
predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$tipo))

## SVM - Melhor Modelo - Predição novos casos

dados_novos_casos <- read.csv("./Prática Veículos-20220820/Material 03 - 5 - C - Veiculos - Dados - Novos Casos.csv")
dados_novos_casos$a <- NULL
View(dados_novos_casos)

predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
resultado$tipo <- NULL
View(resultado)
write.csv(resultado, "./Prática Veículos-20220820/resultado.csv")

## Salvar Modelo

getwd()
saveRDS(svm, "Material 03 - 5 - C - Veiculos - Dados - Melhor Modelo.rds")

modelo_lido <- readRDS("Material 03 - 5 - C - Veiculos - Dados - Melhor Modelo.rds")
novas_predicoes <- predict(modelo_lido, teste)
confusionMatrix(novas_predicoes, as.factor(teste$tipo))