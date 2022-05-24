# Regressao

install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
install.packages("RSNNS")

library("caret")
library("RSNNS")

df <- read.csv("C:/Users/vtrai/Desktop/Docs POS/Materias/2 - R/IAA004 - Linguagem R/Real-estate.csv", sep=",", dec=",")

set.seed(7)

# Separar 80% em treino e 20% em teste

indices <- createDataPartition(df$Y.house.price.of.unit.area, p=0.80, list=FALSE)
treino <- df[indices,]
teste <- df[-indices,]

# Random Forest

rf <- train(Y.house.price.of.unit.area ~ ., data=treino, method="rf")
predicoes.rf <- predict(rf, teste)

#SVM

svm <- train(Y.house.price.of.unit.area~., data=treino, method="svmRadial")
predicoes.svm <- predict(svm, teste)

# RNA

rna <- train(Y.house.price.of.unit.area~., data=treino, method="nnet")
predicoes.rna <- predict(rna, teste)

# MLP

model.mlp <- mlp(treino[,1:7], treino[,8], linOut=T)
predicoes.mlp <- predict(model.mlp, teste[,1:7])

# Aplicar a Regressão

rmse.rf <- RMSE(predicoes.rf, teste$Y.house.price.of.unit.area)
rmse.svm <- RMSE(predicoes.svm, teste$Y.house.price.of.unit.area)
rmse.rna <- RMSE(predicoes.rna, teste$Y.house.price.of.unit.area)
rmse.mlp <- RMSE(predicoes.mlp, teste$Y.house.price.of.unit.area)

cat("RMSE RF: ", rmse.rf, "\n")
cat("RMSE SVM: ", rmse.svm, "\n")
cat("RMSE RNA: ", rmse.rna, "\n")
cat("RMSE MLP: ", rmse.mlp, "\n")
