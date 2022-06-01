# Regressao

install.packages("mlbench")
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
install.packages("RSNNS")

library("mlbench")
library("caret")
library("RSNNS")
library("randomForest")
library("RSNNS")

# ??mlbench # help 

data(Satellite)
df <- Satellite

set.seed(7)

# Separar 80% em treino e 20% em teste

indices <- createDataPartition(df$classes, p=0.80, list=FALSE)
treino <- df[indices,]
teste <- df[-indices,]

# Random Forest
#??train
rf <- train(classes~., data=treino, method="rf")
predicoes.rf <- predict(rf, teste)
confusionMatrix(predicoes.rf, teste$classes) 

#SVM

svm <- train(classes~., data=treino, method="svmRadial")
predicoes.svm <- predict(svm, teste)
confusionMatrix(predicoes.svm, teste$classes) 

# RNA

rna <- train(classes~., data=treino, method="nnet")
predicoes.rna <- predict(rna, teste)
confusionMatrix(predicoes.rna, teste$classes) 

save(rf, file="rf.RData") # salvar pra uso futuro
save(svm, file="svm.RData")
load("rf.RData")