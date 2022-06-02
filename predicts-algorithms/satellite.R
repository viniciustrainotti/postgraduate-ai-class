# Satellite

install.packages("mlbench")
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
install.packages("RSNNS")

library("mlbench")
library("randomForest")
library("RSNNS")
library("caret")

# ??mlbench # help 

data(Satellite)
df <- Satellite

set.seed(7)

# Separar 80% em treino e 20% em teste

indices <- createDataPartition(df$classes, p=0.8, list=FALSE)
treino <- df[indices,]
teste <- df[-indices,]

# Random Forest

rf <- train(classes~., data=treino, method="rf")
predicoes.rf <- predict(rf, teste)
confusionMatrix(predicoes.rf, teste$classes) 

#SVM

svm <- train(classes~., data=treino, method="svmRadial")
predicoes.svm <- predict(svm, teste)
confusionMatrix(predicoes.svm, teste$classes) 

# RNA

rna <- train(classes~., data=treino, method="nnet", trace=FALSE)
predicoes.rna <- predict(rna, teste)
confusionMatrix(predicoes.rna, teste$classes) 

# Em análise a acucária o modelo escolhido foi: RandomForest

# FINAL

indices <- createDataPartition(df$classes, p=1, list=FALSE)
dados <- df[indices,]

rf <- train(classes~., data=dados, method="rf")
final.rf <- predict(rf, dados)
confusionMatrix(final.rf, dados$classes)
saveRDS(rf, "final.rf.rds")


#load("final.rf.rds")