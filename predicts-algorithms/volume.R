# Volume

install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
install.packages("RSNNS")

library("caret")

# ??mlbench # help 

dados <- read.csv("http://www.razer.net.br/datasets/Volumes.csv",sep = ";")

# converter para numeric pois no createDataPartition da erro

apply(dados,2, class) # check columns type
dados <- data.frame(lapply(dados, function(x) {
  gsub(",", ".", x)
}))
dados <- data.frame(apply(dados, 2, function(x) as.numeric(as.character(x))))
dados <- dados[, -1]
apply(dados,2, class) # check columns type

# definindo novo data frame para os testes

df <- data.frame(dados)

set.seed(100)

# Separar 80% em treino e 20% em teste

indices <- createDataPartition(df$VOL, p=0.8, list=FALSE)
treino <- df[indices,]
teste <- df[-indices,]

# Random Forest

rf <- train(VOL~., data=treino, method="rf")
predicoes.rf <- predict(rf, teste)

SSE <- sum((teste$VOL - predicoes.rf)^2)
SST <- sum((teste$VOL - mean(treino$VOL))^2) 
RSQUARE <- 1 - SSE/SST
r.square.rf <- round(RSQUARE, 2)
r.square.rf # [1] 0.87

#SVM

svm <- train(VOL~., data=treino, method="svmRadial")
predicoes.svm <- predict(svm, teste)

SSE <- sum((teste$VOL - predicoes.svm)^2)
SST <- sum((teste$VOL - mean(treino$VOL))^2) 
RSQUARE <- 1 - SSE/SST
r.square.svm <- round(RSQUARE, 2)
r.square.svm #[1] 0.73

# RNA

rna <- train(VOL~., data=treino, method="nnet", trace=FALSE)
predicoes.rna <- predict(rna, teste)

SSE <- sum((teste$VOL - predicoes.rna)^2)
SST <- sum((teste$VOL - mean(treino$VOL))^2) 
RSQUARE <- 1 - SSE/SST
r.square.rna <- round(RSQUARE, 2)
r.square.rna # [1] -0.58

# ALOM

alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, treino, start=list(b0=0.5, b1=0.5))
predicoes.alom <- predict(alom, teste)

SSE <- sum((teste$VOL - predicoes.alom)^2)
SST <- sum((teste$VOL - mean(treino$VOL))^2) 
RSQUARE <- 1 - SSE/SST
r.square.alom <- round(RSQUARE, 2)
r.square.alom # [1] 0.88

# Em análise ao maior r square, o modelo escolhido foi: ALOM

# FINAL

indices <- createDataPartition(df$VOL, p=1, list=FALSE)
dados <- df[indices,]

alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, dados, start=list(b0=0.5, b1=0.5))
final.alom <- predict(alom, dados)

SSE <- sum((dados$VOL - final.alom)^2)
SST <- sum((dados$VOL - mean(dados$VOL))^2) 
RSQUARE <- 1 - SSE/SST
r.square.alom <- round(RSQUARE, 2)
r.square.alom

saveRDS(rf, "final.alom.rds")

