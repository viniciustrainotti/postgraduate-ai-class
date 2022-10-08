# Agrupamento

## Moveis

install.packages("klaR")
library(klaR)

dados <- read.csv("Material 07 - 2 - Moveis - Dados.csv")
View(dados)

set.seed(0758662190)
moveisCluster <- kmodes(dados, 10, iter.max = 10, weighted = FALSE)
moveisCluster

resultado <- cbind(dados, moveisCluster$cluster)
head(resultado, 10)
