# Agrupamento

## Banco

install.packages("klaR")
library(klaR)

dados <- read.csv("Material 07 - 5 – Banco - Dados.csv")
dados$Class <- NULL
View(dados)

set.seed(0758662190)
cluster <- kmodes(dados, 5, iter.max=10, weighted=FALSE)
cluster

resultado <- cbind(dados, cluster$cluster)
head(resultado, 10)
