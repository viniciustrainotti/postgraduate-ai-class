# Agrupamento

## Cancer de Mama

install.packages("klaR")
library(klaR)

dados <- read.csv("Material 02 - 2 - Cancer de Mama - Dados.csv")
dados$Class <- NULL
View(dados)

set.seed(0758662190)
cluster <- kmeans(dados, 10, 2)
cluster

resultado <- cbind(dados, cluster$cluster)
head(resultado, 10)
