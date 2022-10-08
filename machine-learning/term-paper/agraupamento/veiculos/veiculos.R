# Agrupamento

## Veiculos

install.packages("klaR")
library(klaR)

dados <- read.csv("Material 07 - 4 - Veiculos - Dados.csv")
View(dados)

set.seed(0758662190)
cluster <- kmeans(dados, 4)
cluster

resultado <- cbind(dados, cluster$cluster)
head(resultado, 10)
