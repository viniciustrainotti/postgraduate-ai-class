## Associação

# Lista de Compras

install.packages("arules")
library("arules")
install.packages("datasets")
library("datasets")

dados <- read.csv("Material 08 – 1 - Lista de Compras - Dados.csv", sep=";")
View(dados)

set.seed(0758662190)

rules_1 <- apriori(dados, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
inspect(rules_1)

rules_2 <- apriori(dados, parameter = list(supp = 0.5, conf = 0.75, target = "rules"))
inspect(rules_2)

rules_3 <- apriori(dados, parameter = list(supp = 0.4, conf = 0.9, target = "rules"))
inspect(rules_3)
