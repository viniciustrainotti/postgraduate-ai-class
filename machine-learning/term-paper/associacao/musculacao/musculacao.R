## Associação

# Musculação

install.packages("arules", dep=T)
library("arules")
install.packages("datasets")
library("datasets")

dados <- read.transactions(file="Material 08 – 2 - Musculacao - Dados.csv", format="basket", sep=",")
inspect(dados)
View(dados)
set.seed(0758662190)

rules_1 <- apriori(dados, parameter = list(supp = 0.001, conf = 0.1, minlen=1))
inspect(rules_1)

rules_2 <- apriori(dados, parameter = list(supp = 0.1, conf = 0.8, minlen=6))
inspect(rules_2)
#[0 rule(s)] done [0.00s].
rules_3 <- apriori(dados, parameter = list(supp = 0.4, conf = 0.9, target = "rules"))
inspect(rules_3)
#[0 rule(s)] done [0.00s].