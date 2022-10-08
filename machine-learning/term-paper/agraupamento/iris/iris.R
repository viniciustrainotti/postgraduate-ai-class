# Agrupamento

## Iris

install.packages("klaR")
library(klaR)

data(iris)
dados <- iris
View(dados)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()


set.seed(0758662190)
irisCluster <- kmeans(iris[, 3:4], 3)
irisCluster

table(irisCluster$cluster, iris$Species)

resultado <- cbind(dados, irisCluster$cluster)
head(resultado, 10)
