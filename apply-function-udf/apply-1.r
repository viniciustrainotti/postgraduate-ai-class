#Dados
 
idade <- c(56, 34, 67, 33, 25, 28)
peso <- c(78, 67, 56, 44, 56, 89)
altura <- c(165, 171, 167, 167, 166, 181)
dados <- data.frame(idade, peso, altura)
 
# média de todas as colunas (usando apply
apply(dados, 2, mean)
 
# O valor máximo de todas as colunas (usando apply)
apply(dados, 2, max)
 
# A raiz quadrada de todos os valores do data frame, como uma matriz
b <- as.matrix(apply(dados,2,sqrt), 3, 6)
 
# A raiz quadrada de todos os valores do data frame, como uma lista
b <- as.list(apply(dados,2,sqrt))
 
# Todos os valores do data frame multiplicados por 20, como uma matriz (usando uma UDF)
> multiplica20 <- function(x) {
+ return (x*20)
+ }
> b <- as.matrix(apply(dados,2,multiplica20), 3, 6)
