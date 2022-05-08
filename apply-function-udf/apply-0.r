# Média em cada coluna

a <- matrix(1:100000,,10)
head(a)
head(apply(a,2,mean))

# Max em cada linha

a <- matrix(1:100000,,10)
head(a)
head(apply(a,1,max))
