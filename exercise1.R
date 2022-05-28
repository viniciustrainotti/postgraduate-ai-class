library(carData)
library(datasets)
library(BSDA)
library(stats)
library(ggpubr)
library(RcmdrMisc)

load("./realestateiaa.RData")

price = realestateiaa$price
tarea = realestateiaa$tarea
parea = realestateiaa$parea

# ==:: Exercicio A)
# Elaborar o histograma e o boxplot das variáveis “parea e tarea”.

## HISTOGRAMA

X11(width = 10, height = 12)
with(realestateiaa, Hist(parea, scale="frequency", breaks="sturges", 
                    col="darkgray"))

X11(width = 10, height = 12)
with(realestateiaa, Hist(tarea, scale="frequency", breaks="sturges", 
                         col="darkgray"))


## BOXPLOT

X11(width = 10, height = 12)
Boxplot( ~ parea, data=realestateiaa, id=list(method="y"))

X11(width = 10, height = 12)
Boxplot( ~ tarea, data=realestateiaa, id=list(method="y"))


# ==:: Exercicio B) 
# Elaborar a tabela de distribuição de frequências da variável “price”
# (preço dos imóveis);



# ==:: Exercicio - C)
# Para a variável “price” calcular os seguintes indicadores: média; mediana; 
# moda; variância; desvio padrão; CV–Coeficiente de Variação; Quartis; 
# distância interquartílica; percentis.

mean(price) # media price
median(price) # median price
subset(table(price), 
       table(price) == max(table(price))) # moda

var(price) # varianca
sd(price) # desvio padrao

meanPrice <- mean(price)
sdPrice <- sd (price)
cvM <- (sdPrice/meanPrice)*100 # Coeficiente de Variação
cvM

Q1 <- quantile(price, probs = 0.25)
Q1

# Q2 é igual a mediana
Q2 <- quantile(price, probs = 0.50)
Q2

Q3 <- quantile(price, probs = 0.75)
Q3

IQR(price) # distancia interquatilica ou Q3 - Q1 

quantile(price, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) # percentil

# ==:: Exercicio - D)
# Estimar o intervalo de confiança para a média da variável “price” com 95% 
# de confiança

sdPrice = sd(price)
sdPrice

# Usando o Z
options(scipen = 999) # transforma notação cientifica em numerico 
z.test(price, y = NULL, alternative = "two.sided", mu = 0, sigma.x = sdPrice,
       sigma.y = NULL, conf.level = 0.95)

# ou pode usar o Student
t.test(price, y = NULL, alternative = "two.sided", mu = 0, sigma.x = sdPrice,
       sigma.y = NULL, conf.level = 0.95)

# ==:: Exercicio - E)
# Fazer o teste de diferença entre médias para as variáveis “parea” e 
# “tarea”.

sdParea = sd(parea)
sdParea

sdTarea = sd(tarea)
sdTarea

mean(parea)
mean(tarea)

z.test(parea, tarea, alternative = "two.sided", mu = 0, sigma.x = sdParea,
       sigma.y = sdTarea, conf.level = 0.95)

# ==:: Exercicio - F)
# Fazer o teste de diferença entre variâncias para as variáveis “parea” e 
# “tarea”.

var.test(parea, tarea, alternative = "two.sided", conf.level = 0.95)

# ==:: Exercicio - G)
# Fazer o Teste de Wilcoxon-Mann-Whitney para amostras independentes
# para as variáveis “parea” e “tarea”.

options(scipen = 999)
wilcox.test(parea, tarea, alternative = "two.sided") 

# ==:: Exercicio - H)
# Fazer 2 testes de normalidade (a sua escolha) para a variável “price”

########### Teste de normalidade de Kolmogorov-Smirnov

library(RcmdrMisc)

normalityTest(~price, test="lillie.test", data=realestateiaa)

###### Teste de normalidade de Shapiro-Wilk

shapiro.test(price)
