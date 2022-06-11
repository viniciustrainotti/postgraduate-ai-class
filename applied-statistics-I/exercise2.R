# Lista de Exercicios 2 

load("./imoveiscwbav.RData")
gc()

# Modelo Preliminar

resultados <- lm(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
                   trans+kidca+school+health+bike+barb+balc+elev+
                   fitg+party+categ,data=imoveiscwbav)


summary (resultados)

# Especificação do Modelo

#install.packages("PanJen")

library("PanJen")

formBase<-formula(price~parea+tarea+bath+ensuit+garag+plaz+park+
                 trans+kidca+school+health+bike+barb+balc+elev+
                 fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenage<-fform(imoveiscwbav,"age",formBase)
# smoothing vencedor, incluir age

formBase<-formula(price~age+tarea+bath+ensuit+garag+plaz+park+
                    trans+kidca+school+health+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenparea<-fform(imoveiscwbav,"parea",formBase)
# smoothing vencedor, incluir parea

#install.packages("timetk")
#install.packages("hardhat")
library(timetk)

formBase<-formula(price~age+parea+bath+ensuit+garag+plaz+park+
                    trans+kidca+school+health+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJentarea<-fform(imoveiscwbav,"tarea",formBase)
# smoothing vecedor, incluir tarea


formBase<-formula(price~age+parea+tarea+bath+ensuit+garag+park+
                    trans+kidca+school+health+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenplaz<-fform(imoveiscwbav,"plaz",formBase)
# base vencedor, não incluir plaz

formBase<-formula(price~age+parea+tarea+bath+ensuit+garag+plaz+
                    trans+kidca+school+health+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenpark<-fform(imoveiscwbav,"park",formBase)
# smoothing vencedor, incluir park

formBase<-formula(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
                    kidca+school+health+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJentrans<-fform(imoveiscwbav,"trans",formBase)
# base vencedor, não incluir trans

formBase<-formula(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
                    trans+school+health+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenkidca<-fform(imoveiscwbav,"kidca",formBase)
# base vencedor, não incluir kidca

formBase<-formula(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
                    trans+kidca+health+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenschool<-fform(imoveiscwbav,"school",formBase)
# base vencedor, não incluir school

formBase<-formula(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
                    trans+kidca+school+bike+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenhealth<-fform(imoveiscwbav,"health",formBase)
# base vencedor, não incluir health

formBase<-formula(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
                    trans+kidca+school+health+barb+balc+elev+
                    fitg+party+categ)

summary(lm(formBase, data=imoveiscwbav))

PanJenbike<-fform(imoveiscwbav,"bike",formBase)
# base vencedor, não incluir bike

# smoothing, se necessario:

imoveiscwbav$age <- (imoveiscwbav$age^2)
imoveiscwbav$parea <- sqrt(imoveiscwbav$parea)
imoveiscwbav$tarea <- sqrt(imoveiscwbav$tarea)
imoveiscwbav$plaz <- log(imoveiscwbav$plaz)
imoveiscwbav$park <- 1/(imoveiscwbav$park)
imoveiscwbav$trans <- 1/(imoveiscwbav$trans)


save(imoveiscwbav, file="imoveiscwbav1.RData" )

# Valores Padrão

resultados <- lm(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
                   trans+kidca+school+health+bike+barb+balc+elev+
                   fitg+party+categ,data=imoveiscwbav)


summary (resultados)


# Outliers

library (carData)
library(car)

outlierTest(resultados)

# Teste Reset

#install.packages("zoo")

library (zoo)
library (lmtest)

resettest(price~age+parea+tarea+bath+ensuit+garag+plaz+park+
            trans+kidca+school+health+bike+barb+balc+elev+
            fitg+party+categ,power=2:3,type="regressor", data=imoveiscwbav)


# F tabelado:

qf(0.95, df1=38, df2=483)


## multicolinearidade 

cor(imoveiscwbav[,c("age", "parea", "tarea", "bath", "ensuit", "garag", "plaz", "park",
                      "trans", "kidca", "school", "health", "bike")], 
    use="complete")

library(car)

vif(lm(price~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+kidca+school+health+bike,data=imoveiscwbav), type="high-order")

# STEPWISE

resultados <- lm(price~age+parea+tarea+bath+ensuit+garag+plaz+park+trans+kidca+school+health+bike+barb+balc+elev+
                   fitg+party+categ,
                 data=imoveiscwbav)
summary (resultados)

library(RcmdrMisc)
step <- stepwise(resultados, direction= 'backward/forward', criterion ='AIC')
step

# Portanto o melhor modelo é:

resultados <- lm(price ~ age + parea + tarea + ensuit + garag + plaz + 
                   park + trans + bike + barb + balc + elev + fitg + categ, data = imoveiscwbav)
summary(resultados)

# HOMOCEDASTICIDADE

bptest(price ~ age + parea + tarea + ensuit + garag + plaz + 
         park + trans + bike + barb + balc + elev + fitg + categ,studentize=FALSE, data=imoveiscwbav)

# H0: homocedático (variâncias constantes); 
# HA: Heterocedastico (variâncias não constantes)

# O resultado do teste é:

#Breusch-Pagan test

#data:  price ~ age + parea + tarea + ensuit + garag + plaz + park +     trans + bike + barb + balc + elev + fitg + categ
#BP = 290, df = 14, p-value < 2.2e-16

# O valor chiquadrado tabelado é:

qchisq(0.95, df=14, lower.tail = TRUE)

# [1] 23.68479

imoveiscwbav$price <- log(imoveiscwbav$price)
imoveiscwbav$age <- log(imoveiscwbav$age)
imoveiscwbav$parea <- log(imoveiscwbav$parea)
imoveiscwbav$tarea <- log(imoveiscwbav$tarea)
imoveiscwbav$ensuit <- log(imoveiscwbav$ensuit)
imoveiscwbav$garag <- log(imoveiscwbav$garag)
imoveiscwbav$plaz <- log(imoveiscwbav$plaz)
imoveiscwbav$park <- log(imoveiscwbav$park)
imoveiscwbav$trans <- log(imoveiscwbav$trans)
imoveiscwbav$bike <- log(imoveiscwbav$bike)

bptest(price ~ age + parea + tarea + ensuit + garag + plaz + 
         park + trans + bike + barb + balc + elev + fitg + categ,studentize=FALSE, data=imoveiscwbav)

qchisq(0.95, df=14, lower.tail = TRUE)

#install.packages("robust")
library(robust)

resultrob <- lmRob(price ~ age + parea + tarea + ensuit + garag + plaz + 
                     park + trans + bike + barb + balc + elev + fitg + categ,data=imoveiscwbav)
summary(resultrob)

AIC(resultados)
BIC(resultados)

#install.packages("AICcmodavg")
library(AICcmodavg)

# AICc ? usado para pequenas amostras
AICc(resultados)

#install.packages("performace")
library(performance)

# Menores valores de AIC, BIC, Sigma e RMSE são desejáveis
# Maiores valores de R2 e R2(adj) são melhores
# Sigma é o Erro Padrão dos Resíduos
# RMSE é a raiz quadrada média dos erros (menor é melhor)

model_performance(resultados)
model_performance(resultrob)

# Calculando os intervalos de confiança - Para regressão linear "normal"

confint(resultados, level = 0.95)

# Calculando os intervalos de confiança - Para a regressão robusta

confint(resultrob, level=0.95)

################ Fazendo predições no modelos OLS 
#Para:
#age = 5 anos = 1.609438
log(5)
#educ = 150 parea = 5.010635
log(150)
#earns = 190 tarea = 5.247024
log(250)
# plaz
log(0.08)
#kidlt6 = 1 --> tem filhos com menos de 6 anos

#kidge6 =0 --> parâmetro não significativo a 95%
#union = 0  --> parâmetro não significativo a 95%
#husearns = 0 --> parâmetro não significativo a 95%
#hushrs = 0 --> parâmetro não significativo a 95%

predict(object = resultrob,
        data.frame(age=1.60, parea=5.01, tarea=5.24, ensuit = 1, garag = 1, plaz = -2.525729, 
                   park = -2.525729, trans = -2.525729, bike = -2.525729, categ = 1,
                   barb=0, balc=0, elev=0, fitg=0))

#o salario por hora é:
exp(14.11198)

anova.lmRob(resultrob)
anova(resultados)