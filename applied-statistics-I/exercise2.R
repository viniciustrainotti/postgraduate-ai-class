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

imoveiscwbav$age <- smooth_vec(imoveiscwbav$age)
imoveiscwbav$parea <- smooth_vec(imoveiscwbav$parea)
imoveiscwbav$tarea <- smooth_vec(imoveiscwbav$tarea)
imoveiscwbav$park <- smooth_vec(imoveiscwbav$park)


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


imoveiscwbav$lnprice <- with(imoveiscwbav, log(price))
imoveiscwbav$lnage <- with(imoveiscwbav, log(age))
imoveiscwbav$lnparea <- with(imoveiscwbav, log(parea))
imoveiscwbav$lntarea <- with(imoveiscwbav, log(tarea))
imoveiscwbav$lnbath <- with(imoveiscwbav, log(bath))
imoveiscwbav$lnensuit <- with(imoveiscwbav, log(ensuit))
imoveiscwbav$lngarag <- with(imoveiscwbav, log(garag))
imoveiscwbav$lnplaz <- with(imoveiscwbav, log(plaz))
imoveiscwbav$lnpark <- with(imoveiscwbav, log(park))
imoveiscwbav$lntrans <- with(imoveiscwbav, log(trans))
imoveiscwbav$lnkidca <- with(imoveiscwbav, log(kidca))
imoveiscwbav$lnschool <- with(imoveiscwbav, log(school))
imoveiscwbav$lnhealth <- with(imoveiscwbav, log(health))
imoveiscwbav$lnbike <- with(imoveiscwbav, log(bike))

resettest(lnprice~lnage+lnparea+lntarea+bath+ensuit+garag+lnplaz+lnpark+
            lntrans+lnkidca+lnschool+lnhealth+lnbike+barb+balc+elev+
            fitg+party+categ,power=2:3,type="regressor", data=imoveiscwbav)


qf(0.95, df1=38, df2=483)


