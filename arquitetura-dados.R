## Iris

#install.packages("e1071")
#install.packages("randomForest")
#install.packages("kernlab")
install.packages("caret")

library(plyr)
library(readr)

library(ggplot2)
library(gridExtra)   # to put more
library(grid)        # plot together
#install.packages("grid")
#install.packages("tidyr")
library("tidyr")
library(repr)
library(glmnet)
library("caret")
#install.packages("dplyr")
library(dplyr)
#install.packages("corrplot")
library("corrplot")

#View(iris)

#data("wine.csv")

wine <- read.csv("wine.csv", header = FALSE)
colnames(wine) <- c('Classe_Identificacao_Cultivador', 'Alcool', 'Acido_Malico', 'Cinzas',
                    'Alcalinidade_das_cinzas', 'Magnesio', 'Fenois_totais', 'Flavonoides',
                    'Fenois_nao_flavonoides', 'Proantocianinas', 'Intensidade_cor',
                    'Matiz', 'OD280_OD315_diluicao', 'Prolina')
summary(wine)
View(wine)
str(wine)

wine$Classe_Identificacao_Cultivador <- as.factor(wine$Classe_Identificacao_Cultivador)

wine %>% gather(2:14, key = "variables", value = "result") %>%
  ggplot(aes(Classe_Identificacao_Cultivador, result, fill = Classe_Identificacao_Cultivador)) +
  geom_jitter(color = "grey", alpha = 0.5)+
  geom_boxplot()+
  theme_classic()+
  facet_wrap(.~variables, scale = "free")

wine %>% gather(2:14, key = "variables", value = "result") %>%
  ggplot(aes(result)) +
  geom_histogram(aes(fill = variables), color = "white")+
  theme_classic()+
  facet_wrap(.~variables, scale = "free") +
  theme(legend.position = "none")

wine %>% gather(2:14, key = "variables", value = "result") %>%
  ggplot(aes(result, fill = Classe_Identificacao_Cultivador)) +
  geom_density(alpha = 0.5)+
  theme_classic()+
  facet_wrap(.~variables, scale = "free")

corrplot(cor(wine[2:14]), 
         method = "color",
         type = "upper",
         addCoef.col = "white",number.cex = 0.7,
         tl.col="black", tl.srt=35,tl.cex = 0.7,
         cl.cex = 0.7,order = "hclust",
         title = "Correlação de variáveis")

# Flavonoides tem alta correlação com outras variaveis, se for necessário, remover ela depois

set.seed(123)

indices <- createDataPartition(wine$Classe_Identificacao_Cultivador, p=0.80, list=FALSE)
treino <- wine[indices,] # 80% pra treino
teste <- wine[-indices,] # 20% pra teste

set.seed(123)
# treino com metodo RandomForest
rf <- train(Classe_Identificacao_Cultivador ~ ., data=treino, method="rf")
#warning(rf)
predicoes.rf <- predict(rf, teste)
View(predicoes.rf)

saveRDS(rf, "final.rf.wine.RDS")

cmat <- confusionMatrix(predicoes.rf, teste$Classe_Identificacao_Cultivador)
cmat$table

cm_d <- as.data.frame(cmat$table)

cm_st <-data.frame(cmat$overall)

cm_st$cmat.overall <- round(cm_st$cmat.overall,2)

cm_p <- as.data.frame(prop.table(cmat$table))
cm_d$Perc <- round(cm_p$Freq*100,2)


cm_d_p <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq,",",Perc,"%")), color = 'red', size = 8) +
  theme_light() +
  guides(fill="none")

cm_st_p <-  tableGrob(cm_st)

# all together
grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))

# qual é a variavel mais importante

plot(varImp(rf))

#Results show that flavonoid, color intensity, proline are 3 most important variables impact to the quality of wine.

set.seed(7)
# treino com metodo SVM
svm <- train(Species ~ ., data=treino, method="svmRadial")
predicoes.svm <- predict(svm, teste)
confusionMatrix(predicoes.svm, teste$Species)


# --- 

# Accelerometer

accelerometer <- read.csv("accelerometer.csv", header = TRUE)
#colnames(wine) <- c('Classe_Identificacao_Cultivador', 'Alcool', 'Acido_Malico', 'Cinzas',
#                    'Alcalinidade_das_cinzas', 'Magnesio', 'Fenois_totais', 'Flavonoides',
#                    'Fenois_nao_flavonoides', 'Proantocianinas', 'Intensidade_cor',
#                    'Matiz', 'OD280_OD315_diluicao', 'Prolina')

summary(accelerometer)
View(accelerometer)
str(accelerometer)

accelerometer$wconfid <- as.factor(accelerometer$wconfid)



accelerometer %>% gather(2:5, key = "variables", value = "result") %>%
  ggplot(aes(wconfid, result, fill = wconfid)) +
  geom_jitter(color = "grey", alpha = 0.5)+
  geom_boxplot()+
  theme_classic()+
  facet_wrap(.~variables, scale = "free")

accelerometer %>% gather(2:5, key = "variables", value = "result") %>%
  ggplot(aes(result)) +
  geom_histogram(aes(fill = variables), color = "white")+
  theme_classic()+
  facet_wrap(.~variables, scale = "free") +
  theme(legend.position = "none")

accelerometer %>% gather(2:5, key = "variables", value = "result") %>%
  ggplot(aes(result, fill = wconfid)) +
  geom_density(alpha = 0.5)+
  theme_classic()+
  facet_wrap(.~variables, scale = "free")

corrplot(cor(accelerometer[2:5]), 
         method = "color",
         type = "upper",
         addCoef.col = "white",number.cex = 0.7,
         tl.col="black", tl.srt=35,tl.cex = 0.7,
         cl.cex = 0.7,order = "hclust",
         title = "Correlação de variáveis")

# parece que as variaveis não tem alta correlação então temos que usar todas

set.seed(123)

indices <- createDataPartition(accelerometer$wconfid, p=0.80, list=FALSE)
treino <- accelerometer[indices,] # 80% pra treino
teste <- accelerometer[-indices,] # 20% pra teste

set.seed(123)
# treino com metodo RandomForest
rf <- train(wconfid ~ ., data=treino, method="rf")
#warning(rf)
predicoes.rf <- predict(rf, teste)
View(predicoes.rf)
confusionMatrix(predicoes.rf, teste$wconfid)

saveRDS(rf, "final.rf.accelerometer.wconfid.RDS")

cmat <- confusionMatrix(predicoes.rf, teste$wconfid)
cmat$table

cm_d <- as.data.frame(cmat$table)

cm_st <-data.frame(cmat$overall)

cm_st$cmat.overall <- round(cm_st$cmat.overall,2)

cm_p <- as.data.frame(prop.table(cmat$table))
cm_d$Perc <- round(cm_p$Freq*100,2)


cm_d_p <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
  geom_tile() +
  geom_text(aes(label = paste("",Freq,",",Perc,"%")), color = 'red', size = 8) +
  theme_light() +
  guides(fill="none")

cm_st_p <-  tableGrob(cm_st)

# all together
grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))

# qual é a variavel mais importante

plot(varImp(rf))
  
  
