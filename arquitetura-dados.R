library("plyr")
library("readr")
library("ggplot2")
library("gridExtra")
library("grid")
library("tidyr")
library("repr")
library("glmnet")
library("caret")
library("dplyr")
library("corrplot")

# --- 

# Accelerometer

accelerometer <- read.csv("accelerometer.csv", header = TRUE)

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

set.seed(123)

indices <- createDataPartition(accelerometer$wconfid, p=0.80, list=FALSE)
treino <- accelerometer[indices,] # 80% pra treino
teste <- accelerometer[-indices,] # 20% pra teste

set.seed(123)
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

grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
             top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))

plot(varImp(rf))

#set.seed(123)
#svm <- train(wconfid ~ ., data=treino, method="svmRadial")
#predicoes.svm <- predict(svm, teste)
#confusionMatrix(predicoes.svm, teste$wconfid)
  
