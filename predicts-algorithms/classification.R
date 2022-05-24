## Iris

summary(iris)
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
library("caret")
data(iris)
dataset <- iris
dataset
indices <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
treino <- dataset[indices,] # 80% pra treino
teste <- dataset[-indices,] # 20% pra teste
set.seed(7)
rf <- train(Species~., data=treino, method="rf") # treino com metodo RandomForest
predicoes.rf <- predict(rf, teste)
confusionMatrix(predicoes.rf, teste$Species) 
svm <- train(Species~., data=treino, method="svmRadial") # treino com metodo SVM
predicoes.svm <- predict(svm, teste)
confusionMatrix(predicoes.svm, teste$Species)
getwd()
save(rf, file="rf.RData") # salvar pra uso futuro
save(svm, file="svm.RData")
load("rf.RData")

## Cancer de Mama

install.packages("caret")
install.packages("e1071")
install.packages("mlbench")
install.packages("mice")
library(mlbench)
library(caret)
library(mice)

data(BreastCancer)
temp_dados <- BreastCancer
temp_dados$Id <- NULL
imp <- mice(temp_dados)
dados <- complete(imp, 1)
indices <- createDataPartition(dados$Class, p=0.80,
+ list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]
set.seed(7)
rf <- train(Class~., data=treino, method="rf")
svm <- train(Class~., data=treino, method="svmRadial")
rna <- train(Class~., data=treino, method="nnet", trace=FALSE)

predict.rf <- predict(rf, teste)
predict.svm <- predict(svm, teste)
predict.rna <- predict(rna, teste)

table(teste$Class)

   benign malignant 
       91        48 
confusionMatrix(predict.rf, teste$Class)
Confusion Matrix and Statistics

           Reference
Prediction  benign malignant
  benign        87         2
  malignant      4        46
                                         
               Accuracy : 0.9568         
                 95% CI : (0.9084, 0.984)
    No Information Rate : 0.6547         
    P-Value [Acc NIR] : <2e-16         
                                         
                  Kappa : 0.9055         
                                         
 Mcnemar's Test P-Value : 0.6831         
                                         
            Sensitivity : 0.9560         
            Specificity : 0.9583         
         Pos Pred Value : 0.9775         
         Neg Pred Value : 0.9200         
             Prevalence : 0.6547         
         Detection Rate : 0.6259         
   Detection Prevalence : 0.6403         
      Balanced Accuracy : 0.9572         
                                         
       'Positive' Class : benign         
                                         
confusionMatrix(predict.svm, teste$Class)
Confusion Matrix and Statistics

           Reference
Prediction  benign malignant
  benign        85         0
  malignant      6        48
                                         
               Accuracy : 0.9568         
                 95% CI : (0.9084, 0.984)
    No Information Rate : 0.6547         
    P-Value [Acc NIR] : < 2e-16        
                                         
                  Kappa : 0.9073         
                                         
 Mcnemar's Test P-Value : 0.04123        
                                         
            Sensitivity : 0.9341         
            Specificity : 1.0000         
         Pos Pred Value : 1.0000         
         Neg Pred Value : 0.8889         
             Prevalence : 0.6547         
         Detection Rate : 0.6115         
   Detection Prevalence : 0.6115         
      Balanced Accuracy : 0.9670         
                                         
       'Positive' Class : benign         
                                         
confusionMatrix(predict.rna, teste$Class)
Confusion Matrix and Statistics

           Reference
Prediction  benign malignant
  benign        89         2
  malignant      2        46
                                         
               Accuracy : 0.9712         
                 95% CI : (0.928, 0.9921)
    No Information Rate : 0.6547         
    P-Value [Acc NIR] : <2e-16         
                                         
                  Kappa : 0.9364         
                                         
 Mcnemar's Test P-Value : 1              
                                         
            Sensitivity : 0.9780         
            Specificity : 0.9583         
         Pos Pred Value : 0.9780         
         Neg Pred Value : 0.9583         
             Prevalence : 0.6547         
         Detection Rate : 0.6403         
   Detection Prevalence : 0.6547         
      Balanced Accuracy : 0.9682         
                                         
       'Positive' Class : benign 

• Comparação da Acurácia
• RF : 0,9568
• SVM : 0,9712
• RNA : 0,9568
• Assim, o modelo usando SVM tem melhor desempenho, por esta métrica

• Uma vez que o modelo foi treinado, testado e escolhido
• Treina-se um novo modelo com os parâmetros escolhidos
• Usa-se todos os dados de teste disponíveis
• Este é o modelo que será disponibilizado
• Então é feito o salvamento do modelo, para posterior uso
• saveRDS()
• readRDS()

A função train() do pacote caret usa:
• SMV: função ksvm() do pacote kernlab
• RF: função randomForest() do pacote randomForest
• RNA: pacote nnet, neuralnet, etc

## pra SVM

print(svm)
Support Vector Machines with Radial Basis Function Kernel 

560 samples
  9 predictor
  2 classes: 'benign', 'malignant' 

No pre-processing
Resampling: Bootstrapped (25 reps) 
Summary of sample sizes: 560, 560, 560, 560, 560, 560, ... 
Resampling results across tuning parameters:

  C     Accuracy   Kappa    
  0.25  0.9557653  0.9038441
  0.50  0.9631510  0.9194000
  1.00  0.9652492  0.9238511

Tuning parameter 'sigma' was held constant at a value of 0.01655509
Accuracy was used to select the optimal model using the largest value.
The final values used for the model were sigma = 0.01655509 and C = 1.

> final_model <- ksvm(type="C-svc", Class~., data=dados, kernel="rbfdot",
C=1.0, kpar=list(sigma=0.01655509))
> final_predict.svm <- predict(final_model, dados)
> confusionMatrix(final_predict.svm, dados$Class)
> saveRDS(final_model, "cancer_mama_svm.rds")
> meu_modelo <- readRDS("./cancer_mama_svm.rds")
> novas_predicoes <- predict(meu_modelo, teste)
> confusionMatrix(novas_predicoes, teste$Class)

## pra RNA
> print(rna)
Neural Network 

560 samples
  9 predictor
  2 classes: 'benign', 'malignant' 

No pre-processing
Resampling: Bootstrapped (25 reps) 
Summary of sample sizes: 560, 560, 560, 560, 560, 560, ... 
Resampling results across tuning parameters:

  size  decay  Accuracy   Kappa    
  1     0e+00  0.9506522  0.8881662
  1     1e-04  0.9512411  0.8897855
  1     1e-01  0.9463659  0.8783607
  3     0e+00  0.9500440  0.8863558
  3     1e-04  0.9462929  0.8786754
  3     1e-01  0.9523441  0.8918434
  5     0e+00  0.9400082  0.8640485
  5     1e-04  0.9480662  0.8818227
  5     1e-01  0.9525108  0.8921688

Accuracy was used to select the optimal model using the largest value.
The final values used for the model were size = 5 and decay = 0.1.

nnetGrid <- expand.grid(size = 5, decay = 0.1)
final_model_rna <- train(Class~., data=dados, method="nnet", trace=FALSE, tuneGrid = nnetGrid)
final_predict.rna <- predict(final_model_rna, dados)
confusionMatrix(final_predict.rna, dados$Class)

> confusionMatrix(final_predict.rna, dados$Class)
Confusion Matrix and Statistics

           Reference
Prediction  benign malignant
  benign       458         0
  malignant      0       241
                                     
               Accuracy : 1          
                 95% CI : (0.9947, 1)
    No Information Rate : 0.6552     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
                                     
 Mcnemar's Test P-Value : NA         
                                     
            Sensitivity : 1.0000     
            Specificity : 1.0000     
         Pos Pred Value : 1.0000     
         Neg Pred Value : 1.0000     
             Prevalence : 0.6552     
         Detection Rate : 0.6552     
   Detection Prevalence : 0.6552     
      Balanced Accuracy : 1.0000     
                                     
       'Positive' Class : benign    

saveRDS(final_model_rna, "cancer_mama_svm.rds")

> meu_modelo <- readRDS("./cancer_mama_svm.rds")
> novas_predicoes <- predict(meu_modelo, teste)
> confusionMatrix(novas_predicoes, teste$Class)