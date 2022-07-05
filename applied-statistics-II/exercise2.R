#################### MODELO ETS ##########################

# E (Error) T (Trend) S (Season)
# Erro: aditivo (A) ou multiplicativo (M)
# Tendência: nenhuma (N), aditiva (A), multiplicativa (M) 
#            ou amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

##############################################################
# Previsão de Consumo Energia Elétrica na região sudeste (Gwh)
# Fonte: http://www.ipeadata.gov.br/Default.aspx
##############################################################

#### Instalando e carregando pacotes

pacotes <- c("readxl","plotly","tidyverse","gridExtra",
             "forecast","TTR","smooth","tidyverse", 
             "tsibble", "fable","tsibbledata", "fpp3",
             "urca")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

###### Carregando a base de dados de energia

#energia <- read_excel("energia.xlsx")
#load("./consumo_brasil.RData")
#energia <- consumo_brasil$Consumo
#View(energia)
library(readxl)
prodbebidas <- read_excel("C:/Users/vtrai/Desktop/Docs POS/Materias/4 - Estatistica Aplicada II/Lista 2/prodbebidas.xls")
View(prodbebidas)

prodbebidas <- prodbebidas$Prodbebidas

# Separa a coluna 2 que contém os dados de consumo de 
# energia

#energia=energia[2]

# Declara os dados como uma série temporal no período  
# de estudo

prodbebidas=ts(prodbebidas,start=c(2002,1), end=c(2022,4), 
           frequency=12)
prodbebidas

# Tamanho da amostra

length(prodbebidas)

# Separa a base de dados, um "pedaço" para treinar o
# modelo e outra para prever/testar

# Separação do dataset para treinar e testar o modelo

prodbebidastreino=window(prodbebidas,start=c(2002,1), 
                     end=c(2021,4))
plot(prodbebidastreino)
length(prodbebidastreino)

prodbebidasteste=window(prodbebidas,start=c(2021,5), 
                    end=c(2022,4))
plot(prodbebidasteste)
length(prodbebidasteste)

###Estimando o modelo ETS

prodbebidastreino.ets <- ets(prodbebidastreino)
summary(prodbebidastreino.ets)

# Fazendo previsão para os 12 períodos da base 
# de teste

prodbebidas.ets.forecasts <- forecast.ets(prodbebidastreino.ets, 
                                      h = 12)
summary(prodbebidas.ets.forecasts)

# Plotando o gráfico com as previsões

autoplot(prodbebidas.ets.forecasts)

ggplotly(
  autoplot(prodbebidastreino)+
    autolayer(prodbebidasteste,serie="Valores Reais")+
    autolayer(prodbebidas.ets.forecasts$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

# Estatísticas de acurácia na base de teste

accuracy(prodbebidas.ets.forecasts$mean,prodbebidasteste)

# Plotagem dos resíduos da estimativa

autoplot(prodbebidastreino.ets$residuals)

# Plotando a função de autocorrelação da estimativa

acf(prodbebidastreino.ets$residuals)

# Teste de validação do modelo

# Analisando os resíduos (erros) das previsões
# Condições:
# não podem ser correlacionados; se forem 
# correlacionados ficaram informações nos
# resíduos que deveriam estar no modelo,
# devem possui média zero, caso não possua
# então as previsões são viesadas

Box.test(prodbebidastreino.ets$residuals, lag=1,
         type=c("Ljung-Box"))

# Teste de Ljung-box
# H0: os resíduos são iid (modelo não exibe
#     falhas de ajustes)
# HA: os resíduos não são iid (modelo exibe
#     falhas de ajustes)

# Não desejamos rejeitar H0 (desejo um p-value>0.05)

library(seastests)

combined_test(prodbebidastreino)

# p-value é = 0 (ou seja, <0.5) então a série tem 
# sazonalidade, então podemos utilizar o SARIMA, caso
# contrário teriamos de usar o ARIMA


arimaprodbebidastreino=auto.arima(prodbebidastreino, trace=T)

checkresiduals(arimaprodbebidastreino)

ks.test(arimaprodbebidastreino$residuals, "pnorm", mean(arimaprodbebidastreino$residuals),
        sd(arimaprodbebidastreino$residuals))

ArchTest(arimaprodbebidastreino$residuals)

## Previsao para a série de ProdBebidas

prevprodbebidas=forecast::forecast(arimaprodbebidastreino, h=18)

# Observando os valores da previsão

prevprodbebidas

autoplot(prevprodbebidas) +
  theme_bw()

# Estatísticas de acurácia do modelo

accuracy(prevprodbebidas, prodbebidasteste)

# Gráfico do modelo ajustado com previsões

ggplotly(
  autoplot(prodbebidastreino)+
    autolayer(prodbebidasteste,serie="Valores Reais")+
    autolayer(prevprodbebidas$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)
