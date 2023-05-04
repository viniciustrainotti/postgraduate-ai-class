# Visualizando dados do Global Cancer Observatory (GCO)

install.packages("TreeAndLeaf") # Pacote Bioconductor
install.packages("RedeR") # Pacote Bioconductor
install.packages("RColorBrewer")  # Pacote CRAN
install.packages("igraph")  # Pacote CRAN
install.packages("SummarizedExperiment") # Pacote Bioconductor
install.packages("ComplexHeatmap") # Pacote Bioconductor
install.packages("circlize") # Pacote CRAN
install.packages("RColorBrewer")  # Pacote CRAN
install.packages("survival")  # Pacote CRAN
install.packages("survminer")  # Pacote CRAN
install.packages("shiny")  # Pacote CRAN

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("TreeAndLeaf", "RedeR", "SummarizedExperiment", "ComplexHeatmap"))

# Carregamento dos pacotes em R:
library(TreeAndLeaf) # Pacote Bioconductor
library(RedeR) # Pacote Bioconductor
library(RColorBrewer)  # Pacote CRAN
library(igraph)  # Pacote CRAN
library(SummarizedExperiment) # Pacote Bioconductor
library(ComplexHeatmap) # Pacote Bioconductor
library(circlize) # Pacote CRAN
library(RColorBrewer)  # Pacote CRAN
library(survival)  # Pacote CRAN
library(survminer)  # Pacote CRAN
library(shiny)  # Pacote CRAN

install.packages("ggplot2")
library(ggplot2)

dataset.df <- read.csv(file="./dataset-absolute-numbers-inc-both-sexes-in-2040-breast.csv")

year <- dataset.df$Year[(dataset.df$Year == 2020 | dataset.df$Year == 2040)]
breast_cancer.df <- data.frame("pop"= dataset.df$Population[(dataset.df$Year == 2020 | dataset.df$Year == 2040)], 
                                "year"=dataset.df$Year[(dataset.df$Year == 2020 | dataset.df$Year == 2040)],
                                "n_cases"=dataset.df$Prediction[(dataset.df$Year == 2020 | dataset.df$Year == 2040)])

breast_cancer.df$year[breast_cancer.df$year == 2020] = "2020"
breast_cancer.df$year[breast_cancer.df$year == 2040] = "2040"

breast_cancer.df


#ggplot(breast_cancer.df, aes(fill=breast_cancer.df$year, y=breast_cancer.df$n_cases, x=breast_cancer.df$pop)) + 
#  geom_bar(position="dodge", stat="identity")


# Criar objeto ggplot
grafico <- ggplot(data = breast_cancer.df, aes(x = breast_cancer.df$pop,
                                              y = breast_cancer.df$n_cases,
                                              fill = breast_cancer.df$year))

# Adicionar camada de barras
grafico <- grafico + geom_bar(stat = "identity", position = "dodge")

# Adicionar rótulos aos eixos e título
grafico <- grafico + xlab("População") + ylab("Número de casos") + ggtitle("Casos por população e ano")

# Personalizar a aparência do gráfico
grafico <- grafico + theme(panel.background = element_rect(fill = "white"),
                           panel.grid.major = element_line(colour = "gray"),
                           panel.grid.minor = element_blank(),
                           axis.line = element_line(colour = "black"),
                           legend.position = "top")

# Mostrar gráfico
grafico
