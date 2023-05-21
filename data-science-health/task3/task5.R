#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

setwd("C:/Users/vtrai/Desktop/Docs POS/Materias/11 - Ciencia de Dados aplicada a Saude/Task1")

load(file = "./tcgaLIHCdata_preprocessed.RData")
#class(tcgaLIHCdata)


library(SummarizedExperiment)
library(survival)
library(survminer)
library(shiny)

lihc_df <- colData(tcgaLIHCdata) 

gexp <- assay(tcgaLIHCdata)
rowAnnotation <- rowData(tcgaLIHCdata)
colAnnotation <- colData(tcgaLIHCdata)
rownames(gexp) <- rowAnnotation$SYMBOL

ui <- fluidPage(
  
  navbarPage(
    "Exercicio 5",
    
    tabPanel("Survival Analysis",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("stratify_survival", "Stratify by:",
                              c("Tumor Stage"="ts", "Gender"="gender")
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Kaplan-Meier Plot", 
                            plotOutput("plot_km")),
                   tabPanel("Cox Regression Forest Plot", 
                            plotOutput("plot_cox"))
                 )
               )
             )
    )
  )
)

server <- function(input, output){
  
  output$plot_km <- renderPlot({
    if (input$stratify_survival == "ts") {
      # Fit survival curves
      fit <- survfit(Surv(OS.time.months, OS) ~ Tumor_Stage, data = lihc_df) 
      
      
      # Customized survival curves
      p <- ggsurvplot(fit, data = lihc_df,
                      surv.median.line = "hv", # Add medians survival
                      
                      # Change legends: title & labels
                      legend.title = "Tumor Stage",
                      legend.labs = c("1", "2", "3", "4"),
                      xlab = "Months",
                      # Add p-value and tervals
                      pval = TRUE,
                      
                      conf.int = TRUE,
                      # Add risk table
                      risk.table = TRUE,
                      tables.height = 0.2,
                      tables.theme = theme_cleantable(),
                      
                      # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
                      # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
                      #palette = c("#E7B800", "#2E9FDF"),
                      ggtheme = theme_bw() # Change ggplot2 theme
      )
      
    }
    
    if (input$stratify_survival == "gender") {
      # Fit survival curves
      fit <- survfit(Surv(OS.time.months, OS) ~ gender, data = lihc_df) 
      
      # Customized survival curves
      p <- ggsurvplot(fit, data = lihc_df,
                      surv.median.line = "hv", # Add medians survival
                      
                      # Change legends: title & labels
                      legend.title = "Gender",
                      legend.labs = c("Female", "Male"),
                      xlab = "Months",
                      # Add p-value and intervals
                      pval = TRUE,
                      
                      conf.int = TRUE,
                      # Add risk table
                      risk.table = TRUE,
                      tables.height = 0.2,
                      tables.theme = theme_cleantable(),
                      
                      # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
                      # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
                      #palette = c("#E7B800", "#2E9FDF"),
                      ggtheme = theme_bw() # Change ggplot2 theme
      )
      
    }
    
    p
  })
  
  output$plot_cox <- renderPlot({
    if (input$stratify_survival == "ts") {
      fit <- coxph(Surv(OS.time.months, OS) ~ Stage, data = lihc_df)
      p <- ggforest(fit)
    }
    
    if (input$stratify_survival == "gender") {
      fit <- coxph(Surv(OS.time.months, OS) ~ gender, data = lihc_df)
      p <- ggforest(fit)
      
    }
    
    p
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
