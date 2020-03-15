#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# copy the ALDH SNP list from the S3 folder (terminal window command)
load("/home/ubuntu/COVID/COVID.Rdata")


library(tidyverse)
#Sys.setlocale(category = "LC_ALL", locale = "Chinese")
library(readxl)
library(ggplot2)
library(cluster)
library(table1)

library(ggpubr)

library(factoextra)
library(cluster)
require(ggrepel)
require(ggfortify)
library(shiny)

load("/home/ubuntu/COVID/COVID.Rdata")



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 Patient Condition Monitor: Input Patient Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3,
            numericInput("Age",
                         "Age (1~100)",60,
                         min = 1,
                         max = 100),
            
            selectInput("UnderlyingDisease",
                        "Underlying Disease", c("yes"=1, "no"=0)),
            
            numericInput("TotalTCell",
                         "Total T Cell Counts (Allowed range: 20~5000)",780,
                         min = 20,
                         max = 5000),
            
            
            numericInput("HelperTCell",
                         "Helper T Cell Counts (Allowed range: 10~2500)",460,
                         min = 10,
                         max = 2500),
            
            numericInput("SuppressorTCell",
                         "Suppressor T Cell Counts (Allowed range: 10~1500)",300,
                         min = 10,
                         max = 1500),
            
            numericInput("TH_TS",
                         "TH/TS (Allowed range: 0.1~10)",1.7,
                         min = 0.1,
                         max = 10)
        ),
        mainPanel(
            width = 7,
            plotOutput("distPlot")
        )
    )
    
    
    # Show a plot of the generated distribution
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
#        if(input$Age > 100) input$Age = 100
#        if(input$Age < 1) input$Age =1
#        if(input$TotalTcell > 5000) input$TotalTcell = 5000
#        if(input$TotalTcell < 20) input$TotalTcell = 20
#        if(input$HelperTcell > 2500) input$HelperTcell = 2500
#        if(input$HelperTcell < 10) input$HelperTcell = 10
#        if(input$SuppressorTcell > 1500) input$SuppressorTcell = 1500
#        if(input$SuppressorTcell < 10) input$SuppressorTcell = 10
#        if(input$TH_TS > 10) input$TH_TS = 10
#        if(input$TH_TS < 0.1) input$TH_TS = 0.1
        
        df=as.data.frame(scale(rbind(df1[,1:6],
                                     c(input$Age, as.numeric(input$UnderlyingDisease), input$TotalTCell, input$HelperTCell, input$SuppressorTCell, input$TH_TS))))
        
        group = c(df1$Outcome, "New Patient")
        
        k2 <- kmeans(df, centers = 2, nstart = 25)
        
        
        fviz_cluster(list(data = df, cluster=group), geom="point", pointsize = c(rep(1,340),10))+ 
            scale_fill_manual(values=c("lightcoral", "mediumseagreen","black")) + 
            theme_minimal()+xlab("")+ylab("")+ggtitle("")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
