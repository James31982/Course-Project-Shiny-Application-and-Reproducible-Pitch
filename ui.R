library(shiny)
library(shiny)
library(rpart)
library(caTools)
library(caret)
library(dplyr)
library(lubridate)

shinyUI(fluidPage(
        
        titlePanel("Decision Tree Modeling: Loan Payments Data"),
        
        sidebarLayout(
                
                sidebarPanel(
                        
                        sliderInput("sliderPartition", "Select Data Partition(%)", 0, 1, value = 0.5),
                        
                        radioButtons("radio", label = h3("Select Model"),
                                     choices = list("Model 1" = "showModel1", "Model 2" = "showModel2", "Clear" = "Clear"), 
                                     selected = "Clear"),
                        
                        hr(),
                        fluidRow(column(3, verbatimTextOutput("value"))),
                        
                        submitButton("Submit")
                        
                ),
                
                mainPanel(
                        
                        plotOutput("plot1"),  
                        
                        h3("Accuracy of Model 1:"),
                        
                        textOutput("pred1"),
                        
                        h3("Accuracy of Model 2:"),
                        
                        textOutput("pred2")
                        
                )
                
        )
        
))