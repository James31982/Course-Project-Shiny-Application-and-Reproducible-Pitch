library(shiny)
library(shiny)
library(rpart)
library(caTools)
library(caret)
library(dplyr)
library(lubridate)
library(rpart.plot)
library(e1071)


loan_data <- read.csv("https://raw.githubusercontent.com/James31982/test-repo/master/Loan%20payments%20data.csv", header = TRUE)

loan_data$paid_off_time <-  mdy_hm(loan_data$paid_off_time)
loan_data$paid_off <-  ifelse(is.na(loan_data$paid_off_time),0,1)
loan_data$past_due_days<- ifelse(is.na(loan_data$past_due_days),0,loan_data$past_due_days)

vars1 <- c("loan_status","past_due_days")
vars2 <- c("loan_status","paid_off", "past_due_days")

shinyServer(function(input, output) {
        
        
        set.seed(123)
        split = sample.split(loan_data$loan_status, SplitRatio = 0.75)
        
        training_set = subset(loan_data, split == TRUE)
        test_set = subset(loan_data, split == FALSE)
        
        model1pred <- reactive({
                
                partitionInput1 <- input$sliderPartition
                
                set.seed(123)
                split = sample.split(loan_data$loan_status, SplitRatio = partitionInput1)
                
                
        })
        
        
        
        model2pred <- reactive({
                
                partitionInput2 <- input$sliderPartition
                
                set.seed(123)
                split = sample.split(loan_data$loan_status, SplitRatio = partitionInput2)
                
                
        })
        
        
        
        output$plot1 <- renderPlot({
                
                partitionInput <- input$sliderPartition
                set.seed(123)
                split = sample.split(loan_data$loan_status, SplitRatio = partitionInput)
                
                
             
                if( input$radio == "showModel1"){
                        
                       
                        training_set = subset(loan_data, split == TRUE)
                        test_set = subset(loan_data, split == FALSE)
                        
                        train_tree1<- training_set[vars1]
                        test_tree1<- test_set[vars1]
                        
                        rpart_tree1 <- rpart(loan_status ~., data = train_tree1)
                        rpart.plot(rpart_tree1)
                        
                }
                
                if(input$radio == "showModel2"){
                        
                        
                        training_set = subset(loan_data, split == TRUE)
                        test_set = subset(loan_data, split == FALSE)
                        
                        train_tree2<- training_set[vars2]
                        test_tree2<- test_set[vars2]
                        rpart_tree2 <- rpart(loan_status ~., data = train_tree2)
                        rpart.plot(rpart_tree2)
                             
                }
                
              
        })
        
        
        
        output$pred1 <- renderText({
                
                partitionInput1 <- input$sliderPartition
                set.seed(123)
                split = sample.split(loan_data$loan_status, SplitRatio = partitionInput1)
                
                
                training_set = subset(loan_data, split == TRUE)
                test_set = subset(loan_data, split == FALSE)
                
                train_tree1<- training_set[vars1]
                test_tree1<- test_set[vars1]
                
                rpart_tree1 <- rpart(loan_status ~., data = train_tree1)
                
                
                rpart_tree_pred1<- train(loan_status ~., method = 'rpart', data = train_tree1)
                pred1<- predict(rpart_tree_pred1,test_tree1)
                confusionMatrix(pred1, test_tree1$loan_status)$overall[1]*100
                
        })
        
        
        
        output$pred2 <- renderText({
                
                partitionInput2 <- input$sliderPartition
                set.seed(123)
                split = sample.split(loan_data$loan_status, SplitRatio = partitionInput2)
                
                
                training_set = subset(loan_data, split == TRUE)
                test_set = subset(loan_data, split == FALSE)
                
                train_tree2<- training_set[vars2]
                test_tree2<- test_set[vars2]
                rpart_tree2 <- rpart(loan_status ~., data = train_tree2)
                
                rpart_tree_pred2<- train(loan_status ~., method = 'rpart', data = train_tree2)
                pred2<- predict(rpart_tree_pred2,test_tree2)
                confusionMatrix(pred2, test_tree2$loan_status)$overall[1]*100
                
        })
        
})