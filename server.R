library(shiny)
library(ggplot2)
library(gmodels)
library(pROC)
library(C50)
library(randomForest)
library(DT)
library(e1071)
library(fpc)
library(caret)

set.seed(123)

shinyServer(function(input, output){
  
  #Code for fie upload
  data <- reactive({
    file <- input$file
    if(is.null(file)){
      return()
    }
    read.csv(file = file$datapath, header = T)
  })
  
  #Code for converting data to factor form for categorical features
  data_new <- reactive({
    data1 = data()
    datafactor = c(3,4,5,7:12,25)
    data1[,datafactor] = lapply(data1[,datafactor],factor)
    data1
  })
  
  #Display number of rows
  output$nrow <- renderText({
    paste("Number of rows in the dataset : ", nrow(data_new()))
  })
  
  #Display number of columns
  output$ncol <- renderText({
    paste("Number of features in the dataset : ", ncol(data_new()))
  })
  
  #Display summary of dataset
  output$structure <- renderPrint({
    str(data_new())
  })
  
  #Plot for Gender Vs Defaulter
  output$plotGenderVsDefaulter <- renderPlot({
    ggplot(data_new(),aes(default.payment.next.month,fill=SEX))+geom_bar(position="dodge")+ggtitle("Comparison of Sex Vs Defaulters")+xlab("Default - Yes / No")+ylab("Count of Persons")
  })
  
  #Plot for Education Vs Defaulter
  output$plotEducationVsDefaulter <- renderPlot({
    ggplot(data_new(),aes(default.payment.next.month,fill=EDUCATION))+geom_bar(position="stack")+ggtitle("Comparison of Education Vs Defaulters")+xlab("Default - Yes / No")+ylab("Count of Persons")
  })
  
  #Plot for Marriage Vs Defaulter
  output$plotMarriageVsDefaulter <- renderPlot({
    ggplot(data_new(),aes(default.payment.next.month,fill=MARRIAGE))+geom_bar(position="dodge")+ggtitle("Comparison of Marriage Vs Defaulters")+xlab("Default - Yes / No")+ylab("Count of Persons")
  })
  
  #Histogram to check number of client based on AGE
  output$ageHistogram <- renderPlot({
    hist(data_new()$AGE, col="lightblue", freq=2, main="Histogram & distrubution of client's age", breaks=10, xlab="Age of the client", ylab="Count of Persons")
  })
  
  #Plot for Age Vs Defaulter
  output$plotAgeVsDefaulter <- renderPlot({
    ggplot(data_new(),aes(AGE,fill=default.payment.next.month))+geom_bar()+ggtitle("Comparison of Age Vs Defaulters")+xlab("Age")+ylab("Count of Persons")
  })
  
  #Plot for Age Vs Education Vs Defaulter
  output$plotAgeVsEducationVsDefaulter <- renderPlot({
    ggplot(data_new(),aes(AGE,fill=default.payment.next.month))+geom_histogram(binwidth=6)+facet_grid(.~EDUCATION)+ggtitle("Comparison of Age Vs Education Vs Defaulters")+xlab("Age")+ylab("Count of Persons")
  })
  
  #Plot for PAY_0 vs Defaulter
  output$plotPayVsDefaulter <- renderPlot({
    ggplot(data_new(),aes(PAY_0,fill=default.payment.next.month))+geom_bar()+theme_light()+ggtitle("Comparison of Pay Vs Defaulters")+xlab("Pay")+ylab("Count of Persons")
  })
  
  #For display the data table
  output$dataTable <- DT::renderDataTable({
    DT::datatable(data(), options = list(pageLength = 25))
  })
  
  #######################################################################################################
  
  #Split the dataset into training and test data
  
  trainingSetIndex <- reactive({
    sample(seq_len(nrow(data())), size = floor(0.70 * nrow(data())))
  })
  
  trainingData <- reactive({
    data()[trainingSetIndex(),]
  })
  
  testData <- reactive({
    data()[-trainingSetIndex(),]
  })
  
  #######################################################################################################
  
  #Logistic regression model
  
  #lm_Model <- reactive({
  #  glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + 
  #        PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + 
  #        BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, data = trainingData(), 
  #      family=binomial)
  #})
  
  #Revised model after removing insignificant variables
  rev_lm_Model <- reactive({
    glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + 
          PAY_3 +  BILL_AMT1 + PAY_AMT1 + PAY_AMT2, data = trainingData(), family = binomial)
  })
  
  #Adding column to test dataset for prediction probability
  #Getting predicted probability of default variable for the test dataset
  probabilityForTestData <- reactive({
    predict(rev_lm_Model(), testData(), type = "response")
  })
  
  #Adding new column to test dataset for prediced values
  #Map the probabilities for above prediction
  #First insert all the values as 1 in new column
  #Update the value to 0 if the probability is smaller than 0.5
  testData_LM <- reactive({
    testData1 = testData()
    testData1$lm_predicted_value = probabilityForTestData()
    testData1$lm_prediction = rep(1, nrow(testData()))
    testData1$lm_prediction[testData1$lm_predicted_value < 0.5] = 0
    testData1
  })
  
  #Table for comparing prediction output vs actual output
  lm_Actual_Output <- reactive({
    testData_LM()$default.payment.next.month
  })
  
  lm_Predicted_Output <- reactive({
    testData_LM()$lm_prediction
  })
  
  output$lm_crosstable <- renderPrint({
    print("Confusion Matrix")
    CT <- CrossTable(lm_Predicted_Output(), lm_Actual_Output(), prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
             dnn = c('Predicted Output', 'Actual Output'))
  })
  
  output$lm_confusionMatrix <- renderPrint({
    print(confusionMatrix(lm_Predicted_Output(), lm_Actual_Output()))
  })
  
  output$lm_rocCurve <- renderPlot({
    roc <- roc(default.payment.next.month ~ lm_predicted_value, data = testData_LM())
    plot(roc, main = "ROC Curve for Logistic Regression")
  })
  
  #######################################################################################################
  
  #Decision Tree Model
  
  DT_Model <- reactive({
    C5.0(trainingData()[-25], as.factor(trainingData()$default.payment.next.month))
  })
  
  output$DT_Summary <- renderPrint({
    summary(DT_Model())
  })

  #Applying model on test data
  DT_predicted_value<- reactive({
    predict(DT_Model(), testData())
  })
  
  #Adding new column to test dataset for prediction
  testData_DT <- reactive({
    testData1 <- testData()
    testData1$DT_prediction <- DT_predicted_value()
    testData1
  })
  
  #Table for comparing prediction output vs actual output
  DT_Actual_Output <- reactive({
    testData_DT()$default.payment.next.month
  })
  
  DT_Predicted_Output <- reactive({
    testData_DT()$DT_prediction
  })
  
  output$DT_crosstable <- renderPrint({
    print("Confusion Matrix")
    CT <- CrossTable(DT_Predicted_Output(), DT_Actual_Output(), prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
                     dnn = c('Predicted Output', 'Actual Output'))
  })
  
  output$DT_confusionMatrix <- renderPrint({
    print(confusionMatrix(DT_Predicted_Output(), DT_Actual_Output()))
  })
  
  #######################################################################################################
  
  #SVM model
  
  svm_Model <- reactive({
    svm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + 
          PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + 
          BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, data = trainingData())
  })
  
  #Adding column to test dataset for prediction probability
  #Getting predicted probability of default variable for the test dataset
  probabilityForTestData_svm <- reactive({
    predict(svm_Model(), testData(), type = "response")
  })
  
  #Adding new column to test dataset for prediced values
  #Map the probabilities for above prediction
  #First insert all the values as 1 in new column
  #Update the value to 0 if the probability is smaller than 0.5
  testData_SVM <- reactive({
    testData1 = testData()
    testData1$svm_predicted_value = probabilityForTestData_svm()
    testData1$svm_prediction = rep(1, nrow(testData()))
    testData1$svm_prediction[testData1$svm_predicted_value < 0.5] = 0
    testData1
  })
  
  #Table for comparing prediction output vs actual output
  svm_Actual_Output <- reactive({
    testData_SVM()$default.payment.next.month
  })
  
  svm_Predicted_Output <- reactive({
    testData_SVM()$svm_prediction
  })
  
  output$svm_crosstable <- renderPrint({
    print("Confusion Matrix")
    CT <- CrossTable(svm_Predicted_Output(), svm_Actual_Output(), prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
                     dnn = c('Predicted Output', 'Actual Output'))
  })
  
  output$svm_confusionMatrix <- renderPrint({
    print(confusionMatrix(svm_Predicted_Output(), svm_Actual_Output()))
  })
  
  output$svm_rocCurve <- renderPlot({
    roc <- roc(testData()$default.payment.next.month ~ probabilityForTestData_svm(), data = testData())
    plot(roc, main = "ROC Curve for SVM")
  })
  
  #######################################################################################################
  
  #Random Forest Model
  
  RF_Model <- reactive({
    randomForest(default.payment.next.month~., data=trainingData(), ntree=60)
  })
  
  probabilityForTestData_RF<- reactive({
    predict(RF_Model(), testData(), type = "response")
  })
  
  testData_RF <- reactive({
    testData1 = testData()
    testData1$rf_predicted_value = probabilityForTestData_RF()
    testData1$rf_prediction = rep(1, nrow(testData()))
    testData1$rf_prediction[testData1$rf_predicted_value < 0.5] = 0
    testData1
  })

  #Table for comparing prediction output vs actual output
  rf_Actual_Output <- reactive({
    testData_RF()$default.payment.next.month
  })
  
  rf_Predicted_Output <- reactive({
    testData_RF()$rf_prediction
  })
  
  output$rf_crosstable <- renderPrint({
    print("Confusion Matrix")
    CT <- CrossTable(rf_Predicted_Output(), rf_Actual_Output(), prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
                     dnn = c('Predicted Output', 'Actual Output'))
  })
  
  output$rf_confusionMatrix <- renderPrint({
    print(confusionMatrix(rf_Predicted_Output(), rf_Actual_Output()))
  })
  
  output$rf_rocCurve <- renderPlot({
    roc <- roc(testData()$default.payment.next.month ~ probabilityForTestData_RF(), data = testData())
    plot(roc, main = "ROC Curve for Random Forest")
  })
  
  output$rf_Plot <- renderPlot({
    plot(RF_Model(), main = "Error Vs Trees Plot")
  })
  
  output$rf_variablePlot <- renderPlot({
    varImpPlot(RF_Model(), sort = T, main = "Variable Importance", n.var = 10)
  })
  
  #######################################################################################################
  
  #K Means Clustering
  
  #We are considering SEX,EDUCATION,MARRIAGE,AGE and default.payment.next.month for clustering 
  #hence convering it to numeric and taking subset for default.payment.next.month = 1
  data_kmeans <- reactive({
    data1 = data()
    datafactor1 = c(3,4,5,6,25)
    data1[,datafactor1] = lapply(data1[,datafactor1], as.numeric)
    data1 = subset(data1,data1$default.payment.next.month == 1)
    data1[,datafactor1]
  })
  
  #Applying kmeans initially 2 clusters
  kmean1 <- reactive({
    kmean = kmeans(data_kmeans(),2,nstart=20)
    kmean$cluster = as.factor(kmean$cluster)
    kmean
  })
  
  output$kmean1Summary <- renderPrint({
    kmean1()
  })
  
  #Plotting the results
  output$kmean1Plot <- renderPlot({
    plot(data_kmeans(),col=(kmean1()$cluster))
  })
  
  output$kmean1plotcluster <- renderPlot({
    plotcluster(data_kmeans(),kmean1()$cluster,main="K Mean Clustering with 2 clusters")
  })
  
  #Checking optimal number of clusters
  elbow_graph <- function(data, nc){
    eg <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      eg[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    plot(1:nc, eg, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Elbow Curve")
  }
  
  output$elbow_graph <- renderPlot({
    elbow_graph(data_kmeans(), nc=10) 
  })
  
  output$elbow_graph_conclusion <- renderPrint({
    paste("We can see in the above Elbow Curve that after 4 clusters there is no significant difference in the 'Within groups sum of squares'. Hence we can say that optimal number of clusters is 4.")
  })
  
  #Applying kmeans with 4 clusters
  kmean2 <- reactive({
    kmean = kmeans(data_kmeans(),4,nstart=20)
    kmean$cluster = as.factor(kmean$cluster)
    kmean
  })
  
  output$kmean2Summary <- renderPrint({
    kmean2()
  })
  
  #Plotting the results
  output$kmean2Plot <- renderPlot({
    plot(data_kmeans(),col=(kmean2()$cluster))
  })
  
  output$kmean2plotcluster <- renderPlot({
    plotcluster(data_kmeans(),kmean2()$cluster,main="K Mean Clustering with 4 clusters")
  })
  
  output$result <- renderPrint({
    aggregate(data_kmeans(), by=list(cluster=kmean2()$cluster),mean)
  })
  
  #######################################################################################################
  
  #Displays the output on Screen
  output$selectedOptionOutput <-renderUI({
    if(is.null(data())){
      print("Please upload File")
    }
    else{
      tabsetPanel(
        if(input$option == "Summary"){
          tabPanel(
            "Basic Information", verbatimTextOutput("nrow"), verbatimTextOutput("ncol"), 
            verbatimTextOutput("structure"), plotOutput("plotGenderVsDefaulter"), 
            plotOutput("plotEducationVsDefaulter"), plotOutput("plotMarriageVsDefaulter"),
            plotOutput("ageHistogram"), plotOutput("plotAgeVsDefaulter"), plotOutput("plotAgeVsEducationVsDefaulter"),
            plotOutput("plotPayVsDefaulter")
          )
        }else if(input$option == "View Data"){
          tabPanel(
            "View Data", DT::dataTableOutput("dataTable")
          )
        }else if(input$option == "Logistic Regression"){
          tabPanel(
            "Logistic Regression", verbatimTextOutput("lm_crosstable"), verbatimTextOutput("lm_confusionMatrix"), 
            plotOutput("lm_rocCurve")
          )
        }else if(input$option == "Decision Tree"){
          tabPanel(
            "Decision Tree", verbatimTextOutput("DT_Summary"), verbatimTextOutput("DT_crosstable"), 
            verbatimTextOutput("DT_confusionMatrix")
          )
        }else if(input$option == "Support Vector Machine (SVM)"){
          tabPanel(
            "Support Vector Machine (SVM)", verbatimTextOutput("svm_crosstable"), verbatimTextOutput("svm_confusionMatrix"), 
            plotOutput("svm_rocCurve")
          )
        }else if(input$option == "Random Forest"){
          tabPanel(
            "Random Forest", verbatimTextOutput("rf_crosstable"), verbatimTextOutput("rf_confusionMatrix"), 
            plotOutput("rf_rocCurve"), plotOutput("rf_Plot"), plotOutput("rf_variablePlot")
          )
        }else if(input$option == "K Means Clustering"){
          tabPanel(
            "K Means Clustering", verbatimTextOutput("kmean1Summary"), plotOutput("kmean1Plot"), plotOutput("kmean1plotcluster"),
            plotOutput("elbow_graph"), textOutput("elbow_graph_conclusion"), br(),
            verbatimTextOutput("kmean2Summary"), verbatimTextOutput("result"),
            plotOutput("kmean2Plot"), plotOutput("kmean2plotcluster")
          )
        }else{
          print("Please select valid option")
        }
      )
    }
  })
  
})