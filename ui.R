library(shiny)
shinyUI(fluidPage(
  titlePanel(h2("Default of Credit Card Clients Dataset", align = "center")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Please upload file"),
      br(),
      selectInput("option", "Please select the desired option", 
                  choices = c("Summary", "View Data", "Logistic Regression", "Decision Tree", 
                              "Support Vector Machine (SVM)", "Random Forest", "K Means Clustering"))
    ),
    mainPanel(
      uiOutput("selectedOptionOutput")
    )
  )
))