library(shiny)
library(mice)
library(dplyr)
library(randomForest)


set.seed(1999)  ## for Prince

## load the data
setwd("C:/Users/Calvin/Desktop/ORSA_Topics_CBM")
trainSet <- read.table("train.csv", sep = ",", header = TRUE)  ## load training data
testSet <- read.table("test.csv", sep = ",", header = TRUE)  ## load test data

## get all data from train and test sets

suppressWarnings(Alldata <- bind_rows(trainSet, testSet))  ## bind all data to prepare for MICE

## use MICE to fill in mising age values

sink("NUL")
suppressWarnings(mice_mod <- mice(Alldata[, !names(Alldata) %in% c('PassengerId','Name','Ticket','Cabin')], method='rf'))  ## impute ages 
sink()

mice_output <- complete(mice_mod)  ## create output for all ages, including imputed values
Alldata$Age <- mice_output$Age  ## insert newly computed ages (includes 177 imputed values)
trainSet$Age <- Alldata$Age[1:891]  ## use MICE values; 




## create lists for drop down menus

lst.gender <- as.character(unique(trainSet$Sex))
lst.Plcass <- as.character(unique(trainSet$Pclass))
lst.Embark <- as.character(unique(trainSet$Embarked))

## create user interface page

ui <- fluidPage(
  titlePanel("Titanic Risk Reduction Calculator"),
  p(),
  h3("What Can you Change To Increase the Probability of Survival?", align = "center"),
  br(),
  sidebarPanel(
    p("Fixed Attributes"),
    selectInput('Sex', 'Sex', choices = lst.gender, selected = "male"),
    numericInput('Age', 'Age (1 to 80)', 25, min = 1, max = 80),
    
    
    p("Variables for targeted action"),
    selectInput('Pclass', 'Passenger Class:', list("1st Class Passenger" = "1","2nd Class Passenger" = "2", "3rd Class Passenger" = "3"), selected = 3),
    numericInput('Fare', 'Fare (1 to 125)', 8, min = 1, max = 125),
    selectInput('Embarked', 'Port of Embarkation:', list("Cherbourg" = "C","Queenstown" = "Q", "Southampton" = "S"), selected = "C")
  ),
  
  mainPanel(
    img(src='RMS_Titanic_3_wikipedia.jpg', align = "right"),
    h3('The Predicted Probability of Survival Is:', align = "center"),
    br(),  ## line break
    h2(textOutput('prob'), style = 'color:blue', align = "center"),
    br(),
    br(),
    br(),
    h3('The 95% confidence interval is:', align = "center"),
    h2(textOutput('conf'), style = 'color:blue', align = "center"),
    br(),
    br(),
        h5("This estimated probability is based on a simple mathematical model.", align = "center")
  )
)

## prepare to make a model


## convert variables to factors to use in GLM

trainSet$Pclass <- as.factor(trainSet$Pclass)  ## convert to factors
trainSet$Sex <- as.factor(trainSet$Sex)  ## convert to factors
trainSet$Embarked <- as.factor(trainSet$Embarked) ## convert to factors

## train the model

glm_big2 <- glm(Survived ~ Sex + Age + Pclass + Fare + Embarked, data = trainSet, family = binomial)

## create the server function

server <- function(input, output) {
  
  ## get the data in reactive mode
  
  dataInput <- reactive({
    data.frame(
      Sex = input$Sex, 
      Age = input$Age, 
      Pclass = input$Pclass,
      Fare = input$Fare,
      Embarked = input$Embarked
      )
  })
  
  ## calculate the prediction based on dataInput
  
  tryit <- reactive({
    suppressWarnings(
    predict(glm_big2, dataInput(), type = "response", se.fit = TRUE)
    )
    
    })
  
  ## calculate the confidence interval
  
  conf_int <- reactive({
    c(tryit()$fit - 1.96*tryit()$se.fit, tryit()$fit + 1.96*tryit()$se.fit)
    
  })
  


  ## send the output to the main panel, showing answer as a percentage
  ##  First display the predicted probability of survival
  
  output$prob <- renderText({
    
    (sprintf("%0.1f%%",100*(tryit()$fit)))
    
  })
  
  ## render text to display the confidence interval
  
  output$conf <- renderText({
    
    paste0((sprintf("%0.1f%%",100*(conf_int()[1]))),
    ' to ', 
    (sprintf("%0.1f%%",100*(conf_int()[2]))))
  })
  
## run the app  
  
}

shinyApp(ui = ui, server = server)
