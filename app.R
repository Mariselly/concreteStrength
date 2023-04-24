library(shiny)
library(readr)
library(caret)

# Read CSV file
concrete_data<-read.csv("Concrete_Data.csv")


# Train model
set.seed(123)
trainIndex <- createDataPartition(Concrete_Data$ConcreteCompressiveStrength, p = 0.7, list = FALSE)
training <- concrete_data[trainIndex, ]
testing <- concrete_data[-trainIndex, ]
model <- train(as.formula("ConcreteCompressiveStrength ~ ."), method = "lm", data = training)

# Define UI for app
ui <- fluidPage(
  # Title
  titlePanel("Model Prediction App"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Inputs for other eight columns
      numericInput("Cement", "Enter value for Cement Column", 102.0, min=0),
      numericInput("BlastFurnaceSlag", "Enter value for BlastFurnaceSlag Column", 153.0, min=0),
      numericInput("FlyAsh", "Enter value for FlyAsh Column", 0.0, min=0),
      numericInput("Water", "Enter value for Water Column", 192.0, min=0),
      numericInput("Superplasticizer", "Enter value for Superplasticizer Column", 0.0, min=0),
      numericInput("CoarseAggregate", "Enter value for CoarseAggregate Column", 887.0, min=0),
      numericInput("FineAggregate", "Enter value for FineAggregate Column", 942.0, min=0),
      numericInput("Age", "Enter value for Age Column", 3, min=0),
      
      # Predict button
      actionButton("predict", "Predict")
    ),
    
    # Main panel
    mainPanel(
      # Prediction output
      textOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Predict value when predict button is clicked
  prediction <- eventReactive(input$predict, {
    # Create data frame with input values
    inputValues <- data.frame(Cement = as.numeric(input$Cement),
                              BlastFurnaceSlag = as.numeric(input$BlastFurnaceSlag),
                              FlyAsh = as.numeric(input$FlyAsh),
                              Water = as.numeric(input$Water),
                              Superplasticizer = as.numeric(input$Superplasticizer),
                              CoarseAggregate = as.numeric(input$CoarseAggregate),
                              FineAggregate = as.numeric(input$FineAggregate),
                              Age = as.numeric(input$Age))
    
    # Predict value for ninth column
    predict(model, newdata = inputValues)
  })
  
  # Display prediction
  output$prediction <- renderText({
    paste("Predicted value for ConcreteCompressiveStrength Column:", prediction())
  })
}

# Run the app
shinyApp(ui = ui, server = server)



