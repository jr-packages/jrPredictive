library(shiny)
library(caret)
data(advertising, package = "jrPredictive")


ui =  fluidPage(
  titlePanel("Examine the advertising data set"),
  sidebarLayout(
    sidebarPanel(
      textInput("formula", "Predictor terms", value = "TV + Radio"),
      selectInput("method", "Method", choices = c("lm", "knn", "rpart", "rf")),
      checkboxInput("points", "Include Points?", value = FALSE),
      uiOutput("extra"),
      sliderInput("angle", "Z Axis rotation", value  = 30, min = 0, max = 360)
      #         submitButton("GO!")
    ),
    mainPanel(
      plotOutput("surface")
    )
  )
)


server = function(input, output) {
  grid = reactive({
    if (input$method == "knn")
      data.frame(k = input$k)
    else if (input$method == "rpart")
      data.frame(cp = input$k)
    else if (input$method == "rf")
      data.frame(mtry = 2)
    else NULL
  })
  slider = reactive({
    if (input$method == "knn")
      sliderInput("k", "# Nearest Neighours", value = 10, min = 1, max = 200)
    else if (input$method == "rpart")
      numericInput("k", "Complexity parameter", value = 0.1)
  })
  output$extra = shiny::renderUI(slider())
  modelformula = reactive({
    as.formula(paste("Sales~", input$formula))
  })
  modelfit = reactive({
    train(modelformula(), data = advertising, method = input$method,
          trControl = trainControl(method = "none"), tuneGrid = grid(),
          preProcess = c("center", "scale"))
  })
  output$surface = renderPlot({
    plot3d(modelfit(), xvar = advertising$TV,
           yvar = advertising$Radio,
           zvar = advertising$Sales,
           points = input$points,
           col = 5,
           theta = input$angle, zlim = range(advertising$Sales))
  })
}




shinyApp(server = server, ui = ui)
