library(shiny)
library(caret)
data("mixture.example", package = "ElemStatLearn")
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
df = data.frame(x = x[,1], z = x[,2] ,y = factor(g,labels = c("No","Yes")))
dd = data.frame(x = xnew[,1], z = xnew[,2])
server = function(input,output){
  slider= reactive({
    if(input$method == "knn")
      sliderInput("k","# Nearest Neighoburs",
                  min = 1, max = 30, step = 2,value = 10)
  })

  output$slider = renderUI({
    slider()
  })

  grid = reactive({
    if(input$method == "knn") data.frame(k = input$k)
    else NULL
  })
  modelfit = reactive({
    train(y~x+z, data = df, method = input$method, tuneGrid = grid(),
          trControl = trainControl(method = "none"), preProcess = c("center","scale"))
  })
  output$plot = renderPlot({
    jrPredictive::boundary_plot(modelfit(),df$x,df$z,df$y, lwd=5,jitter=FALSE)
  })


}

ui = fluidPage(
  titlePanel("Classification boundaries"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("method","Classifier",
                   choices = c("lda","qda","knn","glm")),
      uiOutput("slider")
    ),
    mainPanel(plotOutput("plot"))
  )
)


shinyApp(server=server, ui=ui)
