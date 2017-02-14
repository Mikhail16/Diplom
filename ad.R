library(shiny)
runApp(list(
  ui = fluidPage(
    titlePanel("Hello Shiny!"),
    sidebarLayout(
      sidebarPanel(
        tags$form(
          numericInput('n', 'Number of obs', 100)
          , br()
          , actionButton("button1", "Action 1")
        )
        , tags$form(
          textInput("text", "enter some text", value= "some text")
          , br()
          , actionButton("button2", "Action 2")
        )
      ),
      mainPanel(
        plotOutput('plot')
        , textOutput("stext")
      )
    )
  ),
  server = function(input, output) {
    output$plot <- renderPlot({ 
      input$button1
      hist(runif(isolate(input$n))) 
    })
    output$stext <- renderText({
      input$button2
      isolate(input$text )
    })
  }
)
)