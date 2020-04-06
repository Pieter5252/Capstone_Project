library(shiny)
source("./Script/predict.R", local = TRUE)

# predict the next word/s
shinyServer(function(input, output) {

    output$pred <- renderText({
            return(pred(input$caption, input$num))
       

    })

})
