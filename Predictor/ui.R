library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Predict next word/s"),

    # Sidebar with a slider input for number of words
    sidebarLayout(
        sidebarPanel(
            h4("Select number of words to predict"),
            sliderInput("num",
                        "Number of words:",
                        min = 1,
                        max = 6,
                        value = 1)
        ),

        # Text input, submit button and predicted return
        mainPanel(
            textInput(inputId = "caption",
                      label = "Input text here"),
            submitButton("submit"),
            verbatimTextOutput("pred")
        )
    )
))
