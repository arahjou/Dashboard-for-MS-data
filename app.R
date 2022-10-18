#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Rif1 interactors (I-DIRT list)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            radioButtons("Variablechoice", "Choice of Variable", 
                         choices = c("Waiting Time", "Eruption Time"),
                         selected = "Waiting Time"),
            checkboxInput("axischoice", "Flip axisws?")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("usefullabel"),
           verbatimTextOutput("summaryofvariable"),
           plotOutput("scatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        if(input$Variablechoice == "Waiting Time") {x    <- faithful[, 2]}
        if(input$Variablechoice == "Eruption Time") {x    <- faithful[, 1]}
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white', 
             xlab=input$Variablechoice, 
             main=paste("Histogram of" , input$Variablechoice))
    })
    
    output$summaryofvariable <- renderPrint({
        if(input$Variablechoice == "Waiting Time") {x    <- faithful[, 2]}
        if(input$Variablechoice == "Eruption Time") {x    <- faithful[, 1]}
        summary(x)
        
    })
    
    
    output$usefullabel <- renderText({
        
        paste("Summary of" , input$Variablechoice, ":")
    })
    
    output$scatterPlot <- renderPlot({
        if (input$axischoice == FALSE){
        y <- faithful$eruptions
        x <- faithful$waiting
        plot(y~x, xlab="Wating Time", ylab = "Eruption Time")}
        if (input$axischoice == TRUE){
            x <- faithful$eruptions
            y <- faithful$waiting
            plot(y~x, xlab="Eruption Time", ylab = "Waiting Time")}
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
