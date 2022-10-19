## app.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggrepel)

#mac office
#file = /Users/arahjou/Documents/Shinyapp/Rif1/
mydata <- read.csv("Rif1_original.csv", sep = ",", header = TRUE)
mydata <- mydata[c("id","Gene_Name","Protein_Descriptions","MW","PEP","Ratio", "Cut_off", "count", "PEP_log10")]
#Filtering data
mydata <- subset(mydata, mydata$count >= 4)
mydata <- subset(mydata, mydata$PEP_log10 >= 1.3013)
mydata <- subset(mydata, mydata$Ratio != 	"#DIV/0!")
mydata <- subset(mydata, mydata$Cut_off != "xSD")



#data gromming
mydata$Ratio <- as.numeric(mydata$Ratio)
mydata$Cut_off <- factor(mydata$Cut_off, levels=c("xSD", "1xSD", "2xSD", "3xSD", "4xSD"))

if (interactive()) {
    ui <- dashboardPage(
        dashboardHeader(title = "Rif1 interactors"),
        dashboardSidebar(
            sidebarMenu(
                textAreaInput("Rif1Interactors", "Please type one name per line \n and use gene symbol", value = "Rif1")
            )
        ),
        dashboardBody(
            # Boxes need to be put in a row (or column)
            fluidRow(
                box(plotOutput("plot1", height = 500, width = 650)),
                tableOutput("information")
            )
        )
    )
    
    server <- function(input, output) {
        
        output$plot1 <- renderPlot({
            data2 <- strsplit(input$Rif1Interactors, "\n")[[1]]
            data1 <- mydata[ mydata$Gene_Name %in% c(data2), ]
            #data1 <- mydata[ mydata$Gene_Name %in% c(input$Rif1Interactors), ]
            data1$Ratio <- as.numeric(data1$Ratio)
            ggplot(mydata, aes(x=Ratio, y=PEP_log10, color = Cut_off)) + 
                geom_point(aes(size=log(count))) +  
                geom_text_repel(data= data1, label = data1$Gene_Name, color="black", box.padding = 1, max.overlaps = Inf, size=6) +
                labs(x = "H(H+L)", y ="Log(-Log(PEP))")
        })
        output$information <- renderTable(mydata[ mydata$Gene_Name %in% c(strsplit(input$Rif1Interactors, "\n")[[1]]), ])
        #output$information <- renderTable(mydata[ mydata$Gene_Name %in% c(input$Rif1Interactors), ])
        
        
    }
    
    shinyApp(ui, server)
}
