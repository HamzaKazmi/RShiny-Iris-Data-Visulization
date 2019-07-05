#Hamza Kazmi
#R shiny Iris dataset visualization application


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  headerPanel('Distribution of Length and Width of Petal and Sepal based on Species'),
  sidebarPanel(
    selectInput('Species', 'Select Species', as.character(unique(iris$Species)))
  ),
  mainPanel(
    plotOutput('plot1',width = 800, height = 600)
  )
)

library(ggplot2)
library(gridExtra)


# Server to create graphs for the dashboard
server <- function(input, output) {
  
  data <- reactive({iris[iris$Species == input$Species,]})
  
  
  output$plot1 <- renderPlot({
    
    #Plot 1:Sepal Length 
    SepalLengthGraph <- ggplot(data(), aes(Sepal.Length))
    SepalLengthGraph <- SepalLengthGraph + geom_histogram(binwidth = .5, fill="blue", color = "black")
    SepalLengthGraph <- SepalLengthGraph + geom_vline( aes(xintercept = mean(data()$Sepal.Length)), linetype="dashed",color="grey")
    SepalLengthGraph <- SepalLengthGraph + labs(x = "Sepal Length")
    SepalLengthGraph <- SepalLengthGraph + labs(y = "Frequency")
    SepalLengthGraph <- SepalLengthGraph + labs(title = paste("Frequency of different Sepal Lengths", round(mean(data()$Sepal.Length))))
    
    #Plot 2:Sepal Width
    SepalWidthGraph <- ggplot(data(), aes(Sepal.Width))
    SepalWidthGraph <- SepalWidthGraph + geom_histogram(binwidth = .5, fill="blue", color = "black")
    SepalWidthGraph <- SepalWidthGraph + geom_vline( aes(xintercept = mean(data()$Sepal.Width)),  linetype="dashed",color="grey")
    SepalWidthGraph <- SepalWidthGraph + labs(x = "Sepal Width")
    SepalWidthGraph <- SepalWidthGraph + labs(y = "Frequency")
    SepalWidthGraph <- SepalWidthGraph + labs(title = paste("Frequencies of different Sepal Widths"))
    
    #Plot 3:Petal Length
    PetalLengthGraph <- ggplot(data(), aes(Petal.Length))
    PetalLengthGraph <- PetalLengthGraph + geom_histogram(binwidth = .5, fill="red", color = "black")
    PetalLengthGraph <- PetalLengthGraph + geom_vline( aes(xintercept = mean(data()$Petal.Length)),linetype="dashed",color="grey")
    PetalLengthGraph <- PetalLengthGraph + labs(x = "Petal Length")
    PetalLengthGraph <- PetalLengthGraph + labs(y = "Frequency")
    PetalLengthGraph <- PetalLengthGraph + labs(title = paste("Frequencies of different Petal Lengths"))
    
    #Plot 4:Petal Width
    PetalWidthGraph <- ggplot(data(), aes(Petal.Width))
    PetalWidthGraph <- PetalWidthGraph + geom_histogram(binwidth = .5, fill="red", color = "black")
    PetalWidthGraph <- PetalWidthGraph + geom_vline( aes(xintercept = mean(data()$Petal.Width)), linetype="dashed",color="grey")
    PetalWidthGraph <- PetalWidthGraph + labs(x = "Petal Width")
    PetalWidthGraph <- PetalWidthGraph + labs(y = "Frequency")
    PetalWidthGraph <- PetalWidthGraph + labs(title = paste("Frequencies of different Petal Widths"))
    
    #Graphing the 4 plots 
    grid.arrange(SepalLengthGraph,SepalWidthGraph,PetalLengthGraph,PetalWidthGraph,nrow=2, ncol=2)
  })
  
}






# Run the application 
shinyApp(ui = ui, server = server)

