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

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(MASS)
    library(plotly)
    library(mvtnorm)
    library(ggplot2)
    library(dplyr)
    library(widgetframe)
    
    output$distPlot <- renderPlot({
        mvnorm1000 <- 
            mvrnorm(
                n = 1000, 
                mu = c(10, 15), 
                Sigma = 
                    matrix(c(30, 40, 40, 60), 
                           nrow=2, ncol=2, byrow=FALSE)
            )
        
        dens <- kde2d(mvnorm1000[,1], mvnorm1000[,2])
            plot_ly(x = dens$x,
                    y = dens$y,
                    z = dens$z) %>% 
            add_surface()
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
