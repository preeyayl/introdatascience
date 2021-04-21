library(shiny)
library(UsingR)
data(galton)

ui <- fluidPage(
  titlePanel(
    "Histogram"),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  helpText("Please guess the mean"),

                  sliderInput("mu", h4("Chosen mean"),
                              min = 62, max = 74, value = 70, step= 0.05)
                ),
                
                mainPanel(
                  plotOutput('newHist')
                  
                )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$newHist <- renderPlot({
    hist(galton$child, xlab = 'Childs height', col = 'pink', main = 'Histogram of childs height')
    mu <- input$mu
    lines(c(mu,mu), c(0,200), col = 'red', lwd = 6)
    mse <- mean((galton$child - mu)^2)
    text(63,150,paste("mu is", mu))
    text(63,140,paste("mean square error of prediction is", round(mse,2)))
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)