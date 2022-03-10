
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(imager)
library(bslib)

img <- grayscale(load.image('example.jpg'))
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 3, bootswatch = "readable", base_font = font_google('Roboto')),
    # Show a plot of the generated distribution
  tags$div(
    style="margin-top:25px;",
    mainPanel( align = 'center', fluid = FALSE, width = '100%',
       sliderInput(inputId = 'thx', label = 'Binarization Threshold',  min = 0, max = 255, value = 100, width = '80%'),
       fluidRow(
         splitLayout(cellWidths = c("50%", "50%"),
       plotOutput(outputId = "OrigImage",  width = '100%'),
       plotOutput(outputId = "ThreshImage",  width = '100%')
         )
       )
    )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$OrigImage <- renderPlot({
    plot(img, axes = FALSE)
  })
  
  output$ThreshImage <- renderPlot({
    thx <- input$thx/255
    imgTHX <- threshold(img, thr = thx)
    plot(imgTHX, axes = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
