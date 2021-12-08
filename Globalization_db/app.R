library(shiny)
library(leaflet)
library(pacman)
p_load(rio)
KGI <- rio::import(file = "KGI.Rdata")

ui <- fluidPage(
    titlePanel("A Globalization Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(KGI$date,
                        "Year",
                        min = 1990,
                        max = 2020,
                        value = 2020,
                        sep = "")
        ),

        mainPanel(
            leafletOutput("map") 
        )
    )
)

# Define server logic required to draw the map (see 03_map)
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
