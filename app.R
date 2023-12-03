library(shiny)
library(plotly)

# Load the data from circuits.csv (make sure the file is in the same directory as this script)
circuits <- read.csv("train.csv")

ui <- fluidPage(
  titlePanel("House price Scatter Plot"),
  
  sidebarLayout(
    sidebarPanel(
#      selectInput("sqft", "Select GrLivArea:",
#                  choices = unique(circuits$GrLivArea),
#                  selected = unique(circuits$GrLivArea)[1:3],
#                  multiple = TRUE),
      selectInput("mustang", "Select Neighborhood:",
                  choices = unique(circuits$Neighborhood),
                  selected = unique(circuits$Neighborhood)[1:3],
                  multiple = TRUE),
      actionButton("update_plot", "Update Plot")
    ),
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

server <- function(input, output) {
  # Reactive function to update the plot based on user input
  reactive_circuits <- reactive({
    circuits_top_countries <- circuits[circuits$GrLivArea %in% input$sqft,]
    return(circuits_top_countries)
  })
  
  reactive_circuits1 <- reactive({
    circuits_top_countries1 <- circuits[circuits$Neighborhood %in% input$mustang,]
    return(circuits_top_countries1)
  })
  
  output$scatter_plot <- renderPlotly({
    # Get the data for the selected countries from the reactive function
    filtered_circuits <- reactive_circuits()
    filtered_circuits1 <- reactive_circuits1()
    
    # Plot the data using plotly
    p <- plot_ly(filtered_circuits1, x = ~GrLivArea, y = ~SalePrice, color = ~YearBuilt,
                 colors = "Set1", alpha = 0.7, size = I(1000),
                 type = "scatter", mode = "markers", marker = list(line = list(color = 'black', width = 1)))

    # Update the layout
    p <- p %>% layout(title = "GrLivArea vs SalePrice",
                      xaxis = list(title = "GrLivArea"),
                      yaxis = list(title = "price"),
                      showlegend = TRUE)
    
    return(p)
  })
}

# Run the Shiny app
shinyApp(ui, server)
