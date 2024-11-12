library(shiny)

# Define the list of Swedish cities
city_list <- c(
  "Stockholm", "Gothenburg", "Malmo", "Uppsala", "Vasteras", "Orebro",
  "Linkoping", "Helsingborg", "Jonkoping", "Norrkoping", "Lund", "Umea",
  "Gavle", "Boras", "Sodertalje", "Eskilstuna", "Karlstad", "Taby",
  "Sundsvall", "Lulea"
)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Sweden Weather Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select a city:", choices = city_list),  # Dropdown list of cities
      actionButton("get_weather", "Get Weather")
    ),

    mainPanel(
      textOutput("weather_output")  # Display the weather details here
    )
  )
)

# Define server logic for the application
server <- function(input, output) {
  observeEvent(input$get_weather, {
    output$weather_output <- renderText({
      result <- get_weather(input$city)  # Fetch weather for the selected city
      paste(
        "City:", input$city,
        "\nTemperature:", result$temperature, "°C",
        "\nWindspeed:", result$windspeed, "m/s"
      )
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
