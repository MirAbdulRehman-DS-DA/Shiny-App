#' Fetch Weather for a City in Sweden
#'
#' @param city_name The name of the Swedish city (e.g., "Stockholm", "Gothenburg").
#' @return A list containing the current weather data (temperature, windspeed, etc.) for the specified city.
#' @export
#' @examples
#' get_weather("Stockholm")
#' @name get_weather
#'
library(httr)
library(jsonlite)

# Define a list of Swedish cities and their latitudes and longitudes
city_coordinates <- list(
  "Stockholm" = list(latitude = 59.3293, longitude = 18.0686),
  "Gothenburg" = list(latitude = 57.7089, longitude = 11.9746),
  "Malmo" = list(latitude = 55.6050, longitude = 13.0038),
  "Uppsala" = list(latitude = 59.8586, longitude = 17.6389),
  "Vasteras" = list(latitude = 59.6099, longitude = 16.5448),
  "Orebro" = list(latitude = 59.2753, longitude = 15.2134),
  "Linkoping" = list(latitude = 58.4108, longitude = 15.6214),
  "Helsingborg" = list(latitude = 56.0465, longitude = 12.6945),
  "Jonkoping" = list(latitude = 57.7826, longitude = 14.1618),
  "Norrkoping" = list(latitude = 58.5942, longitude = 16.1826),
  "Lund" = list(latitude = 55.7047, longitude = 13.1910),
  "Umea" = list(latitude = 63.8258, longitude = 20.2630),
  "Gavle" = list(latitude = 60.6749, longitude = 17.1413),
  "Boras" = list(latitude = 57.7210, longitude = 12.9401),
  "Sodertalje" = list(latitude = 59.1955, longitude = 17.6253),
  "Eskilstuna" = list(latitude = 59.3704, longitude = 16.5094),
  "Karlstad" = list(latitude = 59.3793, longitude = 13.5036),
  "Taby" = list(latitude = 59.4439, longitude = 18.0687),
  "Sundsvall" = list(latitude = 62.3908, longitude = 17.3069),
  "Lulea" = list(latitude = 65.5848, longitude = 22.1547)
)

#' Get Weather for a Swedish City
#'
#' @param city_name Name of the Swedish city
#' @return A list with the current weather information for the city
#' @export
#' @name get_weather
get_weather <- function(city_name) {
  # Check if the city is in the list of supported cities
  if (!city_name %in% names(city_coordinates)) {
    stop("City not found in the database.")
  }

  # Get latitude and longitude for the city
  lat_long <- city_coordinates[[city_name]]

  # Base URL for the weather API
  base_url <- "https://api.open-meteo.com/v1/forecast"
  url <- paste0(
    base_url,
    "?latitude=", lat_long$latitude,
    "&longitude=", lat_long$longitude,
    "&current_weather=true"
  )

  # Make the API request
  response <- httr::GET(url)

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch weather data. Please check the city name or try again later.")
  }

  # Parse the JSON response safely
  tryCatch({
    weather_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(weather_data$current_weather)
  }, error = function(e) {
    stop("Failed to parse weather data. Please try again later.")
  })
}

# Example usage:
# weather_data <- get_weather("Linkoping")
# print(weather_data)
