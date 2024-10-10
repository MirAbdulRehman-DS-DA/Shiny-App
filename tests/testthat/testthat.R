library(testthat)
library(httr)
library(jsonlite)

# Test for handling invalid city inputs
test_that("API handles invalid input gracefully", {
  # Check for an unknown city
  expect_error(get_weather("UnknownCity"), "City not found in the database.")

  # Simulate a network failure by passing a non-existent city
  expect_error(get_weather("InvalidCity"), "City not found in the database.")
})

# Test for handling empty inputs
test_that("API handles empty inputs", {
  expect_error(get_weather(""), "City not found in the database.")
})

# Test for returning valid output for valid city names
test_that("API returns valid output for valid city names", {
  result <- get_weather("Stockholm")

  # Check that the result contains the expected fields (like temperature)
  expect_true("temperature" %in% names(result))
  expect_true(is.numeric(result$temperature))

  # Check that the returned weather data has non-null values
  expect_true(!is.null(result$temperature))
})

# Test for returning correct error messages for invalid inputs
test_that("API returns correct error messages for invalid inputs", {
  expect_error(get_weather("NonExistentCity"), "City not found in the database.")
})

# Test for handling multiple valid city queries
test_that("API handles multiple valid city queries", {
  cities <- c("Stockholm", "Gothenburg", "Malmö")

  # Mock response simulating what the API would return
  mock_response <- list(
    current_weather = list(
      temperature = 20,
      windspeed = 5
    )
  )

  # Use with_mocked_bindings to mock the httr::GET function
  with_mocked_bindings(
    `httr::GET` = function(...) {
      structure(
        list(
          content = function(...) jsonlite::toJSON(mock_response),  # Return mock response as JSON
          status_code = function() 200  # Simulating a successful response status
        ),
        class = "response"
      )
    },
    {
      for (city in cities) {
        result <- get_weather(city)  # Call the function with mocked GET
        expect_equal(result$temperature, 20)  # Check if the mocked temperature is returned
        expect_true(is.numeric(result$temperature))  # Ensure it's numeric
        expect_true(result$temperature >= -30 && result$temperature <= 50)  # Reasonable temperature check
      }
    }
  )
})

