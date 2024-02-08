library(testthat)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(BCFerriesConnector)


test_that("Integration flow from fetching routes to plotting capacity works", {
  # Step 1: Fetch routes
  routes_info <- get_routes()
  expect_true(nrow(routes_info) > 0, info = "Routes info should not be empty.")

  # Pick a route for further testing, assuming routes_info contains a column `routeCode`
  test_route <- routes_info$routeCode[1]

  schedule_info <- get_schedule(test_route)
  expect_true(nrow(schedule_info) > 0, info = "Schedule info should not be empty for the test route.")

  capacity_info <- get_capacity(test_route)
  expect_true(nrow(capacity_info) > 0, info = "Capacity info should not be empty for the test route.")
})






