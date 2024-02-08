library(testthat)
library(httr)
library(jsonlite)


test_that("get_data() returns a list or data frame on success", {
  result <- get_data()
  expect_true(is.list(result) || is.data.frame(result),
              info = "get_data should return a list or data frame")
})


test_that("get_routes() returns a data frame with specific columns", {
  route_info <- get_routes()
  expect_true(is.data.frame(route_info),
              info = "get_routes should return a data frame")
  expected_columns <- c("routeCode", "fromTerminalCode", "toTerminalCode")
  for (col in expected_columns) {
    expect_true(col %in% names(route_info),
                info = sprintf("Expected column '%s' not found in route_info", col))
  }
})

