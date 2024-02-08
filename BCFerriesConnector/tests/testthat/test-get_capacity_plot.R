library(testthat)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

test_that("get_data() returns a list or data frame on success", {
  result <- get_data()
  expect_true(is.list(result) || is.data.frame(result))
})


test_that("get_combined_sailings_df() returns a data frame with expected columns", {
  combined_sailings_df <- get_combined_sailings_df()
  expect_true(is.data.frame(combined_sailings_df))
  expected_columns <- c("routeCode", "fromTerminalCode", "toTerminalCode", "sailingDuration", 
                        "time", "arrivalTime", "sailingStatus", "vesselName", "carFill", "oversizeFill")
  for (col in expected_columns) {
    expect_true(col %in% names(combined_sailings_df), info = sprintf("Expected column '%s' not found", col))
  }
})


test_that("get_capacity() returns a data frame with expected columns for a specific routeCode", {
  test_routeCode <- "TSASWB"
  capacity_info <- tryCatch({
    get_capacity(test_routeCode)
  }, error = function(e) {
    NULL
  })
  
  expect_true(!is.null(capacity_info), info = "get_capacity should return a data frame")
  
  if (!is.null(capacity_info)) {
    expected_columns <- c("routeCode", "time", "sailingStatus", "vesselName", "fill", "carFill", "oversizeFill")
    expect_true(all(expected_columns %in% names(capacity_info)), info = "Data frame should contain all expected columns")
  }
})


test_that("get_capacity_plot() runs without errors and returns a ggplot object", {
  test_data <- data.frame(
    sailingStatus = c("current", "future"),
    time = c("08:00", "10:00"),
    carFill = c(80, 90),
    oversizeFill = c(60, 70)
  )
  
  plot <- get_capacity_plot(test_data)
  expect_true(inherits(plot, "ggplot"), info = "The function should return a ggplot object")
})


