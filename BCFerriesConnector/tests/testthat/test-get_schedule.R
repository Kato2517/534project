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


test_that("get_schedule() filters data correctly for a given routeCode", {
  test_routeCode <- "ExampleRouteCode"  
  
  schedule_info <- get_schedule(test_routeCode)
  expect_true(is.data.frame(schedule_info))
 
  expect_true(all(schedule_info$routeCode == test_routeCode), info = "Filtered data should only contain the specified routeCode")

  expected_columns <- c("routeCode", "sailingDuration", "time", "arrivalTime", "sailingStatus", "vesselName")
  for (col in expected_columns) {
    expect_true(col %in% names(schedule_info), info = sprintf("Expected column '%s' not found in schedule_info", col))
  }
})
