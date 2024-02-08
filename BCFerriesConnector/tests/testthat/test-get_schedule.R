library(testthat)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

# 测试 get_data 函数是否返回正确的数据结构
test_that("get_data() returns a list or data frame on success", {
  result <- get_data()
  expect_true(is.list(result) || is.data.frame(result))
})

# 测试 get_combined_sailings_df 函数返回的数据框是否含有特定的列
test_that("get_combined_sailings_df() returns a data frame with expected columns", {
  combined_sailings_df <- get_combined_sailings_df()
  expect_true(is.data.frame(combined_sailings_df))
  expected_columns <- c("routeCode", "fromTerminalCode", "toTerminalCode", "sailingDuration", 
                        "time", "arrivalTime", "sailingStatus", "vesselName", "carFill", "oversizeFill")
  for (col in expected_columns) {
    expect_true(col %in% names(combined_sailings_df), info = sprintf("Expected column '%s' not found", col))
  }
})

# 测试 get_schedule 函数是否根据给定的 routeCode 返回相应的调度信息
test_that("get_schedule() filters data correctly for a given routeCode", {
  test_routeCode <- "ExampleRouteCode"  # 这里假设有一个示例路线代码，实际应用时应替换为真实路线代码
  
  # 为了这个测试有效，需要确保combined_sailings_df中至少有一条对应test_routeCode的数据
  # 由于我们无法控制外部API的响应，这里的测试假设get_combined_sailings_df函数工作正常，且API返回了数据
  schedule_info <- get_schedule(test_routeCode)
  expect_true(is.data.frame(schedule_info))
  # 检查是否所有返回行的routeCode都与给定的routeCode匹配
  expect_true(all(schedule_info$routeCode == test_routeCode), info = "Filtered data should only contain the specified routeCode")
  # 检查返回的调度信息是否包含预期的列
  expected_columns <- c("routeCode", "sailingDuration", "time", "arrivalTime", "sailingStatus", "vesselName")
  for (col in expected_columns) {
    expect_true(col %in% names(schedule_info), info = sprintf("Expected column '%s' not found in schedule_info", col))
  }
})
