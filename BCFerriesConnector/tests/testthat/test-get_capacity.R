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

library(testthat)

# 测试 get_capacity 函数
test_that("get_capacity() returns a data frame with expected columns for a specific routeCode", {
  # 直接调用 get_capacity() 函数并传入一个示例路线代码
  # 这里使用的路线代码需要确保是API可以返回数据的一个有效值
  # 对于这个测试案例，选择一个已知的、稳定的路线代码，例如"TSASWB"，或者根据需要调整
  test_routeCode <- "TSASWB"  # 假设这是一个有效且API有数据的路线代码
  capacity_info <- tryCatch({
    get_capacity(test_routeCode)
  }, error = function(e) {
    NULL  # 如果发生错误，返回NULL以避免测试失败
  })
  
  # 检查是否成功返回数据框
  expect_true(!is.null(capacity_info), info = "get_capacity should return a data frame")
  
  # 如果返回的不是NULL，则继续检查结构
  if (!is.null(capacity_info)) {
    expected_columns <- c("routeCode", "time", "sailingStatus", "vesselName", "fill", "carFill", "oversizeFill")
    expect_true(all(expected_columns %in% names(capacity_info)), info = "Data frame should contain all expected columns")
  }
})



