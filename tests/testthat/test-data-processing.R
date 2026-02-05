library(testthat)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)

test_that("process_items_data works on regular data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=1, col_names = TRUE)
  
  result <- process_items_data(raw_input)
  columns <- c("Product_Code", "Brand", "Item", "Category", "Description", "Base_Cost", "Add_Cost", "Is_Constant")
  
  # Validate structure
  expect_equal(nrow(result), nrow(raw_input))
  expect_equal(colnames(result), columns)
  
  # Validate columns
  expect_false(anyNA(result$Brand))
  expect_false(anyNA(result$Item))
  expect_true(all(result$Base_Cost > 0))
  expect_true(all(result$Add_Cost >= 0))
  expect_true(all((result$Is_Constant == TRUE | (result$Is_Constant == FALSE))))
})

test_that("process_items_data works on empty data", {
  path <- test_path("testdata/empty-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=1, col_names = TRUE)
  expect_true(nrow(raw_input) == 0)
  
  result <- process_items_data(raw_input)
  columns <- c("Product_Code", "Brand", "Item", "Category", "Description", "Base_Cost", "Add_Cost", "Is_Constant")
  
  # Validate structure
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), columns)
})

test_that("process_items_data works on broken data", {
  path <- test_path("testdata/errors-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=1, col_names = TRUE)
  
  result <- process_items_data(raw_input)
  columns <- c("Product_Code", "Brand", "Item", "Category", "Description", "Base_Cost", "Add_Cost", "Is_Constant")
  
  # Validate structure
  expect_equal(nrow(result), 1)
  expect_equal(colnames(result), columns)
})

test_that("process_services_data works on regular data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=2, col_names = TRUE)
  
  result <- process_services_data(raw_input)
  columns <- c("Group", "Service", "Description", "Base_Price")
  
  # Validate structure
  expect_equal(nrow(result), nrow(raw_input))
  expect_equal(colnames(result), columns)
  
  expect_false(anyNA(result$Base_Price))
  expect_true(all(result$Base_Price >= 0))
})

test_that("process_services_data works on empty data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=2, col_names = TRUE)
  
  result <- process_services_data(raw_input)
  columns <- c("Group", "Service", "Description", "Base_Price")
  
  # Validate structure
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), columns)
})

test_that("process_services_data works on broken data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=2, col_names = TRUE)
  
  result <- process_services_data(raw_input)
  columns <- c("Group", "Service", "Description", "Base_Price")
  
  # Validate structure
  expect_equal(nrow(result), nrow(raw_input))
  expect_equal(colnames(result), columns)
  
  expect_false(anyNA(result$Base_Price))
  expect_true(all(result$Base_Price >= 0))
})

test_that("process_items_surcharges works on regular data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=3, col_names = TRUE)
  
  result <- process_items_surcharges(raw_input)
  
  expect_true(all(result >= 0))
})

test_that("process_items_surcharges works on empty data", {
  path <- test_path("testdata/empty-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=3, col_names = TRUE)
  
  result <- process_items_surcharges(raw_input)
  
  expect_true(all(result >= 0))
  expect_equal(length(result), 0)
})

test_that("process_services_surcharges works on regular data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=3, col_names = TRUE)
  
  result <- process_services_surcharges(raw_input)
  
  expect_true(all(result >= 0))
})

test_that("process_services_surcharges works on empty data", {
  path <- test_path("testdata/empty-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=3, col_names = TRUE)
  
  result <- process_services_surcharges(raw_input)
  
  expect_true(all(result >= 0))
  expect_equal(length(result), 0)
})

test_that("process_discounts_data works on regular data", {
  path <- test_path("testdata/empty-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=4, col_names = TRUE)
  
  result <- process_discounts_data(raw_input)
  columns <- c("Supplier", "Label", "Amount", "Type", "Display_Text")
  
  expect_equal(nrow(result), nrow(raw_input))
  expect_equal(colnames(result), columns)
  
  expect_true(all(result$Amount >= 0))
})

test_that("get_item_application_protocol works on regular data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=1, col_names = TRUE)
  
  result <- get_item_application_protocol(raw_input)
  columns <- c("Brand", "Item", "Application", "Protocol")
  
  expect_equal(colnames(result), columns)
  expect_true(all(length(result$Application) >= 1))
  expect_true(all(length(result$Protocol) >= 1))
})

test_that("get_service_application_protocol works on regular data", {
  path <- test_path("testdata/sample-spreadsheet.xlsx")
  raw_input <- read_excel(path, sheet=2, col_names = TRUE)
  
  result <- get_service_application_protocol(raw_input)
  columns <- c("Service", "Application", "Protocol")
  
  expect_equal(colnames(result), columns)
  expect_true(all(length(result$Application) >= 1))
  expect_true(all(length(result$Protocol) >= 1))
})