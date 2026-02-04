library(dplyr)

small_test_items <- tibble(
  Product_Code = c("X0001", "X0002", "V0075", "H0209", "K1008"),
  Brand = c("Xenium", "KBrand", "KBrand", "GEM-X", "Next GEM"),
  Item = c("Xenium Kit", "Barcode Kit v3", "Library Construction Kit", 
           "Chromium GEM-X Single Cell 5' Kit v3", "5' CRISPR Kit"),
  Category = c("Library & Gel Bead Kit", "Library & Gel Bead Kit", "Additional Library Kit",
               "Library & Gel Bead Kit", "Additional Library Kit"),
  Description = c("per kit", "per kit", "per kit", "per kit", "per kit"),
  Base_Cost = c(1550, 2250, 650, 2400, 7000),
  Add_Cost = c(0, 0, 45, 0, 200),
  Is_Constant = c(TRUE, FALSE, FALSE, FALSE, FALSE)
)

small_test_application_protocol_items <- tibble(
  Brand = c("Xenium", "KBrand", "KBrand", "GEM-X", "Next GEM"), 
  Item = c("Xenium Kit", "Barcode Kit v3", "Library Construction Kit", 
           "Chromium GEM-X Single Cell 5' Kit v3", "5' CRISPR Kit"), 
  Application = c(list("Xenium"), 
                  list("ALL_APPLICATIONS"), 
                  list("ALL_APPLICATIONS"), 
                  list("Single Cell"), 
                  list("MiniBulk")), 
  Protocol = list(list("Xenium"), 
                  list("ALL_PROTOCOLS"), 
                  list("MB PRM-Seq", "Merscope"),
                  list("3' Gene Expression", "5' Gene Expression"),
                  list("ALL_PROTOCOLS"))
)

test_that("filter_items_data is valid on small input", {
  input <- list(filter_item_brand = "All", filter_category = "All")
  values <- list(
    application_select = "All",
    protocol_select = "All",
    data = list(
      items = small_test_items,
      application_protocol_item = small_test_application_protocol_items
    )
  )
  
  result <- filter_items_data(input, values)
  
  expect_equal(nrow(result), nrow(small_test_items))
  expect_equal(result, small_test_items)
})

test_that("filter_items_data filters by Brand correctly", {
  input <- list(filter_item_brand = "KBrand", filter_category = "All")
  values <- list(
    application_select = "All",
    protocol_select = "All",
    data = list(
      items = small_test_items,
      application_protocol_item = small_test_application_protocol_items
    )
  )
  
  result <- filter_items_data(input, values)
  n_valid_lines <- 2
  
  expect_true(all(result$Brand == "KBrand"))
  expect_equal(nrow(result), n_valid_lines)
})

test_that("filter_items_data filters by Category correctly", {
  input <- list(filter_item_brand = "All", filter_category = "Library & Gel Bead Kit")
  values <- list(
    application_select = "All",
    protocol_select = "All",
    data = list(
      items = small_test_items,
      application_protocol_item = small_test_application_protocol_items
    )
  )
  
  result <- filter_items_data(input, values)
  n_valid_lines <- 3
  
  expect_true(all(result$Category == "Library & Gel Bead Kit"))
  expect_equal(nrow(result), n_valid_lines)
})

test_that("filter_items_data filters by Application correctly", {
  input <- list(filter_item_brand = "All", filter_category = "All")
  values <- list(
    application_select = "Single Cell",
    protocol_select = "All",
    data = list(
      items = small_test_items,
      application_protocol_item = small_test_application_protocol_items
    )
  )
  
  result <- filter_items_data(input, values)
  n_valid_lines <- 3
  
  expect_equal(nrow(result), n_valid_lines)
})

test_that("filter_items_data filters by Protocol correctly", {
  input <- list(filter_item_brand = "All", filter_category = "All")
  values <- list(
    application_select = "All",
    protocol_select = "3' Gene Expression",
    data = list(
      items = small_test_items,
      application_protocol_item = small_test_application_protocol_items
    )
  )
  
  result <- filter_items_data(input, values)
  n_valid_lines <- 3
  
  expect_equal(nrow(result), n_valid_lines)
})