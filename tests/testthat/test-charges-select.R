library(dplyr)

small_test_charges <- tibble(
  Group = c("Group A", "Group A", "Group B", "Group C", "Group E", "Group E"),
  Service = c("3' GEX / 5' GEX", "3' GEX-HT / 5'GEX-HT", "BCR / TCR", 
            "TotalSeq Panel Titration", "Xenium Processing", "Bioinformatics Analysis"),
  Description = c("per run", "per run", "per run", "<10 Abs", "per run", "1 week"),
  Base_Price = c(4000, 5000, 1500, 2500, 3600, 7250)
)

small_test_application_protocol_proc <- tibble(
  Service = small_test_charges$Service,
  Application = list(
    list("Single Cell"),
    list("Single Cell"),
    list("ALL_APPLICATIONS"),
    list("Spatial Omics", "MiniBulk"),
    list("Spatial Omics"),
    list("MiniBulk")
  ),
  Protocol = list(
    list("3' Gene Expression", "5' Gene Expression", "FLEX"),
    list("3' Gene Expression", "5' Gene Expression", "FLEX"),
    list("ALL_PROTOCOLS"),
    list("Multiome", "scPRM-Seq", "MB PRM-Seq"),
    list("scPRM-Seq", "MB PRM-Seq"),
    list("Multiome")
  )
)

test_that("filter_services_data is valid on small input", {
  # Valid inputs, no filters check if dataframe should remain unchanged
  input <- list(filter_group = "All")
  values <- list(
    application_select = "All",
    protocol_select = "All",
    data = list(
      services = small_test_charges,
      application_protocol_proc = small_test_application_protocol_proc
    )
  )
  
  result <- filter_services_data(input, values)
  
  expect_equal(nrow(result), nrow(small_test_charges))
  expect_equal(result, small_test_charges)
})

test_that("filter_services_data filters by Group correctly", {
  # Results should be filtered by group only
  test_group <- "Group A"
  n_valid_lines <- 2
  
  input <- list(filter_group = test_group)
  values <- list(
    application_select = "All",
    protocol_select = "All",
    data = list(
      services = small_test_charges,
      application_protocol_proc = small_test_application_protocol_proc
    )
  )
  
  result <- filter_services_data(input, values)
  
  expect_true(all(result$Group == test_group))
  expect_equal(nrow(result), n_valid_lines)
})

test_that("filter_services_data filters by Application correctly", {
  # Results should be filtered by application only
  test_application <- "Spatial Omics"
  n_valid_lines <- 3
  
  input <- list(filter_group = "All")
  values <- list(
    application_select = test_application,
    protocol_select = "All",
    data = list(
      services = small_test_charges,
      application_protocol_proc = small_test_application_protocol_proc
    )
  )
  
  result <- filter_services_data(input, values)
  
  expect_equal(nrow(result), n_valid_lines)
})

test_that("filter_services_data filters by Protocol correctly", {
  # Results should be filtered by protocol only
  test_protocol <- "scPRM-Seq"
  n_valid_lines <- 3
  
  input <- list(filter_group = "All")
  values <- list(
    application_select = "All",
    protocol_select = test_protocol,
    data = list(
      services = small_test_charges,
      application_protocol_proc = small_test_application_protocol_proc
    )
  )
  
  result <- filter_services_data(input, values)
  
  expect_equal(nrow(result), n_valid_lines)
})