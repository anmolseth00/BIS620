library(testthat)
library(dplyr)
source("ct-util.R")

test_that("Data processing function works", {
  # Generate sample data
  sample_data <- data.frame(
    nct_id = 1:5,
    brief_title = c("Trial 1", "Trial 2", "Trial 3", "Trial 4", "Trial 5"),
    source_class = c("FED", "INDIV", "INDUSTRY", "NETWORK", "NIH"),
    is_fda_regulated_drug = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  # Test the data processing function
  processed_data <- get_studies(sample_data)

  # Add expectations to check the processed data
  expect_is(processed_data, "data.frame")
  expect_equal(nrow(processed_data), 5)
  # Add more expectations based on the data processing logic
})
