library(testthat)
source("ct-util.R")

test_that("Phase plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    phase = c("Phase 1", "Phase 2", "Phase 2", "Phase 3", "Phase 1")
  )
  # Generate the phase plot
  plot <- plot_phase_histogram(sample_data)

  # Add expectations to check the plot
  expect_is(plot, "ggplot")
  # Add more expectations based on the expected plot characteristics
})

test_that("Concurrent studies plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    start_date = c("2020-09-08", "2008-08-31", "2017-05-01", "2007-09-30", "2006-07-31"),
    completion_date = c("2025-12-31", "2009-09-30", "2024-03-30", "2010-12-31", "2015-12-31")
  )
  # Generate the concurrent studies plot
  plot <- plot_concurrent_studies(sample_data)
  
  # Add expectations to check the plot
  expect_is(plot, "ggplot")
  # Add more expectations based on the expected plot characteristics
})

test_that("Conditions plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    condition_name = c("Non Small Cell Lung Carcinoma", "Non Small Cell Lung Carcinoma", "Non Small Cell Lung Carcinoma", "Non Small Cell Lung Carcinoma", "Pain")
  )
  # Generate the conditions plot
  plot <- plot_conditions_histogram(sample_data)
  
  # Add expectations to check the plot
  expect_is(plot, "ggplot")
  # Add more expectations based on the expected plot characteristics
})

test_that("Countries plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    country_name = c("United States", "Canada", "United States", "Puerto Rico", "United Kingdom")
  )
  # Generate the conditions plot
  plot <- plot_countries_frequency(sample_data)
  
  # Add expectations to check the plot
  expect_is(plot, "ggplot")
  # Add more expectations based on the expected plot characteristics
})

test_that("Interventions plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    intervention_type = c("Drug", "Drug", "Biological", "Procedure", "Drug")
  )
  # Generate the conditions plot
  plot <- plot_interventions_histogram(sample_data)
  
  # Add expectations to check the plot
  expect_is(plot, "ggplot")
  # Add more expectations based on the expected plot characteristics
})