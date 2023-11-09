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
  title <- plot$labels$title
  x <- plot$labels$x
  y <- plot$labels$y
  bar_color <- plot$layers[[1]]$aes_params$fill

  # Expect that the generated plot is a ggplot
  expect_is(plot, "ggplot")
  # Expect that the title of the plot corresponds correctly
  expect_equal(title, "Clinical Trial Phase Distribution")
  expect_equal(x, "Phase")
  expect_equal(y, "Count")
  expect_equal(bar_color, "skyblue")
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
  title <- plot$labels$title
  x <- plot$labels$x
  y <- plot$labels$y
  line_color <- plot$layers[[1]]$aes_params$colour
  
  # Expect that the generated plot is a ggplot
  expect_is(plot, "ggplot")
  # Expect that the title of the plot corresponds correctly
  expect_equal(title, "Concurrent Trials Over Time")
  expect_equal(x, "Date")
  expect_equal(y, "Count")
  expect_equal(line_color, "blue")
})

test_that("Conditions plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    condition_name = c("Non Small Cell Lung Carcinoma", "Non Small Cell Lung Carcinoma", "Non Small Cell Lung Carcinoma", "Non Small Cell Lung Carcinoma", "Pain")
  )
  # Generate the conditions plot
  plot <- plot_conditions_histogram(sample_data, 5)
  title <- plot$labels$title
  x <- plot$labels$x
  y <- plot$labels$y
  bar_color <- plot$layers[[1]]$aes_params$fill
  
  # Expect that the generated plot is a ggplot
  expect_is(plot, "ggplot")
  # Expect that the title of the plot corresponds correctly
  expect_equal(title, "Clinical Trial Condition Distribution")
  expect_equal(x, "Condition")
  expect_equal(y, "Count")
  expect_equal(bar_color, "skyblue")
})

test_that("Countries plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    country_name = c("United States", "Canada", "United States", "Puerto Rico", "United Kingdom")
  )
  # Generate the conditions plot
  plot <- plot_countries_frequency(sample_data, 5)
  # Store pieces of information
  title <- plot$labels$title
  x <- plot$labels$x
  y <- plot$labels$y
  bar_color <- plot$layers[[1]]$aes_params$fill
  
  # Expect that the generated plot is a ggplot
  expect_is(plot, "ggplot")
  # Expect that the labels of the plot correspond correctly
  expect_equal(title, "Clinical Trial Country Distribution")
  expect_equal(x, "Country")
  expect_equal(y, "Count")
  expect_equal(bar_color, "skyblue")
})

test_that("Interventions plot is generated correctly", {
  # Create a sample dataset for testing
  sample_data <- data.frame(
    nct_id = 1:5,
    intervention_type = c("Drug", "Drug", "Biological", "Procedure", "Drug")
  )
  # Generate the conditions plot
  plot <- plot_interventions_histogram(sample_data)
  # Store pieces of information
  title <- plot$labels$title
  x <- plot$labels$x
  y <- plot$labels$y
  bar_color <- plot$layers[[1]]$aes_params$fill
  
  # Expect that the generated plot is a ggplot
  expect_is(plot, "ggplot")
  # Expect that the title of the plot corresponds correctly
  expect_equal(title, "Clinical Trial Intervention Distribution")
  expect_equal(x, "Intervention")
  expect_equal(y, "Count")
  expect_equal(bar_color, "skyblue")
})