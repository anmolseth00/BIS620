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
