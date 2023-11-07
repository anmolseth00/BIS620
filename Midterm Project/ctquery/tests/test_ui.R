library(testthat)
library(shiny)
library(shinytest)

# Load your Shiny app
app <- shiny::shinyApp(
  ui = fluidPage(titlePanel("Clinical Trials Query")),
  server = function(input, output) { }
)

test_that("UI contains the title", {
  session <- shinytest::shinytest_session(app)
  shinytest::session_evaluate(session, {
    expect_true("Clinical Trials Query" %in% document$body$textContent)
  })
})
