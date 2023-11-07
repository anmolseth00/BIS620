# Summary:
# This file defines the Shiny web application for querying, filtering, and visualizing clinical trials data. It features a user-friendly interface with various input options, tab panels for data visualization, and data export capabilities.

# Sections:
# - UI Definition: Defines the user interface layout and input elements.
# - Server Logic: Implements the server-side functionality, including data retrieval and visualization.
# - Data Export: Provides the ability to download queried data as a CSV file.

# Usage:
# - Source this file to run the Shiny app.
# - Interact with the UI to query and explore clinical trials data.
# - Utilize the 'server' logic to customize and extend app features.

# Dependencies:
# - Requires R packages such as 'shiny,' 'dplyr,' 'DT,' 'ggplot2,' 'leaflet,' and 'maps.'
# - Ensure the 'ct-util.R' file is in the same directory for utility function access.

# Notes:
# - The app's modular structure allows for easy feature expansion.
# - Review the server logic and utility functions in 'ct-util.R' for a comprehensive understanding.

# Load the Shiny library to enable web application development
library(shiny)

# Source custom utility functions from 'ct-util.R' for data manipulation and visualization
source("ct-util.R")

# Define the maximum number of studies to display within the app
max_num_studies = 1000

# Define the UI layout for the Clinical Trials Query Shiny App
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(

      # Button to download queried data
      downloadButton("download_csv", "Download CSV"),

      # Text input for entering keywords related to brief titles
      textInput("brief_title_kw", "Brief title keywords"),

      # Dropdown input for selecting sponsor types
      selectizeInput("source_class",
                     label = h3("Sponsor Type"),
                     choices = list(
                       "Federal" = "FED",
                       "Individual" = "INDIV",
                       "Industry" = "INDUSTRY",
                       "Network" = "NETWORK",
                       "NIH" = "NIH",
                       "Other" = "OTHER",
                       "Other gov" = "OTHER_GOV",
                       "Unknown" = "Unknown"
                     ),
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Select sponsor types',
                       style = 'btn-primary',
                       dropdown = TRUE
                     )
      ),

      # Checkbox input for filtering FDA regulated drugs
      checkboxGroupInput("is_fda_filter",
                         label = "FDA Regulated Drug",
                         choices = c("TRUE" = "TRUE", "FALSE" = "FALSE")
      )
    ),

    # Main panel with tabs for data visualization
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot")),
        tabPanel("Conditions",
                 plotOutput("conditions_plot"),
                 HTML("<p style='text-align: center; color: gray;'>Note: A keyword must be entered, or sponsor selected to render the plot.</p>"),
                 HTML("<p style='text-align: center; color: gray;'>[TEMPORARY] Dev note: Currently only showing conditions which appear >3 times.</p>")
        ),
        tabPanel("Countries", plotOutput("countries_plot")),
      ),
      # Data table to display query results
      dataTableOutput("trial_table")
    )
  )
)

# Define the server logic for the Clinical Trials Query Shiny App
server <- function(input, output) {

  # Define a reactive function to retrieve and process studies data
  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
    if (!is.null(input$source_class)) {
      ret = ret |>
        filter(source_class %in% !!input$source_class)
    }

    # LEFT JOIN conditions data into the studies data based on nct_id
    ret = ret |>
      left_join(conditions |> rename(condition_name = downcase_name), by = "nct_id") #julia edit- trying downcase name, will revert to "name" column when done

    # Check the selected options in the checkbox input
    if ("TRUE" %in% input$is_fda_filter) {
      ret <- ret %>%
        filter(is_fda_regulated_drug == TRUE)
    }

    if ("FALSE" %in% input$is_fda_filter) {
      ret <- ret %>%
        filter(is_fda_regulated_drug == FALSE)
    }

    # We will not include countries that have been removed
    filtered_countries <- countries %>%
      filter(!removed) %>%
      rename(country_name = name)

    # LEFT JOIN filtered countries data into the studies data based on nct_id
    ret = ret |>
      left_join(filtered_countries, by = "nct_id")

    ret |>
      head(max_num_studies) |>
      collect()
  })

  # Phase histogram
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })

  # Concurrent studies plot
  output$concurrent_plot = renderPlot({
    get_studies() |>
      plot_concurrent_studies()
  })

  # Conditions histogram
  # Problem 2: Add a new tab that gives a histogram showing the conditions that trials in a query are examining.
  output$conditions_plot = renderPlot({
    if (input$brief_title_kw != "" || !is.null(input$source_class)) { # julia edit - histogram will only render if there is a search keyword or sponsor type selected
      get_studies() |>
        plot_conditions_histogram()
    } else {
      print("Enter a keyword or select a sponsor to render plot")
    }
  })

  # Countries histogram
  output$countries_plot = renderPlot({
    get_studies() |>
      plot_countries_frequency()
    #plot_countries_frequency_map()
  })

  # Output a clean table of results of query
  output$trial_table = renderDataTable({
    get_studies() |>
      head(max_num_studies) |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })

  # Define the server logic for downloading data as CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      # Generate a dynamic file name with a description of the query
      query_description <- paste("query_results", input$brief_title_kw, ".csv", sep = "_")
      return(query_description)
    },
    content = function(file) {
      data <- get_studies()
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
