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
# - Requires R packages such as 'shiny' 'dplyr' 'DT' 'ggplot2'
# - Ensure the 'ct-util.R' file is in the same directory for utility function access.

# Notes:
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

      # 1. Text input for entering keywords related to brief titles
      textInput("brief_title_kw", label = h4("Brief Title Keywords")),

      # 2. Drop down input for selecting sponsor types
      selectizeInput("source_class",
                     label = h4("Sponsor Type"),
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
                       dropdown = TRUE)
                     ),
      
      # 3. Dropdown input for filtering on status
      selectizeInput("overall_status", label = h4("Study Status"), 
                     choices = list("Unknown" = "Unknown status",
                                    "Completed" = "Completed",
                                    "Withdrawn" = "Withdrawn",
                                    "Recruiting" = "Recruiting",
                                    "Terminated" = "Terminated",
                                    "Active, not recruiting" = "Active, not recruiting",
                                    "Suspended" = "Suspended",
                                    "Enrolling by invitation" = "Enrolling by invitation",
                                    "Not yet recruiting" = "Not yet recruiting",
                                    "Withheld" = "Withheld",
                                    "No longer available" = "No longer available",
                                    "Approved for marketing" = "Approved for marketing",
                                    "Available" = "Available",
                                    "Temporarily not available" = "Temporarily not available"),
                     multiple = TRUE,
                     options = list(
                       placeholder = 'Select status',
                       style = 'btn-primary',
                       dropdown = TRUE)
                     ),

      # 4. Check box input for filtering FDA regulated drugs
      checkboxGroupInput("is_fda_filter",
                         label = h4("FDA Regulated Drug"),
                         choices = c("Yes" = "TRUE", "No" = "FALSE")
      ),
      
      # 5. Check box input for filtering FDA regulated devices
      checkboxGroupInput("is_fda_device_filter",
                         label = h4("FDA Regulated Device"),
                         choices = c("Yes" = "TRUE", "No" = "FALSE")
      ),
      
      # 6. Button to download queried data
      downloadButton("download_csv", "Download CSV")
    ),

    # Main panel with tabs for data visualization
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot")),
        tabPanel("Conditions", plotOutput("conditions_plot"),
                 div(
                   numericInput("num_top_conditions", 
                                "Number of Top Conditions", 
                                value = 5, 
                                min = 1, 
                                max = 10)
                 )
                 ),
        tabPanel("Countries", plotOutput("countries_plot"),
                 div(
                   numericInput("num_top_countries", 
                                "Number of Top Countries", 
                                value = 5, 
                                min = 1, 
                                max = 10)
                 )
                 ),
        tabPanel("Interventions", plotOutput("interventions_plot"))
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
    
    # 1. Filter data by Brief Title Keywords
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
    
    # 2. Filter data by Sponsor Type
    if (!is.null(input$source_class)) {
      ret = ret |>
        filter(source_class %in% !!input$source_class)
    }
    
    # 3. Filter data by Study Status
    if (!is.null(input$overall_status)) {
      ret = ret |>
        filter(overall_status %in% !!input$overall_status)
    }
    
    # 4. Filter data by FDA Regulated Drug
    if ("TRUE" %in% input$is_fda_filter) {
      ret <- ret %>%
        filter(is_fda_regulated_drug == TRUE)
    }
    if ("FALSE" %in% input$is_fda_filter) {
      ret <- ret %>%
        filter(is_fda_regulated_drug == FALSE)
    }
    
    # 5. Filter data by FDA Regulated Device
    if ("TRUE" %in% input$is_fda_device_filter) {
      ret <- ret %>%
        filter(is_fda_regulated_device == TRUE)
    }
    if ("FALSE" %in% input$is_fda_device_filter) {
      ret <- ret %>%
        filter(is_fda_regulated_device == FALSE)
    }

    # LEFT JOIN conditions data into the studies data based on nct_id
    ret = ret |>
      left_join(conditions |> rename(condition_name = name), by = "nct_id")

    # We will not include countries that have been removed
    filtered_countries <- countries %>%
      filter(!removed) %>%
      rename(country_name = name)

    # LEFT JOIN filtered countries data into the studies data based on nct_id
    ret = ret |>
      left_join(filtered_countries, by = "nct_id")
    
    # LEFT JOIN intervention type data into studies data based on nct_id
    ret = ret |>
      left_join(interventions, by="nct_id")

    ret |>
      head(max_num_studies) |>
      collect()
  })

  # 1. Phase histogram
  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })

  # 2. Concurrent studies plot
  output$concurrent_plot = renderPlot({
    get_studies() |>
      plot_concurrent_studies()
  })

  # 3. Conditions histogram
  # Problem 2: Add a new tab that gives a histogram showing the conditions that trials in a query are examining.
  output$conditions_plot = renderPlot({
    get_studies() |>
      plot_conditions_histogram(input$num_top_conditions)
  })

  # 4. Countries histogram
  output$countries_plot = renderPlot({
    get_studies() |>
      plot_countries_frequency(input$num_top_countries)
  })
  
  # 5. Interventions histogram
  output$interventions_plot = renderPlot({
    get_studies() |>
      plot_interventions_histogram()
  })

  # Output a clean table of results of query
  output$trial_table = renderDataTable({
    get_studies() |>
      head(max_num_studies) |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, 
             `Brief Title` = brief_title,
             `Start Date` = start_date, 
             `Completion Date` = completion_date)
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
