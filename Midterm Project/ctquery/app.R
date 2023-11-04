#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# 0. Midterm scheduling

# 1. Clean up the table column names X
# 2. Allow multiple brief title keywords X
# 3. Create a histogram of the phase
# 4. Organize files.
# 5. Fix the Phase plot
# 6. Plot the concurrent studies (adding a feature/capability).

# Steps to adding a feature:
# 1. Specify the feature.
#   - What does it do?
#   - What will be shown?
# 2. Specify the interface
#   - Where should it go?
#   - Does it enhance the user experience?
# 3. Implement the feature without the UI
# 4. Integrate the feature.

source("ct-util.R")
max_num_studies = 1000

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      # Download queried data
      downloadButton("download_csv", "Download CSV"),

      textInput("brief_title_kw", "Brief title keywords"),
      # checkboxGroupInput("source_class",
      #                    label = h3("Sponsor Type"),
      #                    choices = list("Federal" = "FED",
      #                                   "Individual" = "INDIV",
      #                                   "Industry" = "INDUSTRY",
      #                                   "Network" = "NETWORK",
      #                                   "NIH" = "NIH",
      #                                   "Other" = "OTHER",
      #                                   "Other gov" = "OTHER_GOV",
      #                                   "Unknown" = "Unknown")),
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
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
         type = "tabs",
         tabPanel("Phase", plotOutput("phase_plot")),
         tabPanel("Concurrent", plotOutput("concurrent_plot")),
         tabPanel("Conditions", plotOutput("conditions_plot")),
         tabPanel("Countries", plotOutput("countries_plot")),
       ),
      dataTableOutput("trial_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
      left_join(conditions |> rename(condition_name = name), by = "nct_id")

    # # We will not include countries that have been removed
    filtered_countries <- countries %>%
      filter(!removed) %>%
      rename(country_name = name)
    #
    # # LEFT JOIN filtered countries data into the studies data based on nct_id
    ret = ret |>
      left_join(filtered_countries, by = "nct_id")

    ret |>
      head(max_num_studies) |>
      collect()
  })

  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })

  output$concurrent_plot = renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
      geom_line(color = "blue") +  # Customize line color
      xlab("Date") +
      ylab("Count") +
      labs(title = "Concurrent Trials Over Time",  # Add title
           caption = "Source: https://clinicaltrials.gov/") +  # Add caption
      theme_minimal() +  # Use a minimal theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  })

  output$conditions_plot = renderPlot({
    get_studies() |>
      plot_conditions_histogram()
  })

  output$countries_plot = renderPlot({
    get_studies() |>
      plot_countries_frequency()
      #plot_countries_frequency_map()
  })

  output$trial_table = renderDataTable({
    get_studies() |>
      head(max_num_studies) |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })

  # Define the server logic for downloading data as CSV
  # In the server logic
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
