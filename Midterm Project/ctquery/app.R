#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
      # Problem 3: Add a drop-down so that queries can be subsetted on sponsor type.
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
         tabPanel("Conditions",
                  #div(class = "scrollable-tab", style='overflow-x: scroll'), #trying to see if I can add a horizontal scroll but doesn't seem to be working
                      plotOutput("conditions_plot")),
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
      left_join(conditions |> rename(condition_name = downcase_name), by = "nct_id") #julia edit- trying downcase name, will revert to "name" column when done

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
