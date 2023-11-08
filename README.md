# BIS620
 Fall 2023

ctgov.duckdb located in 'Midterm Project', 1 folder above ctquery


# Title: Clinical Trials Query Shiny App and Utility Functions

# Summary:
# This R-based Shiny web application and utility functions serve as a powerful tool for querying, filtering, and visualizing clinical trials data. The app allows users to search for clinical trials by brief title keywords, filter by sponsor types, and examine trials related to FDA-regulated drugs. It provides various visualizations and the ability to export data in CSV format.

# Author: Julia Stiller and Anmol Seth
# Version: 1.0

# Organization of Files:
# - 'app.R': Contains the Shiny web application UI and server logic.
# - 'ct-util.R': Defines custom utility functions used in the app.
# - 'test.R': Tests the custom utility functions defined in ct-util.R.

# Usage:
# - Run the Shiny app by sourcing 'app.R'.
# - The app provides an intuitive interface for querying and visualizing clinical trials data.
# - Utilize the custom functions in 'ct-util.R' for querying and visualizing data programmatically.

# Dependencies:
# - This application relies on several R packages, including 'shiny,' 'dplyr,' 'DT,' 'ggplot2,' 'leaflet,' and 'maps.'
# - Ensure that the database connection is set up correctly, and the required data tables are available.

