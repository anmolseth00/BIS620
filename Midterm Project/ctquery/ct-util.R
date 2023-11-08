# Summary:
# This file contains a set of custom utility functions used in the Clinical Trials Query Shiny App. 
# These functions facilitate data querying, filtering, and visualization.

# Functions:
# - query_kwds: Queries keywords in a database table and returns filtered results.
# - plot_phase_histogram: Creates a histogram of clinical trial phases.
# - get_concurrent_trials: Computes the number of concurrent trials at different dates.
# - plot_concurrent_studies: Generates a plot of concurrent trials over time.
# - plot_conditions_histogram: Produces a histogram of conditions examined in trials.
# - plot_countries_frequency: Creates a histogram of study frequencies by country.

# Usage:
# - Import these functions to enhance data analysis and visualization.
# - Functions in this file are integral to the 'app.R' Shiny application for clinical trials data exploration.

# Dependencies:
# - These functions rely on R packages, including 'dplyr' 'DT' 'ggplot2'
# - Ensure that the 'app.R' file has access to these functions.

# Notes:
# - Detailed function documentation is available within each function definition.

library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
library(knitr)

# Create the connection to a database and "studies" and "sponsors" tables.
con = dbConnect(
  duckdb(
    file.path("..", "ctgov.duckdb"),
    # "ctgov.duckdb",
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
conditions = tbl(con, "conditions")
countries = tbl(con, "countries")
interventions = tbl(con, "interventions")

#' @title Query keywords from a database table.
#' @description Description goes here.
#' @param d the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords
#' (intersection) or any of the keywords (union)? (default FALSE; union).
query_kwds <- function(d, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(d, sql(query))
}

#' Create a histogram of the phases returned by a brief title keyword search
#' @param x the database table.
plot_phase_histogram = function(x) {
  # Define a fixed set of phases
  x$phase[is.na(x$phase)] = "NA"
  # Problem 1: Fix the phase histogram so that the x-axis values are uniform regardless of the query.
  fixed_phases <- c("Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3",
                    "Phase 3", "Phase 4", "Not Applicable", "NA")  # Include all possible phases

  # Count phase frequencies
  phase_counts <- table(factor(x$phase, levels = fixed_phases))

  # Create a data frame with the fixed phases and their counts
  phase_data <- data.frame(Phase = names(phase_counts), Count = as.numeric(phase_counts))

  # Order phases and create labels
  phase_data$Phase <- factor(phase_data$Phase, levels = fixed_phases)

  # Create the phase histogram
  ggplot(phase_data, aes(x = Phase, y = Count)) +
    geom_col(fill = "skyblue", color = "black") +  # Customize fill and border colors
    xlab("Phase") +
    ylab("Count") +
    labs(title = "Clinical Trial Phase Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    scale_x_discrete(labels = scales::wrap_format(width = 10)) +  # Wrap x-axis labels for better presentation
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
}


#' Get the number of concurrent trials for each date in a set of studies
#' @param d the studies to get the number of concurrent trials for.
#' @return A tibble with a `date` column and a `count` of the number of
#' concurrent trials at that date.
get_concurrent_trials = function(d) {
  # Get all of the unique dates.
  all_dates = d |>
    pivot_longer(cols = everything()) |>
    select(-name) |>
    distinct() |>
    arrange(value) |>
    na.omit() |>
    rename(date = value)

  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }

  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count =
    map_dbl(
      all_dates$date,
      ~ .x |>
        within_date(d$start_date, d$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}

#' Create a plot of the concurrent studies in the query
#' @param studies the database table.
plot_concurrent_studies = function(studies) {
  studies |>
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
}

#' Create a histogram of the conditions that trials in a query are examining
#' @param data the database table.
#' @param num_top_conditions the number conditions the user wants to visualize
plot_conditions_histogram = function(data, num_top_conditions) {
  # Find the top n most common conditions
  x_grouped <- data |>
    group_by(condition_name) |>
    summarize(n=n()) |>
    arrange(desc(n))
  
  top_conditions <- x_grouped$condition_name |>
    head(num_top_conditions)
  
  # Create a new column that determines whether the study is in one of those top n conditions or not
  x_grouped$condition_group <- ifelse(x_grouped$condition_name %in% top_conditions, 
                                      x_grouped$condition_name, 
                                      "Other")
  
  # Define a fixed set of conditions
  fixed_conditions <- append(top_conditions, "Other")
  
  # Count condition frequencies
  condition_counts <- x_grouped |>
    group_by(condition_group)  |>
    summarize(total= sum(n))
  #table(factor(ret$condition_group, levels = fixed_conditions))
  
  # Create a data frame with the fixed conditions and their counts
  condition_data <- data.frame(Condition = condition_counts$condition_group, Count = as.numeric(condition_counts$total))
  
  # Order conditions and create labels
  condition_data$Condition <- factor(condition_data$Condition, levels = fixed_conditions)
  
  # Plot
  ggplot(condition_data, aes(x = Condition, y = Count)) +
    geom_col(fill = "skyblue", color = "black") +
    xlab("Condition") +
    ylab("Count") +
    labs(title = "Clinical Trial Condition Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
    scale_y_log10() + # Scale y to better see smaller buckets
    theme_minimal() + # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
}

#' Create a histogram of the countries that trials in a query are coming from
#' @param data the database table.
#' @param num_top_countries the number countries the user wants to visualize
plot_countries_frequency = function(data, num_top_countries) {
  # Find the top n most common countries
  country_grouped <- data |>
    group_by(country_name) |>
    summarize(n = n()) |>
    arrange(desc(n))
  
  top_countries <- country_grouped$country_name |>
    head(num_top_countries)
  
  # Create a new column that determines whether the study is in one of those top n countries or not
  country_grouped$country_group <- ifelse(country_grouped$country_name %in% top_countries, 
                                          country_grouped$country_name, 
                                          "Other")
  
  # Define a fixed set of countries
  fixed_countries <- append(top_countries, "Other")
  
  # Count country frequencies
  country_counts <- country_grouped |>
    group_by(country_group)  |>
    summarize(total = sum(n))
  
  # Create a data frame with the fixed countries and their counts
  country_data <- data.frame(Country = country_counts$country_group, Count = as.numeric(country_counts$total))
  
  # Order countries and create labels
  country_data$Country <- factor(country_data$Country, levels = fixed_countries)
  
  # Plot
  ggplot(country_data, aes(x = Country, y = Count)) +
    geom_col(fill = "skyblue", color = "black") +
    xlab("Country") +
    ylab("Count") +
    labs(title = "Clinical Trial Country Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
    # scale_y_log10() + # Scale y to better see smaller buckets
    theme_minimal() + # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
}


#' Create a histogram of the intervention types that trials in a query are coming from
#' @param data the database table.
plot_interventions_histogram = function(data) {
  ggplot(data, aes(x = intervention_type)) +
    geom_bar(fill = "skyblue", color = "black") +
    xlab("Country") +
    ylab("Frequency") +
    labs(title = "Clinical Trial Intervention Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    theme_minimal() + # Use a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
}
