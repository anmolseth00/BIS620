# Summary:
# This file contains a set of custom utility functions used in the Clinical Trials Query Shiny App. These functions facilitate data querying, filtering, and visualization.

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
# - These functions rely on R packages, including 'dplyr,' 'DT,' 'ggplot2,' 'leaflet,' and 'maps.'
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
library(leaflet)
library(maps)

# Create the connection to a database and "studies" and "sponsors" tables.

con = dbConnect(
  duckdb(
    file.path("..", "ctgov.duckdb"), # Anmol comment - need this line to run on my end
    #"ctgov.duckdb",
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
#' @param x the database table.
plot_conditions_histogram = function(x) {
  x_grouped <- x |> #julia edit- filtering out when a condition has at least 4 studies on it to minimize how many show up in histogram
    #will remove when decided on better solution: is there any other way we can bucket?! tried looking for a condition category but don't see one.
    #can we bucket on amount of studies per # of conditions? maybe not, question specifically says "showing the conditions"
    #maybe at least for the conditions that only have one study we can list those below the histogram somehow?
    group_by(condition_name) |>
    summarize(n=n())
  x <- left_join(x, x_grouped, by="condition_name") |>
    filter(n>3)

  # count_counditions <- conditions |>
  #   group_by(downcase_name) |>
  #   summarize(n=n()) |>
  #   arrange(desc(n)) |>
  #   collect()
  # count_counditions |>
  #   summarize(n=n())
  # count_counditions |>
  #   filter(n>1) |>
  #   summarize(n=n())

  ggplot(x, aes(x = condition_name)) +
    geom_bar(fill = "skyblue", color = "black") +
    xlab("Condition") +
    ylab("Count") +
    labs(title = "Clinical Trial Conditions Distribution",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Create a histogram of the countries that trials in a query are coming from
#' @param data the database table.
plot_countries_frequency = function(data) {
  ggplot(data, aes(x = country_name)) +
    geom_bar(fill = "skyblue", color = "black") +
    xlab("Country") +
    ylab("Frequency") +
    labs(title = "Frequency of Studies by Country",  # Add title
         caption = "Source: https://clinicaltrials.gov/") +  # Add caption
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
