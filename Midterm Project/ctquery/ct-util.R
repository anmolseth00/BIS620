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
    #file.path("..", "ctrialsgov.duckdb"),
    file.path("..", "ctgov.duckdb"),
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

# Create a histogram of the phases returned by a brief title keyword search
# @param d the database table.
# @param brief_title_kw the brief title keywords to look for. This is optional.
plot_phase_histogram = function(x) {
  # Define a fixed set of phases
  x$phase[is.na(x$phase)] = "NA"
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

plot_conditions_histogram = function(x) {
  ggplot(x, aes(x = condition_name)) +
    geom_bar() +
    xlab("Condition") +
    ylab("Count") +
    theme_bw()
}

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

plot_concurrent_studies = function(studies) {
  plot(mtcars$mpg, mtcars$cyl)
}
