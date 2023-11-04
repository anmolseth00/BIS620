library(dplyr)
library(tidyr)
source("ct-util.R")

plot_phase_histogram(studies)

# cumulative studies
d = studies |>
  query_kwds("pembrolizumab", "brief_title") |>
  select(start_date, completion_date) |>
  collect() |>
  get_concurrent_trials() |>
  ggplot(aes(x = date, y = count)) +
    geom_line() +
    xlab("Date") +
    ylab("Count")

con = dbConnect(
  duckdb(
    file.path("..", "ctgov.duckdb"),
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}
studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
countries = tbl(con, "countries") |> collect()
country_counts <- table(countries$name)


conditions = tbl(con, "conditions") |> collect()
conditions_counts <- head(conditions$name)

test <- conditions |> collect() |> head(1000)
test
#LEFT JOIN on nct_id between studies and conditions (join conditions into studies)
