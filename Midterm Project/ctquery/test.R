library(dplyr)
library(tidyr)
source("ct-util.R")

con = dbConnect(
  duckdb(
    "ctgov.duckdb",
    read_only = TRUE
  )
)

if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}

max_num_studies = 1000

studies = tbl(con, "studies")
sponsors = tbl(con, "sponsors")
interventions = tbl(con, "interventions")
countries = tbl(con, "countries")
country_counts <- table(countries$name)

conditions = tbl(con, "conditions")
conditions_counts <- head(conditions$name)

# Build data for get_studies() in app.R
# test keywords: non-small
# test sponsor: nih
# test status: completed
# test fda regulated: true

brief_title_kw = "non-small"
source = "NIH"
status = "Completed"
fda = TRUE

# 1. Filter data by Brief Title Keywords
if (brief_title_kw != "") {
  si = brief_title_kw |>
    strsplit(",") |>
    unlist() |>
    trimws()
  ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
} else {
  ret = studies
}

# 2. Filter data by Sponsor Type
if (!is.null(source)) {
  ret = ret |>
    filter(source_class %in% !!source)
}

# 3. Filter data by Study Status
if (!is.null(status)) {
  ret = ret |>
    filter(overall_status %in% !!status)
}

# 4. Filter data by FDA Regulated Drug
if ("TRUE" %in% fda) {
  ret <- ret %>%
    filter(is_fda_regulated_drug == TRUE)
}
if ("FALSE" %in% fda) {
  ret <- ret %>%
    filter(is_fda_regulated_drug == FALSE)
}

# LEFT JOIN conditions data into the studies data based on nct_id
ret = ret |>
  left_join(conditions |> rename(condition_name = name), by = "nct_id", copy = TRUE)

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

ret = ret |>
  head(max_num_studies) |>
  collect()

# Plug the data into each of the visuals
# 1. Phase histogram
 ret |>
   plot_phase_histogram()
 
 # plug into actual code
 # Define a fixed set of phases
 ret$phase[is.na(ret$phase)] = "NA"
 # Problem 1: Fix the phase histogram so that the x-axis values are uniform regardless of the query.
 fixed_phases <- c("Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3",
                   "Phase 3", "Phase 4", "Not Applicable", "NA")  # Include all possible phases
 
 # Count phase frequencies
 phase_counts <- table(factor(ret$phase, levels = fixed_phases))
 
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

# 2. Cumulative studies plot
 ret |>
   plot_concurrent_studies()
 
 # plug into actual code
  # Get all of the unique dates.
   all_dates = studies |>
     select(start_date, completion_date) |>
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
   all_dates
 
 ret |>
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

# 3. Conditions histogram
ret |>
  plot_conditions_histogram()

# plug into actual code
x_grouped <- ret |> #julia edit- filtering out when a condition has at least 4 studies on it to minimize how many show up in histogram
  #will remove when decided on better solution: is there any other way we can bucket?! tried looking for a condition category but don't see one.
  #can we bucket on amount of studies per # of conditions? maybe not, question specifically says "showing the conditions"
  #maybe at least for the conditions that only have one study we can list those below the histogram somehow?
  group_by(condition_name) |>
  summarize(n=n())
ret_used <- left_join(ret, x_grouped, by="condition_name", copy = TRUE)
  # filter(n>3) # this part does not work with the current test search parameters

ggplot(ret_used, aes(x = condition_name)) +
  geom_bar(fill = "skyblue", color = "black") +
  xlab("Condition") +
  ylab("Count") +
  labs(title = "Clinical Trial Condition Distribution",  # Add title
       caption = "Source: https://clinicaltrials.gov/") +  # Add caption
  #scale_x_discrete(labels = scales::wrap_format(width = 15)) +  # Wrap x-axis labels for better presentation
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = .5, hjust = 1))

# 4. Countries histogram
ret |>
  plot_countries_frequency()
# plug into actual code
ggplot(ret, aes(x = country_name)) +
  geom_bar(fill = "skyblue", color = "black") +
  xlab("Country") +
  ylab("Frequency") +
  labs(title = "Clinical Trial Country Distribution",  # Add title
       caption = "Source: https://clinicaltrials.gov/") +  # Add caption
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Interventions histogram
ret |>
  plot_interventions_histogram()

# plug into actual code
ggplot(ret, aes(x = intervention_type)) +
  geom_bar(fill = "skyblue", color = "black") +
  xlab("Country") +
  ylab("Frequency") +
  labs(title = "Clinical Trial Intervention Distribution",  # Add title
       caption = "Source: https://clinicaltrials.gov/") +  # Add caption
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))