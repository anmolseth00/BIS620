within_date = function(date, starts, ends) {
  date >= starts & date <= ends
}

within_date_d = 
  partial(within_date, starts = d$start_date, ends = d$completion_date)


within_date(all_dates$date[1], d$start_date, d$completion_date)

library(purrr)

within_date_d = 
  partial(within_date, starts = d$start_date, ends = d$completion_date)

within_date_d(all_dates$date[1]) |> sum(na.rm = TRUE)

wdds = compose(partial(sum, na.rm = TRUE), within_date_d)

# sum(within_date_d(date), na.rm = TRUE)

wdds(all_dates$date[1])

wddsp = . %>% 
  within_date(starts = d$start_date, ends = d$completion_date) %>%
  sum(na.rm = TRUE)

all_dates$count = map_dbl(all_dates$date, wdds)
