
#' goal: clean a messy epidemiological data frame
#' 
#' task: 
#' - identify how the input changes running step by step

library(tidyverse)

dat <- readr::read_csv(
  "https://epiverse-trace.github.io/tutorials-early/data/simulated_ebola_2.csv"
)

dat_dictionary <- tibble::tribble(
  ~options,  ~values,     ~grp, ~orders,
  "1",   "male", "gender",      1L, 
  "2", "female", "gender",      2L,
  "M",   "male", "gender",      3L,
  "F", "female", "gender",      4L,
  "m",   "male", "gender",      5L,
  "f", "female", "gender",      6L
)

dat_dictionary

out <- dat %>%
  cleanepi::standardize_column_names() %>%
  cleanepi::remove_constants() %>%
  cleanepi::remove_duplicates() %>%
  cleanepi::check_subject_ids(
    target_columns = "case_id",
    range = c(0, 15000)
  ) %>%
  cleanepi::standardize_dates(
    target_columns = c("date_onset", "date_sample"),
    timeframe = c(lubridate::ymd("20140101"), lubridate::ymd("20161201"))
  ) %>% 
  cleanepi::convert_to_numeric(target_columns = "age") %>% 
  cleanepi::clean_using_dictionary(dictionary = dat_dictionary)

out

cleanepi::print_report(out)

#' next steps:
#' - tutorials early episode 2 https://epiverse-trace.github.io/tutorials-early/clean-data.html
#' - package reference manual https://epiverse-trace.github.io/cleanepi/