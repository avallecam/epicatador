
#' create datasets for EpiNow2 and CFR
#' assessments #53 and #54

library(tidyverse)

cfr::ebola1976 %>% 
  as_tibble() %>% 
  readr::write_rds("data-out/ebola_full_days.rds")

cfr::ebola1976 %>% 
  as_tibble() %>% 
  incidence2::incidence(
    date_index = "date",
    counts = c("cases", "deaths")
  ) %>% 
  plot()

incidence_class <- incidence2::covidregionaldataUK %>% 
  # preprocess missing values
  tidyr::replace_na(
    list(
      deaths_new = 0,
      cases_new = 0
    )
  ) %>%
  # compute the daily incidence
  incidence2::incidence(
    date_index = "date",
    counts = c("cases_new","deaths_new"),
    date_names_to = "date",
    complete_dates = TRUE
  )

incidence_class %>% 
  plot()

incidence_class %>% 
  pivot_wider(
    id_cols = date,
    names_from = count_variable,
    values_from = count) %>% 
  readr::write_rds("data-out/covid_incidence2_full_days.rds")

