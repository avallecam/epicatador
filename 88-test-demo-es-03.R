
dat_validated

dat_validated %>%
  incidence2::incidence(
    date_index = "date_onset",
    complete_dates = TRUE,
    interval = "epiweek",
    groups = "gender"
  ) %>%
  dplyr::slice(1:20) %>%
  # plot()
  incidence2:::plot.incidence2(
    angle = 45,
    show_cases = TRUE,
    fill = "gender"
  )