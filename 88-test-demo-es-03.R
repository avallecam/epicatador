
#' goal: create a epidemic curve from a line list data frame
#' 
#' task: 
#' - identify how the input changes running step by step

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

#' next step:
#' - tutorials early episode 4 https://epiverse-trace.github.io/tutorials-early/describe-cases.html
#' - how-to guide https://epiverse-trace.github.io/howto/analyses/describe_cases/cleanepi-linelist-incidence2-stratified.html