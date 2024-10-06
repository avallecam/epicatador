
#' goal:
#' estimate transmissibility from time-aggregated linelist

# To Do -------------------------------------------------------------------

#' [O] add to how-to guides

library(cleanepi)
library(linelist)
library(incidence2)
library(tidyverse)

dat <- subset(outbreaks::ebola_sim_clean$linelist ,!is.na(hospital)) %>% 
  dplyr::as_tibble()

# linelist::lost_tags_action(action = "error")
# linelist::lost_tags_action(action = "warning")

dat_linelist <- dat %>% 
  
  # focus on key variables
  # dplyr::select(case_id, date_of_onset, gender, hospital) %>%
  
  linelist::make_linelist(
    id = "case_id",
    date_onset = "date_of_onset", 
    gender = "gender",
    location = "hospital"
  ) %>% 
  linelist::validate_linelist() %>% 
  linelist::tags_df() # change: does removing this step changes the downstream analysis?

# exploratory plot
dat_linelist %>% 
  # aggregate by groups and date type
  incidence2::incidence(
    date_index = "date_onset",
    groups = c("gender", "location"), # change: "gender" or "location" or c("gender", "location")
    interval = "week", # change between: "day" or "week" or "quarter"
  ) %>% 
  incidence2:::plot.incidence2(
    fill = "gender"  # change: "gender", "location"
  )

# get incidence
dat_incidence <- dat_linelist %>% 
  # aggregate by date type
  incidence2::incidence(
    date_index = "date_onset",
    interval = "day", # for interoperability with {epinow2}
    # complete_dates = TRUE, # change: does it affect the downstream analysis? [no]
    date_names_to = "date", # for interoperability with {epinow2}
    count_values_to = "confirm", # for interoperability with {epinow2}
  ) %>%
  # reduce computation time (only for this time-constrained tutorial)
  dplyr::filter(date>="2014-06-01" & date<"2014-10-01") %>% # try: incidence2::keep_first(n = 100)
  # for interoperability with {epinow2}
  dplyr::select(-count_variable) %>% 
  # convert to tibble format for simpler data frame output
  dplyr::as_tibble()

# get delay
serial_interval <-
  epiparameter::epiparameter_db(
    disease = "ebola",
    epi_dist = "serial interval",
    single_epiparameter = TRUE
  )

serial_interval_param <- epiparameter::get_parameters(serial_interval)

serial_interval_gamma <- EpiNow2::Gamma(
  shape = serial_interval_param["shape"],
  scale = serial_interval_param["scale"]
)

# parallel computation
withr::local_options(base::list(mc.cores = 4))

# estimate transmissibility
estimates <- EpiNow2::epinow(
  # cases
  data = dat_incidence,
  # delays
  generation_time = EpiNow2::generation_time_opts(serial_interval_gamma),
  # reduce computation time (only for this time-constrained tutorial)
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(estimates)
