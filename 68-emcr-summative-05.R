
#' age-stratified cfr
#' 
#' to do
#' - change input to use {cleanepi} and calculate age from "date of birth"
#' - save input file in tutorials-early?

library(cleanepi)
library(linelist)
library(incidence2)
library(tidyverse)

dat <- subset(outbreaks::ebola_sim_clean$linelist ,!is.na(hospital)) %>% 
  dplyr::as_tibble()

# linelist::lost_tags_action(action = "error")
# linelist::lost_tags_action(action = "warning")

set.seed(33)

dat_incidence <- dat %>% 
  # add age as a normal-distributed variable
  dplyr::mutate(age = charlatan::ch_norm(n = n(), mean = 55, sd = 10)) %>% 
  # categorize age
  mutate(age_category = cut(
    x = age,
    breaks = c(0,30,50,70,100),
    include.lowest = TRUE,
    right = FALSE
  )
  ) %>% 
  # create date of death variable
  dplyr::mutate(date_of_death = dplyr::case_when(
    outcome == "Death" ~ date_of_outcome,
    TRUE ~ NA_Date_
  )) %>% 
  # focus on key variables
  # dplyr::select(case_id, date_of_onset, date_of_death, age_category) %>%
  
  linelist::make_linelist(
    id = "case_id",
    date_onset = "date_of_onset",
    date_death = "date_of_death",
    occupation = "age_category" # not accurate
  ) %>% 
  linelist::validate_linelist() %>% 
  linelist::tags_df() %>% 
  
  # aggregate by groups and date type
  incidence2::incidence(
    date_index = c("date_onset", "date_death"),
    groups = "occupation", # "age_category",
    complete_dates = TRUE,
    interval = "day" # change between: day, week
  )

# exploratory plot
dat_incidence %>% 
  incidence2:::plot.incidence2(fill = "occupation")
  # incidence2:::plot.incidence2(fill = "age_category")

# get delay
delay_onset_death <-
  epiparameter::epiparameter_db(
    disease = "ebola",
    epi_dist = "onset to death",
    single_epiparameter = TRUE
  )

# FILL ISSUE
# ?cfr::prepare_data() does not preserve TIBBLE output
# rationale: "if input is a tibble, output should be a tibble"
dat_incidence %>% 
  # good interoperability between {incidece2} and {cfr}
  cfr::prepare_data(
    cases_variable = "date_onset",
    deaths_variable = "date_death"
  ) %>% 
  as_tibble() %>%
  # to stratify {cfr} output
  # group_by(age_category) %>%
  group_by(occupation) %>%
  nest() %>%
  mutate(
    temp =
      map(
        .x = data,
        .f = cfr::cfr_static,
        delay_density = function(x) density(delay_onset_death, x)
      )
  ) %>%
  unnest(cols = temp) %>% 
  identity()


