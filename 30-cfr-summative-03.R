
# TODO

#' (O) fix and add to howto

# [4] summative 04 --------------------------------------------------------

#' packages:
#' 
#' simulist -> incidence2 -> [epiparameter -> cfr]
#' 
#' concepts:
#' 
#' - simulist package
#' 
#' useful for:
#' 
#' - tutorial episode 03 as assessment

# load packages -----------------------------------------------------------

library(simulist)
library(epiparameter)
library(incidence2)
library(cfr)
library(tidyverse)

# comments ----------------------------------------------------------------

#' ideas
#' - add "create epidist from scratch" as an assessment in read delays chapter
#' - add "use get_parameters()" as assessment in read delays

# estimate severity -------------------------------------------------------

# james reprex ------------------------------------------------------------

## reuse epi distributions -------------------------------------------------

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "serial") %>% 
  epiparameter::list_distributions()

covid_parameter <- 
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "serial",
    author = "Hiroshi",
    single_epidist = T
  ) %>% 
  epiparameter::get_parameters()

covid_parameter

# create a distribution from scratch

serial_interval <- epiparameter::epidist(
  disease = "covid",
  epi_dist = "serial",
  prob_distribution = "lnorm",
  prob_distribution_params = c(meanlog = covid_parameter %>% pluck(1), 
                               sdlog = covid_parameter %>% pluck(2))
)

# serial_interval <- epiparameter::epidist(
#   disease = "covid",
#   epi_dist = "serial",
#   prob_distribution = "gamma",
#   prob_distribution_params = c(shape = 1, scale = 1)
# )

serial_interval

# reuse from systematic review

onset_to_hosp <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "onset to hospitalisation",
  single_epidist = TRUE
)

onset_to_hosp

onset_to_death <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "onset to death",
  single_epidist = TRUE
)

onset_to_death

## simulate a linelist -----------------------------------------------------

set.seed(33)

linelist <- sim_linelist(
  R = 1.1,
  serial_interval = serial_interval,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  # hosp_death_risk = 0.8,
  # non_hosp_death_risk = 0.5,
  # hosp_risk = 0.5,
  # add_ct = FALSE,
  # add_names = FALSE
)

linelist %>% 
  glimpse()

incidence2::incidence(
  x = linelist,
  date_index = c("date_onset","date_death"),
  interval = "week",
  date_names_to = "date"
) %>% 
  plot()

## convert to incidence2 class object --------------------------------------

incidence_data <- incidence2::incidence(
  x = linelist,
  date_index = c("date_onset","date_death")
)

incidence_data
incidence_data %>% count(count_variable)

plot(incidence_data)

## prepare incidence2 for cfr estimation -----------------------------------

cfr_data <- cfr::prepare_data(
  data = incidence_data,
  cases_variable = "date_onset",
  deaths_variable = "date_death",
  fill_NA = TRUE
)

#' Error in (function (df, colname)  : 
#' `NA`s present in the 'cases' count. Set `fill_NA = TRUE` to use 0s

cfr_data

## estimate cfr ------------------------------------------------------------

cfr::cfr_static(data = cfr_data)
