library(simulist)
library(epiparameter)
library(cfr)
library(tidyverse)

# simulate sars-cov2 outbreak --------------------------------------------


# epiparameter -----------------------------------------------------------

# epiparameter::epiparameter_db(disease = "covid") %>% 
#   epiparameter::parameter_tbl()

# create COVID-19 contact distribution
covid_contact_distribution <- epiparameter::epiparameter(
  disease = "COVID-19",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 2)
  )
)

# create COVID-19 infectious period
covid_infectious_period <- epiparameter::epiparameter(
  disease = "COVID-19",
  epi_name = "infectious period",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(shape = 1, scale = 1)
  )
)

# get onset to hospital admission from {epiparameter}
covid_onset_to_hosp <- epiparameter::epiparameter_db(
  disease = "COVID-19",
  epi_name = "onset to hospitalisation",
  single_epiparameter = TRUE
)

# get onset to death from {epiparameter}
covid_onset_to_death <- epiparameter::epiparameter_db(
  disease = "COVID-19",
  epi_name = "onset to death",
  single_epiparameter = TRUE
)


# simulist ---------------------------------------------------------------

set.seed(1)
simulated_linelist <- simulist::sim_linelist(
  contact_distribution = covid_contact_distribution,
  infectious_period = covid_infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = covid_onset_to_hosp,
  onset_to_death = covid_onset_to_death,
  hosp_risk = 0.1,
  hosp_death_risk = 0.05,
  non_hosp_death_risk = 0.02,
  outbreak_size = c(1000, 1500)
)

simulated_linelist %>% as_tibble()

# size
nrow(simulated_linelist)
# hospital risk
sum(!is.na(simulated_linelist$date_admission)) / nrow(simulated_linelist)
# naive CFR
sum(simulated_linelist$outcome == "died") / nrow(simulated_linelist)
# HFR
sum(simulated_linelist$outcome == "died") / sum(!is.na(simulated_linelist$date_admission))


# incidence2 -------------------------------------------------------------

simulated_incidence <- simulated_linelist %>% 
  incidence2::incidence(
    date_index = c("date_onset","date_outcome"),
    interval = "day",
    complete_dates = TRUE 
  )
  
simulated_incidence %>%
  plot()


# cfr --------------------------------------------------------------------

simulated_incidence %>% 
  cfr::prepare_data(cases_variable = "date_onset", deaths_variable = "date_outcome") %>% 
  cfr::cfr_static()

simulated_incidence %>% 
  cfr::prepare_data(cases_variable = "date_onset", deaths_variable = "date_outcome") %>% 
  cfr::cfr_static(delay_density = as.function(covid_onset_to_death, "density"))

rolling_naive <- simulated_incidence %>% 
  cfr::prepare_data(cases_variable = "date_onset", deaths_variable = "date_outcome") %>% 
  cfr::cfr_rolling()

rolling_adjust <- simulated_incidence %>% 
  cfr::prepare_data(cases_variable = "date_onset", deaths_variable = "date_outcome") %>% 
  cfr::cfr_rolling(delay_density = as.function(covid_onset_to_death, "density"))

