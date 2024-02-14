
#' 
#' goal
#' get a cleaner howto guide to connect
#' epiparameter to EpiNow2
#' 
#' shared
#' https://github.com/epiverse-trace/howto/issues/34

# howto epinow2 -----------------------------------------------------------

library(epiparameter)
library(EpiNow2)
library(tidyverse)

# cases -------------------------------------------------------------------

example_confirmed # assumption: covid data

# delay: generation time --------------------------------------------------------------

covid_serialint <- 
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "serial",
    author = "Nishiura",
    single_epidist = T
  )

covid_serialint

covid_serialint_parameters <- epiparameter::get_parameters(covid_serialint)

covid_serialint_parameters

covid_serialint_discrete <- 
  epiparameter::discretise(covid_serialint)

covid_serialint_discrete_max <- 
  covid_serialint_discrete$prob_dist$q(p = 0.999)

serial_interval_covid <- 
  dist_spec(
    mean = covid_serialint_parameters["meanlog"],
    sd = covid_serialint_parameters["sdlog"],
    max = covid_serialint_discrete_max, #must for epinow()
    distribution = "lognormal"
  )

serial_interval_covid

# delay: incubation period -------------------------------------------------------------------

covid_incubation <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  author = "Natalie",
  single_epidist = T
)

covid_incubation

covid_incubation_parameters <- epiparameter::get_parameters(covid_incubation)

covid_incubation_parameters

covid_incubation_discrete <- epiparameter::discretise(covid_incubation)

incubation_time_covid <- dist_spec(
  mean = covid_incubation_parameters["meanlog"],
  sd = covid_incubation_parameters["sdlog"],
  max = covid_incubation_discrete$prob_dist$q(p = 0.999), #must for epinow()
  distribution = "lognormal"
)

incubation_time_covid

# epinow -------------------------------------------------------------------

epinow_estimates <- epinow(
  # cases
  reported_cases = example_confirmed[1:60],
  # delays
  generation_time = generation_time_opts(serial_interval_covid),
  delays = delay_opts(incubation_time_covid),
  # computation
  stan = stan_opts(
    cores = 4, samples = 1000, chains = 3,
    control = list(adapt_delta = 0.99)
  )
)

summary(epinow_estimates)
plot(epinow_estimates)
