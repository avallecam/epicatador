# howto epinow2 -----------------------------------------------------------

library(epiparameter, warn.conflicts = F)
library(EpiNow2, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)

# cases -------------------------------------------------------------------

example_confirmed

# delay: generation time --------------------------------------------------------------

covid_serialint <- 
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "serial",
    author = "Nishiura",
    single_epidist = T
  )

covid_serialint

covid_serialint_discrete <- 
  epiparameter::discretise(covid_serialint)

covid_serialint_discrete_max <- 
  covid_serialint_discrete$prob_dist$q(p = 0.999)

covid_serialint_discrete_max

# previous ----------------------------------------------------------------

serial_interval_covid <- 
  EpiNow2::dist_spec(
    mean = covid_serialint$summary_stats$mean,
    sd = covid_serialint$summary_stats$sd,
    max = covid_serialint_discrete_max,
    distribution = "lognormal"
  )

serial_interval_covid

# update ------------------------------------------------------------------

si_x <- seq(1L, to = covid_serialint_discrete_max, by = 1L)

serial_interval_covid_update <- 
  EpiNow2::dist_spec(pmf = covid_serialint_discrete$prob_dist$d(si_x))

serial_interval_covid_update

# delay: incubation period -------------------------------------------------------------------

covid_incubation <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  author = "Natalie",
  single_epidist = T
)

covid_incubation

covid_incubation_discrete <- epiparameter::discretise(covid_incubation)

incubation_time_covid <- dist_spec(
  mean = covid_incubation$summary_stats$mean,
  sd = covid_incubation$summary_stats$sd,
  max = covid_incubation_discrete$prob_dist$q(p = 0.999),
  distribution = "lognormal"
)

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

base::plot(epinow_estimates)
