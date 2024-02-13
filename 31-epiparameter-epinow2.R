# howto epinow2 -----------------------------------------------------------

library(epiparameter)
library(EpiNow2)
library(tidyverse)

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

# discretise continuous distribution --------------------------------------

covid_serialint_discrete <- 
  epiparameter::discretise(covid_serialint)

# get maximum value from discrete distribution ----------------------------

covid_serialint_discrete_max <- 
  covid_serialint_discrete$prob_dist$q(p = 0.999)

covid_serialint_discrete_max

# get sequence of quantile values -----------------------------------------

si_x <- seq(1L, to = covid_serialint_discrete_max, by = 1L)

# # previous: EpiNow2 1.4.0 -------------------------------------------------
# 
# serial_interval_covid <- 
#   EpiNow2::dist_spec(
#     mean = covid_serialint$summary_stats$mean,
#     sd = covid_serialint$summary_stats$sd,
#     max = covid_serialint_discrete_max,
#     distribution = "lognormal"
#   )
# 
# serial_interval_covid


# # update: EpiNow2 1.4.9 ---------------------------------------------------
# 
# si_x <- seq(1L, to = covid_serialint_discrete_max, by = 1L)
# 
# serial_interval_covid_update <- 
#   EpiNow2::dist_spec(pmf = covid_serialint_discrete$prob_dist$d(si_x))
# 
# serial_interval_covid_update


# new branch: EpiNow2@dist-interfase --------------------------------------

serial_interval_covid_branch_pmf <- 
  EpiNow2::pmf(covid_serialint_discrete$prob_dist$d(si_x))

serial_interval_covid_branch_pmf

# new branch: is this a valid alternative? --------------------------------

covid_serialint_parameters <- epiparameter::get_parameters(covid_serialint)

covid_serialint_parameters

library(purrr)

serial_interval_covid_branch_lognormalmax <- 
  EpiNow2::LogNormal(
    meanlog = covid_serialint_parameters %>% pluck("meanlog"),
    sdlog = covid_serialint_parameters %>% pluck("sdlog"),
    max = covid_serialint_discrete_max)

serial_interval_covid_branch_lognormalmax

# # delay: incubation period -------------------------------------------------------------------
# 
# covid_incubation <- epiparameter::epidist_db(
#   disease = "covid",
#   epi_dist = "incubation",
#   author = "Natalie",
#   single_epidist = T
# )
# 
# covid_incubation
# 
# covid_incubation_discrete <- epiparameter::discretise(covid_incubation)
# 
# incubation_time_covid <- dist_spec(
#   mean = covid_incubation$summary_stats$mean,
#   sd = covid_incubation$summary_stats$sd,
#   max = covid_incubation_discrete$prob_dist$q(p = 0.999),
#   distribution = "lognormal"
# )

# epinow -------------------------------------------------------------------

epinow_estimates <- epinow(
  # cases
  reported_cases = example_confirmed[1:60],
  # delays
  generation_time = generation_time_opts(serial_interval_covid_branch_pmf),
  # computation
  stan = stan_opts(
    cores = 4, samples = 1000, chains = 3,
    control = list(adapt_delta = 0.99)
  )
)

summary(epinow_estimates)
plot(epinow_estimates)


# is this a valid alternative? --------------------------------------------

epinow_estimates_alt <- epinow(
  # cases
  reported_cases = example_confirmed[1:60],
  # delays
  generation_time = generation_time_opts(serial_interval_covid_branch_lognormalmax),
  # computation
  stan = stan_opts(
    cores = 4, samples = 1000, chains = 3,
    control = list(adapt_delta = 0.99)
  )
)

summary(epinow_estimates_alt)
plot(epinow_estimates_alt)
