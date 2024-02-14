
#' 
#' goal
#' clean epiparameter epinow2 connection
#' to clarify the potential misspecification of inputs 
#' from epiparameter to epinow2 
#' 
#' shared
#' https://github.com/epiverse-trace/episoap/issues/130#issuecomment-1941735534

library(epiparameter)
library(EpiNow2)

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

incubation_time_covid <- EpiNow2::dist_spec(
  mean = covid_incubation_parameters["meanlog"],
  sd = covid_incubation_parameters["sdlog"],
  max = covid_incubation_discrete$prob_dist$q(p = 0.999), #must for epinow()
  distribution = "lognormal"
)

incubation_time_covid