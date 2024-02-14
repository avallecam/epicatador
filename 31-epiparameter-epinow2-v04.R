
#' 
#' goal
#' find example of uncertainty with weibull or nbinom
#' to input them to EpiNow2
#' 
#' status: NOT COMPLETE YET!

library(epiparameter)
library(tidyverse)

epidist_db() %>% 
  list_distributions() %>% 
  # count(prob_distribution,sort = T)
  filter(magrittr::is_in(prob_distribution,c("weibull","nbinom"))) %>% 
  arrange(author)

# observation
# nbinom is only available for offspring distributions

# lets go for weibull

influenza_incubation <- 
  epidist_db(
    disease = "influenza",
    epi_dist = "incubation",
    author = "Victor",
    single_epidist = T
  )

influenza_incubation

influenza_incubation$uncertainty

get_parameters(influenza_incubation)

influenza_incubation$summary_stats


# reference ---------------------------------------------------------------

covid_lnorm <-
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "serial",
    author = "Nishiura",
    single_epidist = T
  )

covid_lnorm$summary_stats
