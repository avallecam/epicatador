
# readme ------------------------------------------------------------------

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("epiverse-trace/epiparameter")
# 
# # View available distributions
# epiparameter::epiparam()
# 
# epiparameter::list_distributions(delay_dist = "incubation")
# 
# # Extract incubation period distribution
# incubation_H7N9 <- epiparameter::epidist(
#   pathogen = "influenza_H7N9",
#   delay_dist = "incubation"
# )
# 
# # Plot probability distributions
# plot(incubation_H7N9)


# readme ------------------------------------------------------------------

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("epiverse-trace/epiparameter")
# devtools::load_all()
library(epiparameter)
library(tidyverse)

eparams <- epiparam()
eparams
class(eparams)
eparams %>% as_tibble()

## epiparam ----------------------------------------------------------------

#' function and class

epiparam(epi_dist = "incubation_period")

eparam <- epiparam()
class(eparam)

library(dplyr)
epiparam(epi_dist = "incubation_period") %>% as_tibble()
eparam %>% as_tibble()
epiparam(epi_dist = "incubation_period") %>% as_tibble() %>% glimpse()
epiparam(epi_dist = "incubation_period") %>% as_tibble() %>% naniar::vis_miss()

## epidist -----------------------------------------------------------------

#' function and class

epidist(
  disease = "ebola",
  epi_dist = "incubation_period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1, scale = 1)
) %>%
  class()

epidist(
  disease = "ebola",
  epi_dist = "incubation_period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1, scale = 1)
)

epidist(
  disease = "ebola",
  epi_dist = "incubation_period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1, scale = 1),
  discretise = TRUE
)

epidist(
  disease = "ebola",
  epi_dist = "incubation_period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1, scale = 1)
) %>%
  plot()

epidist(
  disease = "ebola",
  epi_dist = "incubation_period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1, scale = 1),
  discretise = TRUE
) %>%
  plot()

## epidist_db --------------------------------------------------------------

epidist_db(disease = "influenza")
epidist_db(disease = "influenza", epi_dist = "serial_interval")

#' the example works,
#' but not the new trials

epiparam() %>% as_tibble() %>%
  count(disease) %>% print(n = Inf)

epiparam() %>% as_tibble() %>%
  filter(disease == "marburg virus disease") %>%
  count(epi_distribution)

epidist_db(disease = "marburg virus disease")
epidist_db(disease = "marburg virus disease",epi_dist = "incubation_period")

# epidist_db(disease = "ebola")
# epidist_db(disease = "ebola",epi_dist = "incubation_period")
