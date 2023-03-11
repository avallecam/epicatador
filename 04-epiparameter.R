
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


# build reprex ------------------------------------------------------------


# reprex 01 ---------------------------------------------------------------

library(epiparameter)

eparams <- epiparam()

influenza_incubation <- as_epidist(eparams[12, ])
influenza_incubation


## solution 01 ----------------------------------------------------------------

library(epiparameter)
library(tidyverse)

eparams <- epiparam()

influenza_incubation <- 
  eparams %>% 
  filter(disease=="Influenza",
         epi_distribution == "incubation_period",
         prob_distribution=="gamma",
         PMID=="20029668") %>% 
  as_epidist()

influenza_incubation


## solution 01.2 -----------------------------------------------------------

subset(eparams, 
       disease=="Influenza") %>% 
  print()


# reprex 02 ---------------------------------------------------------------

library(epiparameter)
library(tidyverse)
a <- epiparameter::epiparam()
class(a)
a %>% 
  as_tibble() %>% 
  dplyr::distinct(disease)

# readme ------------------------------------------------------------------

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("epiverse-trace/epiparameter")
# devtools::load_all()
library(epiparameter)
library(tidyverse)

eparams <- epiparam()
eparams
class(eparams)

# influenza_incubation <- as_epidist(eparams[12, ])
# influenza_incubation

eparams[12, ]
eparams %>% 
  filter(disease=="Influenza") %>% 
  filter(epi_distribution == "incubation_period") %>% 
  filter(prob_distribution=="gamma") %>% 
  filter(PMID=="20029668") %>%
  as_tibble() %>% 
  select(!dplyr::where(is.na)) %>% 
  select(!dplyr::where(is.list)) %>% 
  glimpse()

influenza_incubation <- 
  eparams %>% 
  filter(disease=="Influenza") %>% 
  filter(epi_distribution == "incubation_period") %>% 
  filter(prob_distribution=="gamma") %>% 
  filter(PMID=="20029668") %>% 
  as_epidist()

# inner work
# ?epiparameter:::make_epidist

influenza_incubation
class(influenza_incubation)

# ?epiparameter:::plot.epidist
plot(influenza_incubation)

influenza_incubation %>% str()

influenza_incubation %>% as_epiparam()

# get started / introduction ----------------------------------------------

extract_param(
  type = "percentiles",
  values = c(6, 13),
  distribution = "lnorm",
  percentiles = c(0.125, 0.875)
)



#' ERROR:
#' list_distributions(epi_dist = "onset_to_hosp")
#' 
#' NOTE: 
#' list_distributions requires `epiparam = epiparam()`
#' 
list_distributions(epiparam = epiparam(), epi_dist = "onset_to_hosp")

#' ERROR:
#' epidist(pathogen = "SARS_CoV", disease = "COVID-19", epi_dist = "incubation")
#' sars_cov_incub <- epidist(pathogen = "SARS_CoV", delay_dist = "incubation")
#' 
#' ERROR:
#' epidist(epi_dist = "incubation_period", disease = "Influenza")
#' # Assertion on 'prob_dist' failed: May not be NA
#' 
#' SOLUTION:
#' replace 'prob_dist' by 'prob_distribution'
#' 
#' ERROR:
#' epidist(epi_dist = "incubation_period", disease = "Influenza", prob_distribution = "gamma")
#' # No adequate summary statistics available to calculate the parameters of the gamma distribution
#' Error in clean_epidist_params.gamma(prob_dist_params = prob_dist_params) : 
#' Names of gamma distribution parameters are incorrect
#' 
#' SOLUTION:
#' add the name of the argument 'prob_distribution_params'
#' 
#' ERROR:
#' epidist()
#' # argument "disease" is missing, with no default
#' 
#' SOLUTION:
#' add the minimum requirement of arguments for epidist()
#' - epi_dist
#' - disease
#' - prob_distribution
#' - prob_distribution_params

epidist(epi_dist = "incubation_period",
        disease = "Influenza", 
        prob_distribution = "gamma",
        prob_distribution_params = c(shape = 1, scale = 1))

epidist(epi_dist = "incubation_period",
        disease = "COVID-19", 
        prob_distribution = "gamma",
        prob_distribution_params = c(shape = 1, scale = 1))


# alternative to extract parameters ---------------------------------------

epiparam(epi_dist = "incubation")


# test --------------------------------------------------------------------

# ERROR

epidist(
  disease = "ebola", 
  epi_dist= "incubation_period", 
  prob_distribution = "lognormal", 
  prob_distribution_params = c(meanlog = 1, sdlog = 1)
)

# SOLUTION

epidist(
  disease = "ebola", 
  epi_dist= "incubation_period", 
  prob_distribution = "lnorm", 
  prob_distribution_params = c(meanlog = 1, sdlog = 1)
)

epidist(
  disease = "ebola", 
  epi_dist= "incubation_period", 
  prob_distribution = "lnorm", 
  prob_distribution_params = c(mu = 1, sigma = 1)
  # prob_distribution_params = c(meanlog = 1, sdlog = 1)
)

create_prob_dist(prob_dist = "lnorm",
                 prob_dist_params = c(mu = 1, sigma = 1),
                 discretise = T,truncation = NA)

prob_dist_params = c(meanlog = 1, sdlog = 1)
distcrete::distcrete(
  name = "lnorm",
  interval = 1,
  meanlog = prob_dist_params[["meanlog"]],
  sdlog = prob_dist_params[["sdlog"]],
  w = 1
)

create_prob_dist(prob_dist = "gamma",
                 prob_dist_params = c(shape = 1, scale = 1),
                 discretise = F,truncation = NA)

epidist(
  disease = "ebola",
  epi_dist = "incubation_period",
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = 1, scale = 1)
)

epidist(
  disease = "dengue", 
  epi_dist = "incubation", 
  prob_distribution = "lnorm", 
  prob_distribution_params = c(meanlog = 1, sdlog = 1)
)

# test 02 -----------------------------------------------------------------


epidist(
  disease = "dengue", 
  epi_dist = "incubation", 
  prob_distribution = "lnorm", 
  prob_distribution_params = c(meanlog = 1, sdlog = 1), 
  metadata = create_epidist_metadata(vector = "anopheles",transmission_mode = "vector_borne")
  # metadata = create_epidist_metadata(vector_borne = TRUE)
)

# all previous code --------------------------------------------------------

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
