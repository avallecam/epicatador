
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


# reprex 05 ---------------------------------------------------------------

#' add an example of how to use a conversion functions to build a custom epidist class object
#' 
#' in a vignette about 
#' conversion functions,
#' the quick start example in README
#' can be used 
#' to apply a conversion function
#' after knowing
#' how to build a custom epidist object
#' with lnorm
#' 
#' first, get the distribution parameters
#' from summary statistics, and
#' then use epidist() 

library(epiparameter)
library(tidyverse)

eparams <- epiparam()

epidist(
  disease = "ebola", 
  epi_dist= "incubation_period", 
  prob_distribution = "lnorm", 
  prob_distribution_params = c(meanlog = 1, sdlog = 1)
)

# This 
# quick start example in README
eparams %>% 
  filter(disease=="Influenza") %>% 
  filter(epi_distribution == "incubation_period") %>% 
  filter(prob_distribution=="gamma") %>% 
  filter(author=="Ghani_etal") %>% 
  
  # break epiparam class object
  as_tibble() %>% 
  select(disease, epi_distribution, mean, sd)

# Can be used to
# show conversion functions in the context of
# How to replicate the quick start example in README?
gamma_example <- gamma_meansd2shapescale(mean = 2.05,
                                         sd = 0.49)
gamma_example

epidist(
  disease = "influenza", 
  epi_dist= "incubation_period", 
  prob_distribution = "gamma",
  prob_distribution_params = c(shape = gamma_example$shape, 
                               scale = gamma_example$scale)
)

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
         author=="Ghani_etal") %>% 
  as_epidist()

influenza_incubation


## solution 01.2 -----------------------------------------------------------

library(epiparameter)
library(tidyverse)

epidist_db(disease = "influenza", 
           epi_dist = "incubation_period",
           author = "Ghani_etal")



# new ---------------------------------------------------------------------

library(epiparameter)
library(tidyverse)

eparams <- epiparam()

eparams %>% 
  filter(disease=="Influenza")

eparams %>% 
  select(disease)

# reprex 03 ---------------------------------------------------------------

library(epiparameter)
library(tidyverse)

eparams <- epiparam()

eparams_filtered <- eparams %>% 
  
  # transform to tibble
  # the name for dataframes to tidyverse users
  # * this step breaks the epiparam class object *
  # as_tibble() %>% 
  
  # filter one row or one parameter
  filter(disease=="Influenza") %>% 
  filter(epi_distribution == "incubation_period") %>% 
  filter(prob_distribution=="gamma") %>% 
  filter(author=="Ghani_etal")

# transform dataframe from epiparam object
# to epidist class object
eparams_filtered %>% 
  as_epidist()


# extra -------------------------------------------------------------------



## solution 03 -----------------------------------------------------------

library(epiparameter)
library(tidyverse)

# 1/4
eparams <- epiparam()

# 2/4
eparams_filtered <- eparams %>% 
  
  # filter one row = one parameter
  filter(disease=="Influenza") %>% 
  filter(epi_distribution == "incubation_period") %>% 
  filter(prob_distribution=="gamma") %>% 
  filter(author=="Ghani_etal")

# # 3/4
# print minimum set of compulsory columns
# that epidist() needs
eparams_filtered %>%
  as_tibble() %>% 
  select(disease,
         epi_distribution,
         prob_distribution,
         mean,
         sd)

# 4/4
eparams_filtered %>%

  select(disease,
         epi_distribution,
         prob_distribution,
         mean,
         sd) %>%
    
  # convert mean+sd to shape+scale
  mutate(gamma_convertion = 
           pmap(.l = select(.,
                            mean,
                            sd),
                .f = gamma_meansd2shapescale)) %>%
  
  # convert named list to named numeric vector
  mutate(prob_distribution_params = 
           # extract list from column
           pull(.,gamma_convertion) %>% 
           pluck(1) %>% 
           # convert to numeric
           unlist(use.names = T) %>% 
           # convert to list
           list()) %>%
  
  # create new column with epidist() output
  mutate(epidist_conversion = 
           pmap(.l = 
                  select(
                    .,
                    disease,
                    epi_dist = epi_distribution,
                    prob_distribution,
                    prob_distribution_params
                  ),
                .f = epidist)) %>% 
  
  # extract epidist() output from column
  pull(epidist_conversion) %>% 
  pluck(1)

epidist_db(disease = "influenza", 
           epi_dist = "incubation_period",
           author = "Ghani_etal")

epidist(epi_dist = "incubation_period",
        disease = "Influenza", 
        prob_distribution = "gamma",
        prob_distribution_params = c(shape = 1, scale = 1))


# reprex 4 ----------------------------------------------------------------

library(epiparameter)
library(tidyverse)

eparams <- epiparam()

eparams %>% 
  
  # transform to tibble
  # the name for dataframes to tidyverse users
  as_tibble() %>% 
  
  # filter one row = one parameter
  filter(disease=="Influenza") %>% 
  filter(epi_distribution == "incubation_period") %>% 
  filter(prob_distribution=="gamma") %>% 
  filter(author=="Ghani_etal") %>% 
  
  # explore content
  glimpse()

# reprex 02 ---------------------------------------------------------------

library(epiparameter)
library(dplyr)
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

library(epiparameter)

epidist(
  disease = "ebola", 
  epi_dist= "incubation_period", 
  prob_distribution = "lnorm", 
  prob_distribution_params = c(meanlog = 1, sdlog = 1)
)

epidist(
  disease = "ebola", 
  epi_dist= "incubation_period", 
  prob_distribution = "lognormal", 
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
