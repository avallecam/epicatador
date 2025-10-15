library(cfr)
library(epiparameter)
library(tidyverse)
library(outbreaks)

# Load the Ebola 1976 data provided with the {cfr} package
data("ebola1976")

# Assume we only have the first 30 days of this data
ebola_30days <- ebola1976 %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::as_tibble()

# Get delay distribution
onset_to_death_ebola <-
  epiparameter::epiparameter_db(
    disease = "Ebola",
    epi_name = "onset_to_death",
    single_epiparameter = TRUE
  ) %>% 
  epiparameter::discretise()



# spoiler start ----------------------------------------------------------

# By Day 1, the expected outcomes are:
ebola_30days$cases[1] *
  density(onset_to_death_ebola, at = 0)

# By Day 2, the expected outcomes are:
ebola_30days$cases[1] * 
  density(onset_to_death_ebola, at = 1) +
  ebola_30days$cases[2] *
    density(onset_to_death_ebola, at = 0)

# By Day 3, the expected outcomes are:
ebola_30days$cases[1] *
  density(onset_to_death_ebola, at = 2) +
  ebola_30days$cases[2] *
    density(onset_to_death_ebola, at = 1) +
  ebola_30days$cases[3] *
    density(onset_to_death_ebola, at = 0)

# Notice that the most recently observe cases
# start the delay distribution from 0
# the others continue with the following day

# we need density let it vary across different x values
function(x) density(onset_to_death_ebola, at = x)

# the understimation factor for each day
cfr::estimate_outcomes(
  data = ebola_30days,
  delay_density = function(x) density(onset_to_death_ebola, at = x)
) %>%
  slice_head(n = 4) %>% 
  pull(estimated_outcomes)

# spoiler ends -----------------------------------------------------------




# other

delay_density <- function(x) {
  density(onset_to_death_ebola,at = x)
}

cases <- ebola_30days$cases[1:10]
pmf_vals <- delay_density(x = seq(from = 0, to = (length(cases)-1)))
length(pmf_vals)



i = 3
sum(cases[seq_len(i)] * rev(pmf_vals[seq_len(i)]))

vapply(
  X = seq_along(cases),
  FUN = function(i) {
    # estimate expected number of outcomes
    sum(cases[seq_len(i)] * rev(pmf_vals[seq_len(i)]))
  },
  FUN.VALUE = numeric(1)
) %>% 
  cumsum() / cumsum(cases)



# Calculate the delay-adjusted CFR
# for the first 30 days
cfr::cfr_static(
  data = ebola_30days,
  delay_density = function(x) density(onset_to_death_ebola, x)
)

cfr::cfr_static(
  data = ebola1976,
  delay_density = function(x) density(onset_to_death_ebola, x)
)
