
#' goal: simulate transmission trajectories in heterogenious-mixing population
#' 
#' task: 
#' - identify how the input changes running step by step

library(tidyverse)
library(socialmixr)
library(epidemics)

# SEIR-V


# {socialmixr} -----------------------------------------------------------

socialmixr::list_surveys()

# transmission
socialsurvey_link <- "https://doi.org/10.5281/zenodo.3874805" # polymod
socialsurvey_country <- "Peru"
age_limits <- c(0, 20, 40)

socialsurvey <- socialmixr::get_survey(survey = socialsurvey_link)

contact_data <- socialmixr::contact_matrix(
  survey = socialsurvey,
  countries = socialsurvey_country,
  age.limits = age_limits,
  symmetric = TRUE
)

# prepare contact matrix
socialcontact_matrix <- t(contact_data$matrix)

## infectious population ---------
initial_i <- infectious_population

initial_conditions_inf <- c(
  S = 1 - initial_i,
  E = 0,
  I = initial_i,
  R = 0,
  V = 0
)

initial_conditions_inf

## free of infection population ---------
initial_conditions_free <- c(
  S = 1,
  E = 0,
  I = 0,
  R = 0,
  V = 0
)

initial_conditions_free

## combine initial conditions ------------

# combine the initial conditions
initial_conditions <- base::rbind(
  initial_conditions_free, # age group 1
  initial_conditions_inf, # age group 2
  initial_conditions_free # age group 3
)

# use contact matrix to assign age group names
rownames(initial_conditions) <- rownames(socialcontact_matrix)

initial_conditions

# prepare the demography vector
demography_vector <- contact_data$demography$population
names(demography_vector) <- rownames(socialcontact_matrix)


# {epidemics} ------------------------------------------------------------

# poblacion
population_object <- epidemics::population(
  name = socialsurvey_country,
  contact_matrix = socialcontact_matrix,
  demography_vector = demography_vector,
  initial_conditions = initial_conditions
)

# objectivo
simulate_baseline <- epidemics::model_default(
  population = population_object,
  transmission_rate = basic_reproduction_number / infectious_period,
  infectiousness_rate = 1/ pre_infectious_period,
  recovery_rate = 1 / infectious_period,
  time_end = 800,
  increment = 1
)

simulate_baseline %>%
  ggplot(aes(
    x = time,
    y = value,
    color = compartment,
    linetype = demography_group
  )) +
  geom_line() +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 10),
    labels = scales::comma
  )


simulate_baseline %>%
  epidemics::new_infections() %>%
  ggplot(aes(time, new_infections, colour = demography_group)) +
  geom_line() +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 10),
    labels = scales::comma
  )


#' next steps:
#' - tutorials late episodes 1-2 https://epiverse-trace.github.io/tutorials-late/contact-matrices.html
#' - run the next script
