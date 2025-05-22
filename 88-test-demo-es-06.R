
library(tidyverse)
library(socialmixr)
library(epidemics)

# SEIR-V

# transmission
infectious_population <- 1 / 1e6 # 1 infectious out of 1,000,000

basic_reproduction_number <- 1.46
pre_infectious_period <- 3 # days
infectious_period <- 7 # days

# homogenious mixing
socialcontact_matrix <- matrix(1)
demography_vector <- 30000000
initial_conditions <- matrix(
  c(
    S = 1 - infectious_population,
    E = 0,
    I = infectious_population,
    R = 0,
    V = 0
  ),
  nrow = 1,
  ncol = 5
)


# {epidemics} ------------------------------------------------------------


# poblacion structure
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
