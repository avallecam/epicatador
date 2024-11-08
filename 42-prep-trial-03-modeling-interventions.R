library(socialmixr)
library(epidemics)
library(tidyverse)

# (1) contact matrix ------------------------------------------------------

polymod <- socialmixr::polymod

contact_data <- socialmixr::contact_matrix(
  polymod,
  countries = "United Kingdom",
  age.limits = c(0, 15, 65),
  symmetric = TRUE
)

# prepare contact matrix
contact_matrix <- t(contact_data$matrix)

# (2) initial conditions --------------------------------------------------

# initial conditions: one in every 1 million is infected
initial_i <- 1e-6
initial_conditions <- c(
  S = 1 - initial_i, E = 0, I = initial_i, R = 0, V = 0
)

# build for all age groups
initial_conditions <- matrix(
  rep(initial_conditions, dim(contact_matrix)[1]),
  ncol = length(initial_conditions), byrow = TRUE
)
rownames(initial_conditions) <- rownames(contact_matrix)

initial_conditions

# (3) population structure ------------------------------------------------

# prepare the demography vector
demography_vector <- contact_data$demography$population
names(demography_vector) <- rownames(contact_matrix)

# prepare the population to model as affected by the epidemic
uk_population <- population(
  name = "UK",
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  initial_conditions = initial_conditions
)


# (4) model parameters ----------------------------------------------------

# parameters
preinfectious_period <- 4.0 #pre-infectious period -> 1/infectiousness rate
infectious_period <- 5.5 # -> 1/recovery rate
basic_reproduction <- 2.7

# rates
infectiousness_rate <- 1/preinfectious_period
recovery_rate <- 1/infectious_period
transmission_rate <-  basic_reproduction*recovery_rate

# NON-PHARMA --------------------------------------------------------------


# ON CONTACTS ----------------------------------------------------



# (5) intervention object -------------------------------------------------

## school closure ---------------------

rownames(contact_matrix)

close_schools <- intervention(
  name = "School closure",
  type = "contacts",
  time_begin = 50,
  time_end = 50 + 100,
  reduction = matrix(c(0.5, 0.01, 0.01))
)

#' [QUESTION] effect of intervention on contacts
#' https://epiverse-trace.github.io/tutorials-late/modelling-interventions.html#effect-of-interventions-on-contacts

# run {epidemics} ---------------------------------------------------------

output_school <- model_default(
  population = uk_population,
  transmission_rate = transmission_rate,
  infectiousness_rate = infectiousness_rate,
  recovery_rate = recovery_rate,
  intervention = list(contacts = close_schools),
  time_end = 300, increment = 1.0
)

output_school

# visualize effect --------------------------------------------------------

# run baseline simulation with no intervention
output_baseline <- model_default(
  population = uk_population,
  transmission_rate = transmission_rate,
  infectiousness_rate = infectiousness_rate,
  recovery_rate = recovery_rate,
  time_end = 300, increment = 1.0
)

output_baseline

# create intervention_type column for plotting
output_school$intervention_type <- "school closure"
output_baseline$intervention_type <- "baseline"
output <- rbind(output_school, output_baseline)

output %>% 
  glimpse()

ggplot(data = output[output$compartment == "infectious", ]) +
  aes(
    x = time,
    y = value,
    color = intervention_type,
    linetype = intervention_type
  ) +
  stat_summary(
    fun = "sum",
    geom = "line",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    x = "Simulation time (days)",
    y = "Individuals"
  ) +
  theme_bw(
    base_size = 15
  ) +
  geom_vline(
    xintercept = c(close_schools$time_begin, close_schools$time_end),
    colour = "black",
    linetype = "dashed",
    linewidth = 0.2
  ) +
  annotate(
    geom = "text",
    label = "Schools closed",
    colour = "black",
    x = (close_schools$time_end - close_schools$time_begin) / 2 +
      close_schools$time_begin,
    y = 10,
    angle = 0,
    vjust = "outward"
  )


# ON TRANSMISSION_RATE -------------------------------------------------------------

# (5) intervention object -------------------------------------------------

## mask mandate ---------------------

mask_mandate <- intervention(
  name = "mask mandate",
  type = "rate",
  time_begin = 40,
  time_end = 40 + 200,
  reduction = 0.163
)

# run {epidemics} ---------------------------------------------------------

output_masks <- model_default(
  population = uk_population,
  transmission_rate = 2.7 / 5.5,
  infectiousness_rate = 1.0 / 4.0,
  recovery_rate = 1.0 / 5.5,
  intervention = list(transmission_rate = mask_mandate),
  time_end = 300, increment = 1.0
)


# visualize effect --------------------------------------------------------

# create intervention_type column for plotting
output_masks$intervention_type <- "mask mandate"
output_baseline$intervention_type <- "baseline"
output <- rbind(output_masks, output_baseline)

ggplot(data = output[output$compartment == "infectious", ]) +
  aes(
    x = time,
    y = value,
    color = intervention_type,
    linetype = intervention_type
  ) +
  stat_summary(
    fun = "sum",
    geom = "line",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    x = "Simulation time (days)",
    y = "Individuals"
  ) +
  theme_bw(
    base_size = 15
  ) +
  geom_vline(
    xintercept = c(mask_mandate$time_begin, mask_mandate$time_end),
    colour = "black",
    linetype = "dashed",
    linewidth = 0.2
  ) +
  annotate(
    geom = "text",
    label = "Mask mandate",
    colour = "black",
    x = (mask_mandate$time_end - mask_mandate$time_begin) / 2 +
      mask_mandate$time_begin,
    y = 10,
    angle = 0,
    vjust = "outward"
  )


# PHARMA ------------------------------------------------------------------

# (5) vaccination object --------------------------------------------------

# prepare a vaccination object
vaccinate <- vaccination(
  name = "vaccinate all",
  time_begin = matrix(40, nrow(contact_matrix)),
  time_end = matrix(40 + 150, nrow(contact_matrix)),
  nu = matrix(c(0.01, 0.01, 0.01))
)


# run {epidemics} ---------------------------------------------------------

output_vaccinate <- model_default(
  population = uk_population,
  transmission_rate = 2.7 / 5.5,
  infectiousness_rate = 1.0 / 4.0,
  recovery_rate = 1.0 / 5.5,
  vaccination = vaccinate,
  time_end = 300, increment = 1.0
)

# visualize effect --------------------------------------------------------

# create intervention_type column for plotting
output_vaccinate$intervention_type <- "vaccination"
output_baseline$intervention_type <- "baseline"
output <- rbind(output_vaccinate, output_baseline)

ggplot(data = output[output$compartment == "infectious", ]) +
  aes(
    x = time,
    y = value,
    color = intervention_type,
    linetype = intervention_type
  ) +
  stat_summary(
    fun = "sum",
    geom = "line",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    x = "Simulation time (days)",
    y = "Individuals"
  ) +
  theme_bw(
    base_size = 15
  ) +
  geom_vline(
    xintercept = c(vaccinate$time_begin, vaccinate$time_end),
    colour = "black",
    linetype = "dashed",
    linewidth = 0.2
  ) +
  annotate(
    geom = "text",
    label = "Vaccinate",
    colour = "black",
    x = (vaccinate$time_end - vaccinate$time_begin) / 2 +
      vaccinate$time_begin,
    y = 10,
    angle = 0,
    vjust = "outward"
  )

# COMPARE -----------------------------------------------------------------

# create intervention_type column for plotting
output_vaccinate$intervention_type <- "vaccination"
output <- rbind(output_school, output_masks, output_vaccinate, output_baseline)

ggplot(data = output[output$compartment == "infectious", ]) +
  aes(
    x = time,
    y = value,
    color = intervention_type,
    linetype = intervention_type
  ) +
  stat_summary(
    fun = "sum",
    geom = "line",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(
    x = "Simulation time (days)",
    y = "Individuals"
  ) +
  theme_bw(
    base_size = 15
  )
