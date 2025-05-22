
#' goal: simulate multiple interventions in heterogenious-mixing population
#' 
#' task: 
#' - identify how the input changes running step by step

# interventions with {epidemics} -----------------------------------------

# intervention
school_begin_early <- 100
school_begin_late <- 200
vaccine_begin_early <- 100
vaccine_begin_late <- 200

rownames(socialcontact_matrix)

close_schools <- epidemics::intervention(
  name = "School closure",
  type = "contacts",
  time_begin = school_begin_late,
  time_end = school_begin_late + 100,
  reduction = matrix(c(0.5, 0.01, 0.01))
)

close_schools


simulate_school <- epidemics::model_default(
  population = population_object,
  transmission_rate = basic_reproduction_number / infectious_period,
  infectiousness_rate = 1/ pre_infectious_period,
  recovery_rate = 1 / infectious_period,
  time_end = 800,
  increment = 1,
  # intervention
  intervention = list(contacts = close_schools)
)


infections_baseline <- epidemics::new_infections(
  data = simulate_baseline,
  by_group = FALSE
)

infections_school <- epidemics::new_infections(
  data = simulate_school,
  by_group = FALSE
)

# Assign scenario names
infections_baseline$scenario <- "Baseline"
infections_school$scenario <- "School closure"

# Combine the data from both scenarios
infections_baseline_school <- bind_rows(infections_baseline, infections_school)

infections_baseline_school %>%
  ggplot(aes(x = time, y = new_infections, colour = scenario)) +
  geom_line() +
  geom_vline(
    xintercept = c(close_schools$time_begin, close_schools$time_end),
    linetype = "dashed",
    linewidth = 0.2
  ) +
  scale_y_continuous(labels = scales::comma)


# Pharmaceutical intervention --------------------------------------------

# Vaccination ------------------------------------------------------------

# prepare a vaccination object
vaccinate <- epidemics::vaccination(
  name = "vaccinate all",
  time_begin = matrix(vaccine_begin_early, nrow(socialcontact_matrix)),
  time_end = matrix(vaccine_begin_early + 150, nrow(socialcontact_matrix)),
  nu = matrix(c(0.001, 0.001, 0.001))
)

vaccinate

# run {epidemics} ---------------------------------------------------------

simulate_vaccinate <- epidemics::model_default(
  population = population_object,
  transmission_rate = basic_reproduction_number / infectious_period,
  infectiousness_rate = 1/ pre_infectious_period,
  recovery_rate = 1 / infectious_period,
  time_end = 800,
  increment = 1,
  # intervention
  vaccination = vaccinate
)

# visualize effect --------------------------------------------------------

infections_baseline <- epidemics::new_infections(
  data = simulate_baseline,
  compartments_from_susceptible = "vaccinated",
  by_group = FALSE
)

infections_vaccinate <- epidemics::new_infections(
  data = simulate_vaccinate,
  compartments_from_susceptible = "vaccinated",
  by_group = FALSE
)

# Assign scenario names
infections_baseline$scenario <- "Baseline"
infections_vaccinate$scenario <- "Vaccinate"

# Combine the data from both scenarios
infections_baseline_vaccinate <- bind_rows(
  infections_baseline,
  infections_vaccinate
)

infections_baseline_vaccinate %>%
  ggplot(aes(x = time, y = new_infections, colour = scenario)) +
  geom_line() +
  geom_vline(
    xintercept = c(vaccinate$time_begin, vaccinate$time_end),
    linetype = "dashed",
    linewidth = 0.2
  ) +
  scale_y_continuous(labels = scales::comma)


# Combine interventions --------------------------------------------------

simulate_school_vaccine <- epidemics::model_default(
  population = population_object,
  transmission_rate = basic_reproduction_number / infectious_period,
  infectiousness_rate = 1/ pre_infectious_period,
  recovery_rate = 1 / infectious_period,
  time_end = 800,
  increment = 1,
  # intervention
  intervention = list(
    contacts = close_schools
  ),
  vaccination = vaccinate 
)


# visualize effect --------------------------------------------------------

infections_baseline <- epidemics::new_infections(
  data = simulate_baseline,
  by_group = FALSE
)

infections_school_vaccine <- epidemics::new_infections(
  data = simulate_school_vaccine,
  compartments_from_susceptible = "vaccinated",
  by_group = FALSE
)

# Assign scenario names
infections_baseline$scenario <- "Baseline"
infections_school_vaccine$scenario <- "School closure + Vaccinate"

# Combine the data from both scenarios
infections_baseline_schoolvaccine <- bind_rows(
  infections_baseline,
  infections_school_vaccine
)

infections_baseline_schoolvaccine %>%
  ggplot(aes(x = time, y = new_infections, colour = scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma)


# Compare interventions --------------------------------------------------

compare_interventions <- bind_rows(
  infections_baseline,
  infections_baseline_school,
  infections_baseline_vaccinate,
  infections_school_vaccine
)

compare_interventions %>%
  ggplot(aes(x = time, y = new_infections, colour = scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Simulation time (days)",
    y = "New infections",
    colour = "Scenario"
  )

#' next steps:
#' - tutorials late episode 4 https://epiverse-trace.github.io/tutorials-late/modelling-interventions.html
#' - package reference manual https://epiverse-trace.github.io/epidemics/
#' - package finalsize https://epiverse-trace.github.io/finalsize/