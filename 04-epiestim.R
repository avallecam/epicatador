# Load required packages
library(outbreaks)
library(incidence)
library(epiparameter)
library(EpiEstim)
library(tidyverse)

# Load the simulated Ebola outbreak data
data(ebola_sim_clean)

# Extract the first element of the list
linelist <- ebola_sim_clean$linelist

# Convert the data to an incidence object
incidence_data <- linelist %>% 
  incidence2::incidence(
    date_index = "date_of_onset",
    interval = 1,
    date_names_to = "dates",
    count_values_to = "I",
    complete_dates = TRUE
  ) %>% 
  select(-count_variable) %>% 
  mutate(dates = as.Date(dates))

incidence_data

# Extract parameter by disease, distribution, author
epidist_ebola <- 
  epiparameter::epiparameter_db(
    disease = "Ebola",
    epi_name = "serial_interval",
    single_epiparameter = TRUE
  )

epidist_ebola

# Estimate the time-varying reproduction number
epiestim_output <- estimate_R(
  incid = incidence_data, 
  method = "parametric_si",
  config = make_config(
    list(
      mean_si = epidist_ebola$summary_stats$mean,
      std_si = epidist_ebola$summary_stats$sd
    )
  )
)

# Plot the time-varying reproduction number
plot(epiestim_output)
