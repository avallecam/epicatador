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
incidence_data <- incidence::incidence(linelist$date_of_onset)

# Extract parameter by disease, distribution, author
epidist_ebola <- 
  epiparameter:: (
    disease = "Ebola",
    epi_name = "serial_interval",
    single_epiparameter = TRUE
  )

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