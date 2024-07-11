
# ebola -------------------------------------------------------------------

library(epiparameter)
library(EpiNow2)
library(tidyverse)

# intervalo serial
ebola_serialint <- epidist_db(
  disease = "ebola",
  epi_dist = "serial",
  single_epidist = TRUE
)

# tiempo generacional
ebola_generationtime <- EpiNow2::Gamma(
  mean = ebola_serialint$summary_stats$mean,
  sd = ebola_serialint$summary_stats$sd,
  max = 40
)

# retraso de sintomas a reporte o notificacion
ebola_reportdelay <- EpiNow2::LogNormal(
  mean = EpiNow2::Normal(mean = 4,sd = 0.5),
  sd = EpiNow2::Normal(mean = 1, sd = 0.5),
  max = 5
)

# input necesario
ebola_reportdelay
ebola_generationtime


# desafio 1 ---------------------------------------------------------------

ebola35 <- read_rds("data/ebola_35days.rds") %>% 
  dplyr::select(date, confirm = cases)

withr::local_options(base::list(mc.cores = 4))

ebola35_epinow <- EpiNow2::epinow(
  data = ebola35,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

plot(ebola35_epinow)
summary(ebola35_epinow)


# livecoding --------------------------------------------------------------

# acceder a periodo de incubacion
ebola_incubationtime <- epiparameter::epidist_db(
  disease = "ebola",
  epi_dist = "incubation",
  single_epidist = TRUE
)

# extraer parametros
ebola_incubationtime_params <- epiparameter::get_parameters(ebola_incubationtime)

# discretizar distribucion y extraer valor maximo (p = 99%)
ebola_incubationtime_max <- ebola_incubationtime %>% 
  epiparameter::discretise() %>% 
  quantile(p = 0.99)

# usar periodo de incubacion en interfase de {EpiNow2}
ebola_incubationtime_epinow <- EpiNow2::Gamma(
  shape = ebola_incubationtime_params["shape"],
  scale = ebola_incubationtime_params["scale"],
  max = ebola_incubationtime_max
)


# desafio 2 ---------------------------------------------------------------

ebola35_epinow_delays <- EpiNow2::epinow(
  data = ebola35,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay + ebola_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

plot(ebola35_epinow)
plot(ebola35_epinow_delays)
summary(ebola35_epinow)
summary(ebola35_epinow_delays)


# covid -------------------------------------------------------------------

library(epiparameter)
library(EpiNow2)
library(tidyverse)

# intervalo serial
covid_serialint <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "serial",
  single_epidist = TRUE
)

# tiempo generacional
covid_generationtime <- EpiNow2::LogNormal(
  mean = covid_serialint$summary_stats$mean,
  sd = covid_serialint$summary_stats$sd,
  max = 20
)

# retraso de sintomas a reporte o notificacion
covid_reportdelay <- EpiNow2::Gamma(
  mean = EpiNow2::Normal(mean = 2, sd = 0.5),
  sd = EpiNow2::Normal(mean = 1,sd = 0.5),
  max = 5
)

# input necesario
covid_generationtime
covid_reportdelay

# desafio 1 ---------------------------------------------------------------


# read data
covid30 <- read_rds("data/covid_30days.rds") %>% 
  dplyr::select(date, confirm)


withr::local_options(base::list(mc.cores = 4))

covid30_epinow <- EpiNow2::epinow(
  data = covid30,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

plot(covid30_epinow)
summary(covid30_epinow)


# live coding -------------------------------------------------------------

# acceder a periodo de incubacion
covid_incubationtime <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  single_epidist = TRUE
)

# extraer parametros
covid_incubationtime_params <- epiparameter::get_parameters(covid_incubationtime)

# discretizar distribucion y extraer valor maximo (p = 99%)
covid_incubationtime_max <- covid_incubationtime %>% 
  epiparameter::discretise() %>% 
  quantile(p = 0.99)

# usar periodo de incubacion en interfase de {EpiNow2}
covid_incubationtime_epinow <- EpiNow2::LogNormal(
  meanlog = covid_incubationtime_params["meanlog"],
  sdlog = covid_incubationtime_params["sdlog"],
  max = covid_incubationtime_max
)

# desafio 2 ---------------------------------------------------------------

covid30_epinow_delay <- EpiNow2::epinow(
  data = covid30,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay + covid_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

plot(covid30_epinow)
plot(covid30_epinow_delay)
summary(covid30_epinow)
summary(covid30_epinow_delay)



