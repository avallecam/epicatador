
#' solucionario dia 4
#' 
#' episodios
#' 
#' - cuantificar transmisibilidad
#' - usar funciones de distribucion para analisis 
#' 
#' contenido
#' 
#' 1. ebola 
#'    - ebola 35 dias (grupo 2)
#'    - ebola 60 dias (grupo 3)
#' 2. covid
#'    - covid 30 dias (grupo 1)
#'    - covid 60 dias (grupo 4)

# ebola -------------------------------------------------------------------


# ebola distributions -----------------------------------------------------------

library(epiparameter)
library(EpiNow2)
library(tidyverse)

# intervalo serial
ebola_serialint <- epiparameter_db(
  disease = "ebola",
  epi_name = "serial",
  single_epiparameter = TRUE
)

# tiempo generacional
ebola_generationtime <- EpiNow2::Gamma(
  mean = ebola_serialint$summary_stats$mean,
  sd = ebola_serialint$summary_stats$sd
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

# ebola livecoding --------------------------------------------------------------

# acceder a periodo de incubacion
ebola_incubationtime <- epiparameter::epiparameter_db(
  disease = "ebola",
  epi_name = "incubation",
  single_epiparameter = TRUE
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

# ebola 35 days -----------------------------------------------------------

ebola35 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/ebola_35days.rds") %>% 
  dplyr::select(date, confirm = cases)

# desafio 1 ---------------------------------------------------------------

withr::local_options(base::list(mc.cores = 4))

tictoc::tic()
ebola35_epinow <- EpiNow2::epinow(
  data = ebola35,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(ebola35_epinow)
summary(ebola35_epinow)

# desafio 2 ---------------------------------------------------------------

tictoc::tic()
ebola35_epinow_delays <- EpiNow2::epinow(
  data = ebola35,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay + ebola_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(ebola35_epinow)
plot(ebola35_epinow_delays)
summary(ebola35_epinow)
summary(ebola35_epinow_delays)


# ebola 60 days -----------------------------------------------------------

ebola60 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/ebola_60days.rds") %>% 
  dplyr::select(date, confirm = cases)

# desafio 1 ---------------------------------------------------------------

withr::local_options(base::list(mc.cores = 4))

tictoc::tic()
ebola60_epinow <- EpiNow2::epinow(
  data = ebola60,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(ebola60_epinow)
summary(ebola60_epinow)

# desafio 2 ---------------------------------------------------------------

tictoc::tic()
ebola60_epinow_delays <- EpiNow2::epinow(
  data = ebola60,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay + ebola_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(ebola60_epinow)
plot(ebola60_epinow_delays)
summary(ebola60_epinow)
summary(ebola60_epinow_delays)


# covid -------------------------------------------------------------------


# covid distributions -----------------------------------------------------

library(epiparameter)
library(EpiNow2)
library(tidyverse)

# intervalo serial
covid_serialint <- epiparameter::epiparameter_db(
  disease = "covid",
  epi_name = "serial",
  single_epiparameter = TRUE
)

# tiempo generacional
covid_generationtime <- EpiNow2::LogNormal(
  mean = covid_serialint$summary_stats$mean,
  sd = covid_serialint$summary_stats$sd
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

# covid live coding -------------------------------------------------------------

# acceder a periodo de incubacion
covid_incubationtime <- epiparameter::epiparameter_db(
  disease = "covid",
  epi_name = "incubation",
  single_epiparameter = TRUE
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

# covid 30 days -----------------------------------------------------------------

# read data
covid30 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/covid_30days.rds") %>% 
  dplyr::select(date, confirm)

# desafio 1 ---------------------------------------------------------------

withr::local_options(base::list(mc.cores = 4))

tictoc::tic()
covid30_epinow <- EpiNow2::epinow(
  data = covid30,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(covid30_epinow)
summary(covid30_epinow)


# desafio 2 ---------------------------------------------------------------

tictoc::tic()
covid30_epinow_delay <- EpiNow2::epinow(
  data = covid30,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay + covid_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(covid30_epinow)
plot(covid30_epinow_delay)
summary(covid30_epinow)
summary(covid30_epinow_delay)


# covid 60 days -----------------------------------------------------------------

# read data
covid60 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/covid_60days.rds") %>% 
  dplyr::select(date, confirm)

# desafio 1 ---------------------------------------------------------------

withr::local_options(base::list(mc.cores = 4))

tictoc::tic()
covid60_epinow <- EpiNow2::epinow(
  data = covid60,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(covid60_epinow)
summary(covid60_epinow)


# desafio 2 ---------------------------------------------------------------

tictoc::tic()
covid60_epinow_delay <- EpiNow2::epinow(
  data = covid60,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay + covid_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)
tictoc::toc()

plot(covid60_epinow)
plot(covid60_epinow_delay)
summary(covid60_epinow)
summary(covid60_epinow_delay)

