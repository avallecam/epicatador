library(epiparameter)
library(EpiNow2)
library(tidyverse)

epidist_db(disease = "ebola") %>% 
  parameter_tbl() %>% 
  count(epi_distribution)

epidist_db(disease = "covid") %>% 
  parameter_tbl() %>% 
  count(epi_distribution)

# ebola 1 -------------------------------------------------------------------

epidist_db(disease = "ebola")

epidist_db(disease = "ebola", epi_dist = "serial") %>% 
  parameter_tbl()

ebola_serialint <- epidist_db(
  disease = "ebola",
  epi_dist = "serial",
  single_epidist = TRUE
)

plot(ebola_serialint)

# ebola 2 -----------------------------------------------------------------

ebola_serialint

ebola_serialint$summary_stats$mean
ebola_serialint$summary_stats$sd

ebola_generationtime <- EpiNow2::Gamma(
  mean = ebola_serialint$summary_stats$mean,
  sd = ebola_serialint$summary_stats$sd,
  max = 40
)

ebola_generationtime

# ebola 3 -----------------------------------------------------------------

# rezago de reporte con incertidumbre

# the time difference between the symptom onset and case report
# follows a uncertain log normal distribution
# The mean follows a normal distribution with mean = 4 and sd = 0.5.
# The standard deviation follows a normal distribution with mean = 1 and sd = 0.5.
# delimit the distribution with max = 5

# what is the name of the disease time period?

ebola_reportdelay <- EpiNow2::LogNormal(
  mean = EpiNow2::Normal(mean = 4,sd = 0.5),
  sd = EpiNow2::Normal(mean = 1, sd = 0.5),
  max = 5
)

ebola_reportdelay

# covid 1 -------------------------------------------------------------------

epidist_db(disease = "covid") %>% 
  parameter_tbl()

covid_serialint <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "serial",
  single_epidist = TRUE
)

plot(covid_serialint)


# covid 2 -----------------------------------------------------------------

covid_serialint$summary_stats$mean
covid_serialint$summary_stats$sd

covid_generationtime <- EpiNow2::LogNormal(
  mean = covid_serialint$summary_stats$mean,
  sd = covid_serialint$summary_stats$sd,
  max = 20
)

covid_generationtime

plot(covid_generationtime)

# covid 3 -----------------------------------------------------------------

# The reporting delay for COVID 

# the time difference between the symptom onset and case report
# follows an uncertain gamma distribution. 
# The mean follows a normal distribution with mean = 2 and sd = 0.5.
# The standard deviation follows a normal distribution with mean = 1 and sd = 0.5.
# limit the distribution with a max = 5

# what is the name of the disease time period?

covid_reportdelay <- EpiNow2::Gamma(
  mean = EpiNow2::Normal(mean = 2, sd = 0.5),
  sd = EpiNow2::Normal(mean = 1,sd = 0.5),
  max = 5
)

# results dia 3 -----------------------------------------------------------------

ebola_serialint
ebola_generationtime
ebola_reportdelay

# ebola 4 -----------------------------------------------------------------

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

# ebola 5 -----------------------------------------------------------------

# en lugar de inspeccion visual
# calcular el valor 99% percentil

ebola_incubationtime <- epiparameter::epidist_db(
  disease = "ebola",
  epi_dist = "incubation",
  single_epidist = TRUE
)

plot(ebola_incubationtime)

ebola_incubationtime_params <- epiparameter::get_parameters(ebola_incubationtime)

EpiNow2::Gamma(
  shape = ebola_incubationtime_params["shape"],
  scale = ebola_incubationtime_params["scale"]
)

# ebola 6 -----------------------------------------------------------------

ebola_incubationtime_max <- ebola_incubationtime %>% 
  epiparameter::discretise() %>% 
  quantile(p = 0.99)

ebola_incubationtime_epinow <- EpiNow2::Gamma(
  shape = ebola_incubationtime_params["shape"],
  scale = ebola_incubationtime_params["scale"],
  max = ebola_incubationtime_max
)

ebola35_epinow_delays <- EpiNow2::epinow(
  data = ebola35,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay + ebola_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)


# results dia 4 -----------------------------------------------------------

plot(ebola35_epinow)
plot(ebola35_epinow_delays)
summary(ebola35_epinow)
summary(ebola35_epinow_delays)

# ebola 60 ----------------------------------------------------------------

ebola60 <- read_rds("data/ebola_60days.rds") %>% 
  select(date, confirm = cases)

withr::local_options(base::list(mc.cores = 4))

ebola60_epinow <- EpiNow2::epinow(
  data = ebola60,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

ebola60_epinow_delays <- EpiNow2::epinow(
  data = ebola60,
  generation_time = EpiNow2::generation_time_opts(ebola_generationtime),
  delays = EpiNow2::delay_opts(ebola_reportdelay + ebola_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

plot(ebola60_epinow)
plot(ebola60_epinow_delays)
summary(ebola60_epinow)
summary(ebola60_epinow_delays)

# como se define el area con datos parciales?
ebola_generationtime

# covid 30 ----------------------------------------------------------------

covid_serialint
covid_generationtime
covid_reportdelay


# covid 4 -----------------------------------------------------------------

covid30 <- read_rds("data/covid_30days.rds") %>% 
  dplyr::select(date, confirm)

withr::local_options(base::list(mc.cores = 4))

covid30_epinow <- EpiNow2::epinow(
  data = covid30,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay)
)

plot(covid30_epinow)
summary(covid30_epinow)


# covid 5 -----------------------------------------------------------------

covid_incubationtime <- epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  single_epidist = TRUE
)

covid_incubationtime_params <- epiparameter::get_parameters(covid_incubationtime)

covid_incubationtime_params["meanlog"]
covid_incubationtime_params["sdlog"]

EpiNow2::LogNormal(
  meanlog = covid_incubationtime_params["meanlog"],
  sdlog = covid_incubationtime_params["sdlog"]
)


# covid 6 -----------------------------------------------------------------

covid_incubationtime_max <- covid_incubationtime %>% 
  epiparameter::discretise() %>% 
  quantile(p = 0.99)

covid_incubationtime_epinow <- EpiNow2::LogNormal(
  meanlog = covid_incubationtime_params["meanlog"],
  sdlog = covid_incubationtime_params["sdlog"],
  max = covid_incubationtime_max
)

covid30_epinow_delay <- EpiNow2::epinow(
  data = covid30,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay + covid_incubationtime_epinow)
)

# results dia 4 -----------------------------------------------------------

plot(covid30_epinow)
plot(covid30_epinow_delay)
summary(covid30_epinow)
summary(covid30_epinow_delay)

# covid 60 ----------------------------------------------------------------

covid60 <- read_rds("data/covid_60days.rds") %>% 
  dplyr::select(date, confirm)

withr::local_options(base::list(mc.cores = 4))

covid60_epinow <- EpiNow2::epinow(
  data = covid60,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

covid60_epinow_delays <- EpiNow2::epinow(
  data = covid60,
  generation_time = EpiNow2::generation_time_opts(covid_generationtime),
  delays = EpiNow2::delay_opts(covid_reportdelay + covid_incubationtime_epinow),
  stan = EpiNow2::stan_opts(samples = 1000,chains = 3)
)

plot(covid60_epinow)
plot(covid60_epinow_delays)
summary(covid60_epinow)
summary(covid60_epinow_delays)
