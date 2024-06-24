library(EpiNow2)
library(cfr)
library(tidyverse)

withr::local_options(list(mc.cores = 4))

# steps -------------------------------------------------------------------

#' 1. delays
#' 2. data

# covid delays ------------------------------------------------------------------

## generation time ------------------------------------------------

# get covid serial interval
covid_serialinterval_raw <-
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "serial",
    single_epidist = TRUE
  )

#plot(covid_serialinterval_raw)

covid_serialinterval_params <- epiparameter::get_parameters(covid_serialinterval_raw)

covid_serialinterval_discrete <- epiparameter::discretise(covid_serialinterval_raw)

covid_serialinterval_epinow <- EpiNow2::LogNormal(
  meanlog = covid_serialinterval_params["meanlog"],
  sdlog = covid_serialinterval_params["sdlog"],
  max = stats::quantile(covid_serialinterval_discrete, p = 0.99)
)

covid_serialinterval_epinow

## incubation period -----------------------------------------------

covid_incubation_raw <-
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "incubation",
    single_epidist = TRUE
  )

# plot(covid_incubation_raw)

covid_incubation_params <- epiparameter::get_parameters(covid_incubation_raw)

covid_incubation_discrete <- epiparameter::discretise(covid_incubation_raw)

covid_incubation_epinow <- EpiNow2::LogNormal(
  meanlog = covid_incubation_params["meanlog"],
  sdlog = covid_incubation_params["sdlog"],
  max = stats::quantile(covid_incubation_discrete, p = 0.99)
)

## reporting delay -------------------------------------------------

# mean = 2
# sd = 1
# uncertainty of 0.5 to each
# max = 5

# get meanlog from mean and sd
reporting_meanlog <- EpiNow2::convert_to_logmean(mean = 2, sd = 1)

# get sdlog from mean and sd
reporting_sdlog <- EpiNow2::convert_to_logsd(mean = 2, sd = 1)

# get max
reporting_epidist <- epiparameter::epidist(
  disease = "covid",
  epi_dist = "reporting delay",
  prob_distribution = "lnorm",
  prob_distribution_params = c(
    meanlog = reporting_meanlog,
    sdlog = reporting_sdlog
  )
)
epiparameter::discretise(reporting_epidist) %>% 
  stats::quantile(p = 0.99)

### fixed ----------------------------------------------------------

# create lognormal fixed
covid_reporting_fixed <- EpiNow2::LogNormal(
  meanlog = reporting_meanlog,
  sdlog = reporting_sdlog,
  max = 5
)

### uncertain ----------------------------------------------------------

# create lognormal with uncertainty
covid_reporting_uncertain <- EpiNow2::LogNormal(
  meanlog = EpiNow2::Normal(mean = reporting_meanlog, sd = 0.5),
  sdlog = EpiNow2::Normal(mean = reporting_sdlog, sd = 0.5),
  max = 5
)

# data 01 - 1:30 -----------------------------------------------------------------

data01_time01 <- EpiNow2::example_confirmed %>% 
  dplyr::slice(1:30)

## 00 delays -------------------------------------------------------

data01_time01_delays00 <- EpiNow2::epinow(
  # cases
  data = data01_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time01_delays00)

## 01 delays -------------------------------------------------------

data01_time01_delays01 <- EpiNow2::epinow(
  # cases
  data = data01_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time01_delays01)

## 02.1 delays fixed ---------------------------------------------------------------

data01_time01_delays021 <- EpiNow2::epinow(
  # cases
  data = data01_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow + covid_reporting_fixed),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time01_delays021)

## 02.2 delays uncertain ---------------------------------------------------------------

data01_time01_delays022 <- EpiNow2::epinow(
  # cases
  data = data01_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow + covid_reporting_uncertain),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time01_delays022)

# data 01 - 1:35 -----------------------------------------------------------------

data01_time03 <- EpiNow2::example_confirmed %>% 
  dplyr::slice(1:35)

data01_time03 %>% 
  ggplot(aes(x = date, y = confirm)) +
  geom_col()

## 00 delays -------------------------------------------------------

data01_time03_delays00 <- EpiNow2::epinow(
  # cases
  data = data01_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time03_delays00)

## 01 delays -------------------------------------------------------

data01_time03_delays01 <- EpiNow2::epinow(
  # cases
  data = data01_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time03_delays01)

## 02.1 delays fixed ---------------------------------------------------------------

data01_time03_delays021 <- EpiNow2::epinow(
  # cases
  data = data01_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow + covid_reporting_fixed),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time03_delays021)

## 02.2 delays uncertain ---------------------------------------------------------------

data01_time03_delays022 <- EpiNow2::epinow(
  # cases
  data = data01_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow + covid_reporting_uncertain),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time03_delays022)



# data 01 - 1:60 -----------------------------------------------------------------

data01_time02 <- EpiNow2::example_confirmed %>% 
  dplyr::slice(1:60)

## 00 delays -------------------------------------------------------

data01_time02_delays00 <- EpiNow2::epinow(
  # cases
  data = data01_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time02_delays00)

## 01 delays -------------------------------------------------------

data01_time02_delays01 <- EpiNow2::epinow(
  # cases
  data = data01_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time02_delays01)

## 02.1 delays fixed ---------------------------------------------------------------

data01_time02_delays021 <- EpiNow2::epinow(
  # cases
  data = data01_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow + covid_reporting_fixed),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time02_delays021)

## 02.2 delays uncertain ---------------------------------------------------------------

data01_time02_delays022 <- EpiNow2::epinow(
  # cases
  data = data01_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(covid_serialinterval_epinow),
  delays = EpiNow2::delay_opts(covid_incubation_epinow + covid_reporting_uncertain),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data01_time02_delays022)


# *** summary tables *** --------------------------------------------------

summary(data01_time01_delays00)
summary(data01_time01_delays01)
summary(data01_time01_delays021)
summary(data01_time01_delays022)

summary(data01_time03_delays00)
summary(data01_time03_delays01)
summary(data01_time03_delays021)
summary(data01_time03_delays022)

summary(data01_time02_delays00)
summary(data01_time02_delays01)
summary(data01_time02_delays021)
summary(data01_time02_delays022)

# ebola delays ------------------------------------------------------------------

## generation time ------------------------------

ebola_serialinterval_raw <- epiparameter::epidist_db(
  disease = "ebola",
  epi_dist = "serial",
  single_epidist = TRUE
)

# epiparameter:::plot.epidist(ebola_serialinterval_raw,cumulative = T, xlim = c(1,60))
# epiparameter:::plot.epidist(epiparameter::discretise(ebola_serialinterval_raw),cumulative = F, xlim = c(1,60))

ebola_serialinterval_param <- epiparameter::get_parameters(ebola_serialinterval_raw)

ebola_serialinterval_discrete <- epiparameter::discretise(ebola_serialinterval_raw)

ebola_serialinterval_epinow <- EpiNow2::Gamma(
  shape = ebola_serialinterval_param["shape"],
  scale = ebola_serialinterval_param["scale"],
  max = stats::quantile(ebola_serialinterval_discrete, p = 0.99)
)

# ebola_serialinterval_epinow
# plot(ebola_serialinterval_epinow)

## incubation time -----------------------------------------------------

ebola_incubation_raw <- epiparameter::epidist_db(
  disease = "ebola",
  epi_dist = "incubation",
  single_epidist = TRUE
)

# plot(ebola_incubation_raw)

ebola_incubation_param <- epiparameter::get_parameters(ebola_incubation_raw)

ebola_incubation_discrete <- epiparameter::discretise(ebola_incubation_raw)

# plot(ebola_incubation_discrete)

ebola_incubation_epinow <- EpiNow2::Gamma(
  shape = ebola_incubation_param["shape"],
  scale = ebola_incubation_param["scale"],
  max = stats::quantile(ebola_incubation_discrete, p = 0.99)
)

# ebola_incubation_epinow
# plot(ebola_incubation_epinow)

## reporting delay -------------------------------------------------

# mean = 4
# sd = 1
# uncertainty of 0.5 to each
# max = 6

# get meanlog from mean and sd
ebola_reporting_meanlog <- EpiNow2::convert_to_logmean(mean = 4, sd = 1)

# get sdlog from mean and sd
ebola_reporting_sdlog <- EpiNow2::convert_to_logsd(mean = 4, sd = 1)

# get max
ebola_reporting_epidist <- epiparameter::epidist(
  disease = "ebola",
  epi_dist = "reporting delay",
  prob_distribution = "lnorm",
  prob_distribution_params = c(
    meanlog = ebola_reporting_meanlog,
    sdlog = ebola_reporting_sdlog
  )
)
epiparameter::discretise(ebola_reporting_epidist) %>%
  stats::quantile(p = 0.99)

### fixed ----------------------------------------------------------

# create lognormal fixed
ebola_reporting_fixed <- EpiNow2::LogNormal(
  meanlog = ebola_reporting_meanlog,
  sdlog = ebola_reporting_meanlog,
  max = 6
)

ebola_reporting_fixed

### uncertain ----------------------------------------------------------

# create lognormal with uncertainty
ebola_reporting_uncertain <- EpiNow2::LogNormal(
  meanlog = EpiNow2::Normal(mean = ebola_reporting_meanlog, sd = 0.5),
  sdlog = EpiNow2::Normal(mean = ebola_reporting_meanlog, sd = 0.5),
  max = 6
)

ebola_reporting_uncertain

# data 02 - 1:20 -----------------------------------------------------------------

data02_time01 <- cfr::ebola1976 %>% 
  dplyr::select(date, confirm = cases) %>% 
  dplyr::slice(1:20)
  

data02_time01 %>% 
  ggplot(aes(x = date, y = confirm)) +
  geom_col()

## 00 delays -------------------------------------------------------

data02_time01_delays00 <- EpiNow2::epinow(
  # cases
  data = data02_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time01_delays00)

## 01 delays -------------------------------------------------------

data02_time01_delays01 <- EpiNow2::epinow(
  # cases
  data = data02_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time01_delays01)

## 02.1 delays fixed ---------------------------------------------------------------

data02_time01_delays021 <- EpiNow2::epinow(
  # cases
  data = data02_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow + ebola_reporting_fixed),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time01_delays021)

## 02.2 delays uncertain ---------------------------------------------------------------

data02_time01_delays022 <- EpiNow2::epinow(
  # cases
  data = data02_time01,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow + ebola_reporting_uncertain),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time01_delays022)

# data 02 - 1:35 -----------------------------------------------------------------

data02_time02 <- cfr::ebola1976 %>% 
  dplyr::select(date, confirm = cases) %>% 
  dplyr::slice(1:35)

data02_time02 %>% 
  ggplot(aes(x = date, y = confirm)) +
  geom_col()

## 00 delays -------------------------------------------------------

data02_time02_delays00 <- EpiNow2::epinow(
  # cases
  data = data02_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time02_delays00)

## 01 delays -------------------------------------------------------

data02_time02_delays01 <- EpiNow2::epinow(
  # cases
  data = data02_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time02_delays01)

## 02.1 delays fixed ---------------------------------------------------------------

data02_time02_delays021 <- EpiNow2::epinow(
  # cases
  data = data02_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow + ebola_reporting_fixed),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time02_delays021)

## 02.2 delays uncertain ---------------------------------------------------------------

data02_time02_delays022 <- EpiNow2::epinow(
  # cases
  data = data02_time02,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow + ebola_reporting_uncertain),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time02_delays022)


# data 02 - 1:60 -----------------------------------------------------------------

data02_time03 <- cfr::ebola1976 %>% 
  dplyr::select(date, confirm = cases) %>% 
  dplyr::slice(1:60)

data02_time03 %>% 
  ggplot(aes(x = date, y = confirm)) +
  geom_col()

## 00 delays -------------------------------------------------------

data02_time03_delays00 <- EpiNow2::epinow(
  # cases
  data = data02_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time03_delays00)

## 01 delays -------------------------------------------------------

data02_time03_delays01 <- EpiNow2::epinow(
  # cases
  data = data02_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time03_delays01)

## 02.1 delays fixed ---------------------------------------------------------------

data02_time03_delays021 <- EpiNow2::epinow(
  # cases
  data = data02_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow + ebola_reporting_fixed),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time03_delays021)

## 02.2 delays uncertain ---------------------------------------------------------------

data02_time03_delays022 <- EpiNow2::epinow(
  # cases
  data = data02_time03,
  # delays
  generation_time = EpiNow2::generation_time_opts(ebola_serialinterval_epinow),
  delays = EpiNow2::delay_opts(ebola_incubation_epinow + ebola_reporting_uncertain),
  # stan = EpiNow2::stan_opts(method = "vb")
  stan = EpiNow2::stan_opts(samples = 1000, chains = 2)
)

plot(data02_time03_delays022)


# ^^^ summary tables ^^^ ----------------------------------------------------------

summary(data02_time02_delays00)
summary(data02_time02_delays01)
summary(data02_time02_delays021)
summary(data02_time02_delays022)

summary(data02_time03_delays00)
summary(data02_time03_delays01)
summary(data02_time03_delays021)
summary(data02_time03_delays022)

# *** stop *** --------------------------------

# data 03 - 1:30 -----------------------------------------------------------------

data03_time01 <- read_csv("https://epiverse-trace.github.io/tutorials-middle/data/ebola_cases.csv")

## 00 delays -------------------------------------------------------

## 01 delays -------------------------------------------------------

## 02 delays ---------------------------------------------------------------


# data 03 - 1:60 -----------------------------------------------------------------

data03_time02 <- read_csv("https://epiverse-trace.github.io/tutorials-middle/data/ebola_cases.csv")

## 00 delays -------------------------------------------------------

## 01 delays -------------------------------------------------------

## 02 delays ---------------------------------------------------------------


# data 03 - 1:90 -----------------------------------------------------------------

data03_time02 <- read_csv("https://epiverse-trace.github.io/tutorials-middle/data/ebola_cases.csv")

## 00 delays -------------------------------------------------------

## 01 delays -------------------------------------------------------

## 02 delays ---------------------------------------------------------------


# stratified --------------------------------------------------------------

cfr::covid_data