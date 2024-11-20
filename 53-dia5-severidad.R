
library(cfr)
library(epiparameter)
library(tidyverse)

packageVersion("cfr")

# ebola -------------------------------------------------------------------

ebola_delay <- epiparameter::epiparameter_db(
  disease = "ebola",
  epi_name = "onset-to-death",
  single_epiparameter = TRUE
)

# ebola 20 ----------------------------------------------------------------

ebola20 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/ebola_20days.rds")

cfr::cfr_static(data = ebola20)

cfr::cfr_static(
  data = ebola20,
  delay_density = function(x) density(ebola_delay, x)
)

# ebola 35 ----------------------------------------------------------------

ebola35 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/ebola_35days.rds")

cfr::cfr_static(data = ebola35)

cfr::cfr_static(
  data = ebola35,
  delay_density = function(x) density(ebola_delay, x)
)

# ebola 60 ----------------------------------------------------------------

ebola60 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/ebola_60days.rds")

cfr::cfr_static(data = ebola60)

cfr::cfr_static(
  data = ebola60,
  delay_density = function(x) density(ebola_delay, x)
)


# rolling ebola -----------------------------------------------------------------

ebola_rolling_naive <- cfr::cfr_rolling(data = ebola60)

ebola_rolling_adjusted <- cfr::cfr_rolling(
  data = ebola60,
  delay_density = function(x) density(ebola_delay, x)
)

# bind by rows both output data frames
bind_rows(
  ebola_rolling_naive %>%
    mutate(method = "naive"),
  ebola_rolling_adjusted %>%
    mutate(method = "adjusted")
) %>%
  # visualise both adjusted and unadjusted rolling estimates
  ggplot() +
  geom_ribbon(
    aes(
      date,
      ymin = severity_low,
      ymax = severity_high,
      fill = method
    ),
    alpha = 0.2, show.legend = FALSE
  ) +
  geom_line(
    aes(date, severity_estimate, colour = method)
  )

# covid -------------------------------------------------------------------

covid_delay <- epiparameter::epiparameter_db(
  disease = "covid",
  epi_name = "onset-to-death",
  single_epiparameter = TRUE
)

# covid 30 ----------------------------------------------------------------

covid30 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/covid_30days.rds") %>% 
  dplyr::select(date, cases = confirm, deaths = secondary)

cfr::cfr_static(data = covid30)

cfr::cfr_static(
  data = covid30,
  delay_density = function(x) density(covid_delay, x)
)

# covid 35 ----------------------------------------------------------------

covid35 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/covid_35days.rds") %>% 
  dplyr::select(date, cases = confirm, deaths = secondary)

cfr::cfr_static(data = covid35)

cfr::cfr_static(
  data = covid35,
  delay_density = function(x) density(covid_delay, x)
)

# covid 60 ----------------------------------------------------------------

covid60 <- read_rds("https://epiverse-trace.github.io/tutorials-middle/data/covid_60days.rds") %>% 
  dplyr::select(date, cases = confirm, deaths = secondary)

cfr::cfr_static(data = covid60)

cfr::cfr_static(
  data = covid60,
  delay_density = function(x) density(covid_delay, x)
)



# rolling covid -------------------------------------------------------------------

covid_rolling_naive <- cfr::cfr_rolling(data = covid60)

covid_rolling_adjusted <- cfr::cfr_rolling(
  data = covid60,
  delay_density = function(x) density(covid_delay, x)
)

# bind by rows both output data frames
bind_rows(
  covid_rolling_naive %>%
    mutate(method = "naive"),
  covid_rolling_adjusted %>%
    mutate(method = "adjusted")
) %>%
  # visualise both adjusted and unadjusted rolling estimates
  ggplot() +
  geom_ribbon(
    aes(
      date,
      ymin = severity_low,
      ymax = severity_high,
      fill = method
    ),
    alpha = 0.2, show.legend = FALSE
  ) +
  geom_line(
    aes(date, severity_estimate, colour = method)
  )
