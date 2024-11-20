
# covid -------------------------------------------------------------------

library(tidyverse)
library(epiparameter)
library(cfr)

# delay -------------------------------------------------------------------

covid_delay <- epiparameter::epiparameter_db(
  disease = "covid",
  epi_name = "onset-to-death",
  single_epiparameter = TRUE
)

# read data ---------------------------------------------------------------

incidence_class <- incidence2::covidregionaldataUK %>% 
  as_tibble() %>% 
  filter(region == "London") %>% 
  # preprocess missing values
  tidyr::replace_na(
    list(
      deaths_new = 0,
      cases_new = 0
    )
  ) %>%
  # compute the daily incidence
  incidence2::incidence(
    date_index = "date",
    counts = c("cases_new","deaths_new"),
    date_names_to = "date",
    complete_dates = TRUE
  ) %>% 
  # filter(date < ymd(20200401)) %>%
  # filter(date < ymd(20200415)) %>%
  # filter(date < ymd(20200701)) %>% 
  identity()


incidence_class %>% 
  plot()

incidence_class %>% 
  ggplot(aes(date,count)) +
  geom_col() +
  facet_grid(count_variable~.,scales = "free_y")

covid_incidence2 <- incidence_class %>% 
  cfr::prepare_data(
    cases_variable = "cases_new",
    deaths_variable = "deaths_new") %>% 
  as_tibble()

# covid_incidence2 <- incidence_class %>% 
#   pivot_wider(
#     id_cols = date,
#     names_from = count_variable,
#     values_from = count) %>% 
#   dplyr::select(date, cases = cases_new, deaths = deaths_new)

# rolling incidence2 ------------------------------------------------------

cfr::cfr_static(data = covid_incidence2)

cfr::cfr_static(
  data = covid_incidence2,
  delay_density = function(x) density(covid_delay, x)
)

covid_rolling_naive <- cfr::cfr_rolling(data = covid_incidence2)

covid_rolling_adjusted <- cfr::cfr_rolling(
  data = covid_incidence2,
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


# time-varying ------------------------------------------------------------

covid_varying_naive <- cfr::cfr_time_varying(data = covid_incidence2)

covid_varying_adjusted <- cfr::cfr_time_varying(
  data = covid_incidence2,
  delay_density = function(x) density(covid_delay, x)
)

# bind by rows both output data frames
bind_rows(
  covid_varying_naive %>%
    mutate(method = "naive"),
  covid_varying_adjusted %>%
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

