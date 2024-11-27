library(tidyverse)

# tutorials early cake ----------------------------------------------------

dat <- read_csv("https://epiverse-trace.github.io/tutorials-early/data/simulated_ebola_2.csv")

dat %>% 
  cleanepi::standardize_column_names() %>% 
  cleanepi::standardize_dates(
    target_columns = "date_onset"
  ) %>% 
  incidence2::incidence(
    date_index = "date_onset",
    groups = "status",
    interval = "week"
  ) %>% 
  plot(
    fill = "status",angle = 45
    )


# tutorials challenge -----------------------------------------------------

# dat <- read_csv("http://epiverse-trace.github.io/tutorials-early/data/covid_simulated_data.csv")
# dat %>% 
#   ggplot(aes(`Date onset`)) +
#   geom_histogram()

