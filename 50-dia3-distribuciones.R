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


