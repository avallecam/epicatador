library(tidyverse)
library(EpiEstim)

# pak::pak("mrc-ide/EpiEstim")
packageVersion("EpiEstim")

incid <- read_rds(file = "https://github.com/mrc-ide/EpiEstim/blob/master/vignettes/aggregated_data/UK_covid_cases.rds?raw=true")

dt <- 7L

mean_si <- 6.3
std_si <- 4.2
method <- "parametric_si"
config <- make_config(
  list(
    mean_si = mean_si,
    std_si = std_si
  )
)

output <- EpiEstim::estimate_R(
  incid = incid,
  dt = dt,
  recon_opt = "match",
  method = method,
  config = config
)

plot(output,"R")
