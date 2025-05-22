
#' goal: probability of a case originating from a cluster of infections
#' given the superspreading potential
#' 
#' task: 
#' - identify how the input changes running step by step

library(tidyverse)
library(epicontacts)
library(fitdistrplus)
library(superspreading)

mers_set <- outbreaks::mers_korea_2015

fit_estimates <- epicontacts::make_epicontacts(
  linelist = mers_set$linelist,
  contacts = mers_set$contacts,
  directed = TRUE
) %>%
  # epicontacts::vis_epicontacts()
  epicontacts::get_degree(
    type = "out",
    only_linelist = TRUE
  ) %>% 
  # tibble::enframe() %>% 
  # ggplot(aes(value)) +
  # geom_histogram(binwidth = 1)
  fitdistrplus::fitdist(distr = "nbinom")

set.seed(33)
superspreading::proportion_cluster_size(
  R = fit_estimates$estimate["mu"],
  k = fit_estimates$estimate["size"],
  cluster_size = c(5, 10, 25)
)

#' next steps:
#' - tutorials middle episode 6-7 https://epiverse-trace.github.io/tutorials-middle/superspreading-estimate.html
#' - package vignettes https://epiverse-trace.github.io/superspreading/