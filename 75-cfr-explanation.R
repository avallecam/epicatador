library(tidyverse)
library(cfr)

size <- 13

dat <- ebola1976 %>% 
  as_tibble() %>% 
  slice_head(n=size) %>% 
  select(date)

# 10 cases in day 1 -------------------------------------------------------

dat_1 <- dat %>% 
  mutate(cases = c(rep(10,1),rep(0,size-1)),
         deaths = c(rep(0,1),rep(0,size-1)))

dat_1 %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) dunif(x = seq(10), min = 0, max = 10)
  )

#' estimated outcomes
#' are calculated by 
#' the convolution of observed cases and the distribution
#' in other words,
#' the observed cases are distributed in time according to the distribution,
#' in other words,
#' the secondary observations (deaths) from the primary observations (cases)
#' are distributed in time according to
#' the probability of observing of observing them in time.
#' 
#' the observed cases in day 1 are multiplied by 
#' the probability (of observing them again as deaths) 
#' given by the delay distribution
#' 
#' u_t, the underestimation factor of the known outcomes (expected outcomes)
#' is the ratio from
#' cumsum of estimated outcomes / cumsum of observed cases

dat_1 %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  )

# add 10 cases in day 2 ---------------------------------------------------

dat_2 <- dat %>% 
  mutate(cases = c(rep(10,2),rep(0,size-2)),
         deaths = c(rep(0,1),rep(0,size-1)))

dat_2 %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  )

dat_2 %>% 
  cfr::cfr_rolling(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  )

# add one death -----------------------------------------------------------

dat_1d <- dat %>% 
  mutate(cases = c(rep(10,2),rep(0,size-2)),
         deaths = c(rep(0,1),rep(1,1),rep(0,size-2)))

dat_1d %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  )

# calculate the naive CFR and p_t -----------------------------------------

#' p_t is the estimator for CFR

dat_1d %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  ) %>% 
  mutate(naive = cumsum(deaths)/cumsum(cases)) %>% 
  mutate(p_t = naive/u_t)

# get 95%CI from the likelihood -------------------------------------------

dat_1d %>% 
  cfr::cfr_rolling(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  )

# condition: observed deaths <= expected outcomes -------------------------

dat_3d <- dat %>% 
  mutate(cases = c(rep(10,2),rep(0,size-2)),
         deaths = c(rep(0,1),rep(3,1),rep(0,size-2)))

dat_3d %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  ) %>% 
  mutate(naive = cumsum(deaths)/cumsum(cases)) %>% 
  mutate(p_t = naive/u_t)

dat_3d %>% 
  cfr::cfr_rolling(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  )


# let's pass the threshold: p_t > 1 --------------------------------------

dat_4d <- dat %>% 
  mutate(cases = c(rep(10,2),rep(0,size-2)),
         deaths = c(rep(0,1),rep(4,1),rep(0,size-2)))

dat_4d %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  ) %>% 
  mutate(naive = cumsum(deaths)/cumsum(cases)) %>% 
  mutate(p_t = naive/u_t)

dat_4d %>% 
  cfr::cfr_rolling(
    delay_density = function(x) dunif(x, min = 0, max = 10)
  )

#' we get NA