library(tidyverse)
library(cfr)

size <- 4

dat <- ebola1976 %>% 
  as_tibble() %>% 
  slice_head(n=size) %>% 
  select(date)


# distribution -----------------------------------------------------------

# Values and probabilities
x <- 0:3
# p <- c(0.2, 0.4, 0.3, 0.1)

ddist <- function(x) {
  stats::setNames(
    c(0.2, 0.4, 0.3, 0.1),
    0:3
  )[as.character(x)]
}

ddist(x)
# dunif(x, min = 0, max = 10)

# 10 cases in day 1 -------------------------------------------------------

dat_1 <- dat %>% 
  mutate(cases = c(5,7,4,3),
         deaths = c(1,1,1,1))

dat_1 %>% 
  cfr::estimate_outcomes(
    delay_density = function(x) {
      vals <- 0:3
      probs <- c(0.2, 0.4, 0.3, 0.1)
      out <- numeric(length(x))
      out[x %in% vals] <- probs[match(x[x %in% vals], vals)]
      out
    }
  ) %>% 
  mutate(naive = cumsum(deaths)/cumsum(cases)) %>% 
  mutate(p_t = naive/u_t)


# internals --------------------------------------------------------------

delay_density <- function(x) {
  vals <- 0:3
  probs <- c(0.2, 0.4, 0.3, 0.1)
  out <- numeric(length(x))
  out[x %in% vals] <- probs[match(x[x %in% vals], vals)]
  out
}

# function to density
ddist(x = 0)
ddist(x = 1)
ddist(x = 2)
ddist(x = 3)

# step by step --------------------------------------

# By Day 1, the expected outcomes are:
dat_1$cases[1] *
  ddist(0)

# By Day 2, the expected outcomes are:
dat_1$cases[1] *
  (ddist(1)) + 
  dat_1$cases[2] *
    (ddist(0))

# By Day 3, the expected outcomes are:
dat_1$cases[1] *
  (ddist(2)) +
  dat_1$cases[2] *
    (ddist(1)) +
  dat_1$cases[3] *
    (ddist(0))

# cfr:::estimate_outcomes() ------------------------------

cases <- dat_1$cases
pmf_vals <- delay_density(x = seq(from = 0, to = (length(cases)-1)))
length(pmf_vals)

# cfr:::.convolve_cases_pmfs() ------------------------------
# el rev() es porque el ultimo dia inicia en
# la distribucion del tiempo epidemiologico 
i = 2
cases[seq_len(i)]
rev(pmf_vals[seq_len(i)])
cases[seq_len(i)] * rev(pmf_vals[seq_len(i)])
sum(cases[seq_len(i)] * rev(pmf_vals[seq_len(i)]))

vapply(
  X = seq_along(cases),
  FUN = function(i) {
    # estimate expected number of outcomes
    sum(cases[seq_len(i)] * rev(pmf_vals[seq_len(i)]))
  },
  FUN.VALUE = numeric(1)
) #%>% 
  #cumsum() / cumsum(cases)

# last output resembles the step-by-step section


