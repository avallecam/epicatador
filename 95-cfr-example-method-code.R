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