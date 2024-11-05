#' 
#' https://github.com/epiforecasts/EpiNow2/issues/703
#' 
#' follow-up idea: keep directionality?

# exponential growth phase ------------------------------------------------

# get example case counts
# reported_cases <- example_confirmed[1:20]
# real time estimates
# summary(def)
#>                             measure              estimate
#>                              <char>                <char>
#> 1:           New infections per day 10485 (3194 -- 23660)
#> 2: Expected change in daily reports            Increasing
#> 3:       Effective reproduction no.      1.9 (1.3 -- 2.3)
#> 4:                   Rate of growth   0.2 (0.068 -- 0.29)
#> 5:     Doubling/halving time (days)       3.4 (2.4 -- 10)

#' inverse order:
#' 0.068 -> 10
#' 0.2 -> 3.4
#' 0.29 -> 2.4

library("ggplot2")

r <- seq(0.068, 0.29, by = 0.01)
dt <- log(2) / r

df <- data.frame(r = r, dt = dt)

ggplot(df, aes(x = r, y = dt)) +
  geom_point() +
  geom_vline(xintercept = 0.2, linetype = "dashed")

# near pick ---------------------------------------------------------------

# get example case counts
# reported_cases <- example_confirmed[1:60]
# real time estimates
# summary(def)
#>                             measure               estimate
#>                              <char>                 <char>
#> 1:           New infections per day     2309 (950 -- 5129)
#> 2: Expected change in daily reports      Likely decreasing
#> 3:       Effective reproduction no.      0.9 (0.59 -- 1.3)
#> 4:                   Rate of growth -0.03 (-0.15 -- 0.089)
#> 5:     Doubling/halving time (days)      -23 (7.8 -- -4.6)

#' order:
#' -0.15 -> -4.6
#' -0.03 -> -23
#' 0.089 -> 7.8

library("ggplot2")

r <- seq(-0.15, 0.089, by = 0.01)
dt <- log(2) / r

df <- data.frame(r = r, dt = dt)

ggplot(df, aes(x = r, y = dt)) +
  geom_point() +
  geom_vline(xintercept = -0.03, linetype = "dashed")


# decay phase -------------------------------------------------------------

#' https://github.com/avallecam/epicatador/blob/main/51-dia4-transmissibilidad-epinow2.R
# summary(ebola60_epinow_delays)
#>                             measure                estimate
#>                              <char>                  <char>
#> 1:           New infections per day              0 (0 -- 0)
#> 2: Expected change in daily reports              Decreasing
#> 3:       Effective reproduction no. 0.022 (0.00098 -- 0.21)
#> 4:                   Rate of growth -0.19 (-0.32 -- -0.048)
#> 5:     Doubling/halving time (days)      -3.7 (-14 -- -2.2)

#' inverse order:
#' -0.32 -> -2.2
#' -0.19 -> -3.7
#' -0.048 -> -14

library("ggplot2")

r <- seq(-0.32, -0.048, by = 0.01)
dt <- log(2) / r

df <- data.frame(r = r, dt = dt)

ggplot(df, aes(x = r, y = dt)) +
  geom_point() +
  geom_vline(xintercept = -0.19, linetype = "dashed")