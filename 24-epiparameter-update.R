
library(epiparameter)
library(tidyverse)

# explore -----------------------------------------------------------------

# it is not case sensitive
epiparameter::epidist_db(disease = "eBola") %>%
  epiparameter::list_distributions() %>%
  as_tibble()

epiparameter::epidist_db(epi_dist = "incubation") %>%
  epiparameter::list_distributions() %>%
  as_tibble()

epiparameter::epidist_db(epi_dist = "Generation") %>%
  epiparameter::list_distributions() %>%
  as_tibble()

epiparameter::epidist_db(epi_dist = "serial") %>%
  epiparameter::list_distributions() %>%
  as_tibble()

# questions ---------------------------------------------------------------

# what variables are visible?
epiparameter::epidist_db() %>%
  epiparameter::list_distributions() %>%
  as_tibble() %>%
  glimpse()

# what diseases
epiparameter::epidist_db() %>%
  epiparameter::list_distributions() %>%
  dplyr::count(disease)

# what distributions
epiparameter::epidist_db() %>%
  epiparameter::list_distributions() %>%
  dplyr::count(epi_distribution)

# what diseases with serial
epiparameter::epidist_db(
  epi_dist = "serial"
) %>%
  epiparameter::list_distributions() %>%
  dplyr::count(disease)

# what distributions for ebola
epiparameter::epidist_db(
  disease = "ebola"
) %>%
  epiparameter::list_distributions() %>%
  dplyr::count(epi_distribution)

# what types of probability distribution
epiparameter::epidist_db(
  disease = "ebola"
) %>%
  epiparameter::list_distributions() %>%
  dplyr::count(prob_distribution)

# all distributions for ebola and serial
epiparameter::epidist_db(
  disease = "ebola",
  epi_dist = "serial"
)

# filter by sample size (by what other parameters?)
epiparameter::epidist_db(
  disease = "ebola",
  epi_dist = "serial",
  subset = sample_size > 300
)

# reprex ------------------------------------------------------------------

library(epiparameter)
library(tidyverse)

# one distribution
epidist_ebola_si <-
  epiparameter::epidist_db(
    disease = "ebola",
    epi_dist = "serial",
    subset = sample_size > 300
  )

# Read epidist class object
# Read distribution: gamma
epidist_ebola_si %>% class()
epidist_ebola_si

# from epidist to distcrete class
epidist_ebola_si_discrete <- epiparameter::discretise(epidist_ebola_si)

# Read epidist class object
# Read distribution: discrete gamma
epidist_ebola_si_discrete %>% class()
epidist_ebola_si_discrete

# distcrete class object
# Read distribution: discrete gamma
epidist_ebola_si_discrete$prob_dist %>% class()
epidist_ebola_si_discrete$prob_dist


# continuous distribution -------------------------------------------------

epidist_ebola_si
epidist_ebola_si$prob_dist # how can I read this? rpta: shape and rate
# distributional::dist_gamma(shape = 2.188,scale = 6.490)
1/6.490 # the rate is the inverse of the rate
distributional::dist_gamma(shape = 2.2,rate = 1/6.490)
epidist_ebola_si$uncertainty
epidist_ebola_si$summary_stats
epidist_ebola_si$summary_stats$mean # arithmetic mean


## set of distribution functions -------------------------------------------

plot(epidist_ebola_si,day_range = 0:50)
stats::density(epidist_ebola_si,at = 10)
epiparameter::cdf(epidist_ebola_si,q = 10)
# quantile at 0.5 cumulative probability
stats::quantile(epidist_ebola_si,p = 0.5) # geometric mean
epiparameter::generate(epidist_ebola_si,times = 10)

epiparameter::generate(epidist_ebola_si,times = 1000) %>%
  enframe() %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 10)

epiparameter::generate(epidist_ebola_si,times = 1000) %>%
  enframe() %>%
  ggplot(aes(value)) +
  geom_density()

# discrete distribution ---------------------------------------------------

#' all under the $ operator

epidist_ebola_si_discrete$summary_stats

## set of distribution functions -------------------------------------------
plot(epidist_ebola_si_discrete,day_range = 0:50)
# generate random values
epidist_ebola_si_discrete$prob_dist$r(1000)
# in what quantile we have the 99.9% cumulative probability?
x_limit <- epidist_ebola_si_discrete$prob_dist$qf(0.999)
# generate quantile values as a sequence for each natural number
si_discrete_x <- seq(1L, to = x_limit, by = 1L)
# calculate the values for each quantiles in the density function
si_discrete_y <- epidist_ebola_si_discrete$prob_dist$d(si_discrete_x)

tibble(
  quantile_values = si_discrete_x,
  density_values = si_discrete_y
) %>%
  ggplot(aes(quantile_values,density_values)) +
  geom_col()

# my questions! -----------------------------------------------------------

# [x] ask: how to extract lnorm? ----------------------------------------------

# epiparameter::epidist_db(
#   disease = "covid",
#   epi_dist = "serial",
#   subset = prob_distribution == "lnorm")

library(epiparameter)
library(tidyverse)

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "serial",
  author = "Nishiura") %>%
  epiparameter::list_distributions()

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "serial",
  author = "Nishiura",
  subset = prob_distribution == "lnorm")

epiparameter::epidist_db(disease = "covid",
                         epi_dist = "serial",
                         author = "Nishiura",
                         single_epidist = T)


# [x] ask: how to get a single set not using single_epidist? ------------------

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  author = "McAloon"
)

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  author = "McAloon"
) %>% 
  epiparameter::list_distributions() %>% 
  as_tibble()

epiparameter::epidist_db(
  disease = "SARS",
  epi_dist = "offspring_distribution",
  subset = sample_size > 40
)

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  author = "McAloon"
)

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "incubation",
  author = "McAloon",
  subset = sample_size > 5
)

# [x] ask: how to get mean_sd and sd_sd for uncertain epinow2? ----------------------

#' if
#' sem = standard error of the mean
#' sd = standard deviation
#' n = sample size
#' 1.96 = critical value for a significant level of 5% from qnorm(p = 0.975)
#' then
#' sem = sd / sqrt(n)
#' and
#' 95%ci = mean +- 1.96*sem
#' where
#' precision = 1.96*sem
#'
#' thus, if we have the 95% ci width (mean_ci_width)
#' mean_ci_width = 2 * precision
#' mean_ci_width = 2 * 1.96 * sem
#' mean_ci_width = 2 * 1.96 * sd / sqrt(n)
#' we have
#' sd = sqrt(n) * (mean_ci_width / (2 * 1.96))

# [*] ask: how to get access to the sample size? -------------------------------

#' for the uncertain epinow estimation,
#' we can add mean_sd and sd_sd
#' for mean_sd I followed this steps
#' and reference https://training.cochrane.org/handbook/current/chapter-06#section-6-3
#' how to get sd_sd?
#' reference: https://stats.stackexchange.com/questions/631/standard-deviation-of-standard-deviation
#'

epiparameter::epidist_db(
  disease = "covid",
  epi_dist = "serial",
  author = "Nishiura"
)

covid_lnorm <- 
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "serial",
    author = "Nishiura",
    single_epidist = T
  )

covid_lnorm

# mean
covid_lnorm$summary_stats$mean

# mean_ci width
covid_lnorm$summary_stats$mean_ci
covid_lnorm$summary_stats$mean_ci_limits
mean_ci_limits_num <- covid_lnorm$summary_stats$mean_ci_limits
mean_ci_width <- mean_ci_limits_num[2] - mean_ci_limits_num[1]
mean_ci_width

# from paper
covid_lnorm_sample <- 28 
stats::qt(p = 0.975,df = covid_lnorm_sample-1)
stats::qt(p = 0.025,df = covid_lnorm_sample-1)
t_095 <- stats::qt(p = 0.975,df = covid_lnorm_sample-1)

# mean_sd
covid_lnorm_mean_sd <- sqrt(covid_lnorm_sample) * (mean_ci_width / 2*t_095)
covid_lnorm_mean_sd
