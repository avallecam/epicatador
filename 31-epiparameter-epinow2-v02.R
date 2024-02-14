
#' 
#' goal
#' get uncertainty inputs from epiparameter to epinow2
#' 
#' shared
#' https://github.com/epiverse-trace/epiparameter/discussions/218


# howto epiparameter uncertain to epinow2 ---------------------------------

library(epiparameter)
library(EpiNow2)

# if
# sem = standard error of the mean = standard deviation of the sample means distribution
# sd = standard deviation of the sample distribution
# ci = confidence intervals of the population mean
# n = sample size
# 1.96 = critical value for a significant level of 5% from qnorm(p = 0.975)
# then
# 95%ci = mean +- 1.96*sem
# precision = 1.96*sem
# 
# for the mean_sd in {EpiNow2}
# we need the standard deviation of the mean distribution
# represented as a Bayesian prior which is assumed to be normally distributed with a given sd
# then,
# for the mean_sd we can use the sem
# thus,
# with mean_ci_width as the 95% ci width of the mean
# mean_ci_width = 2 * precision
# mean_ci_width = 2 * 1.96 * sem
# we have
# sem = (mean_ci_width / (2 * 1.96))

covid_lnorm <-
  epiparameter::epidist_db(
    disease = "covid",
    epi_dist = "serial",
    author = "Nishiura",
    single_epidist = T
  )

# parameters for the delay as a lognormal distribution
get_parameters(covid_lnorm)

# summary statistics are normally distributed
covid_lnorm$summary_stats$mean
covid_lnorm$summary_stats$sd

# get the mean sd ---------------------------------------------------------

# mean_ci width
covid_lnorm$summary_stats$mean_ci
covid_lnorm$summary_stats$mean_ci_limits
mean_ci_limits_num <- covid_lnorm$summary_stats$mean_ci_limits
mean_ci_width <- mean_ci_limits_num[2] - mean_ci_limits_num[1]
mean_ci_width

# from paper
covid_lnorm_sample <- covid_lnorm$metadata$sample_size
covid_lnorm_sample
# stats::qt(p = 0.975,df = covid_lnorm_sample-1)
# stats::qt(p = 0.025,df = covid_lnorm_sample-1)
t_095 <- stats::qt(p = 0.975,df = covid_lnorm_sample-1)

# mean_sd
covid_lnorm_mean_sd <- (mean_ci_width / 2*t_095)
covid_lnorm_mean_sd

# get the sd sd -----------------------------------------------------------

# sd_sd
covid_lnorm$summary_stats$sd
sd_ci_limits_num <- covid_lnorm$summary_stats$sd_ci_limits
sd_ci_width <- sd_ci_limits_num[2] - sd_ci_limits_num[1]
sd_ci_width
covid_lnorm_sd_sd <- (sd_ci_width / 2*t_095)
covid_lnorm_sd_sd

# for epinow2 -------------------------------------------------------------

## discretise continuous distribution --------------------------------------

covid_lnorm_discrete <- 
  epiparameter::discretise(covid_lnorm)

covid_lnorm_discrete

## get maximum value from discrete distribution ----------------------------

covid_lnorm_discrete_max <- 
  covid_lnorm_discrete$prob_dist$q(p = 0.999)

covid_lnorm_discrete_max

## plug in to lognormal ----------------------------------------------------

serial_interval_covid_branch_lognormalmax <- 
  LogNormal(
    mean = Normal(
      mean = covid_lnorm$summary_stats$mean,
      sd = covid_lnorm_mean_sd
      ), 
    sd = Normal(
      mean = covid_lnorm$summary_stats$sd,
      sd = covid_lnorm_sd_sd
      ),
    max = covid_lnorm_discrete_max
  )

serial_interval_covid_branch_lognormalmax

# a lognormal with max value is expected to run for epinow()

# epinow -------------------------------------------------------------------

epinow_estimates_alt <- epinow(
  # cases
  reported_cases = example_confirmed[1:60],
  # delays
  generation_time = generation_time_opts(serial_interval_covid_branch_lognormalmax),
  # computation
  stan = stan_opts(
    cores = 4, samples = 1000, chains = 3,
    control = list(adapt_delta = 0.99)
  )
)

summary(epinow_estimates_alt)
plot(epinow_estimates_alt)