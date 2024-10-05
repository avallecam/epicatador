
# test {epikinetics} with tutorials-early pipeline
# pak::pak("seroanalytics/epikinetics@i6")

library(tidyverse)
library(cleanepi)
library(linelist)
library(incidence2)

#' data dictionary: https://seroanalytics.org/epikinetics/articles/data.html
#' reference paper: https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(24)00484-5/fulltext
#' location: https://github.com/seroanalytics/epikinetics/tree/main/inst

#' introduce messy data
#' - make all entries characters! 

# rawdata <- "data-raw/delta.csv"
rawdata <- "https://raw.githubusercontent.com/seroanalytics/epikinetics/refs/heads/main/inst/delta_full.rds"

dat <- read_csv(rawdata)

dat %>% glimpse()


# what is this data about? ------------------------------------------------

## what "titre type" means? -------------------------------------------------

#' In the time series, 
#' each subject had monthly serum measurements 
#' for three types of antigens ("titre_type").
#' 
#' Serum samples where challenged against Ancestral, Alpha and Delta antigens.
#' 
#' The column "value" measures the titre of 
#' the neutralizing effect of each sample against each antigen 

dat %>%
  dplyr::filter(pid == 2) %>% 
  dplyr::arrange(date) %>% 
  # select time invariant columns
  dplyr::select(
    pid, infection_history, exp_num, last_exp_date, last_vax_type,
    dplyr::everything()
  )

# explore the data --------------------------------------------------------

# 335 subjects where followed up
dat %>% count(pid)

# subject time-invariant data
dat_subject <- dat %>%
  dplyr::count(pid, infection_history, exp_num, last_exp_date, last_vax_type) %>% 
  mutate(exp_num = forcats::as_factor(exp_num)) %>% 
  mutate(last_vax_type = forcats::fct_infreq(last_vax_type))

# table 1: time-invariant columns
dat_subject %>% 
  compareGroups::compareGroups(
    data = .,
    formula = ~infection_history + exp_num + last_exp_date + last_vax_type 
  ) %>% 
  compareGroups::createTable()

# table 2: were vaccine type differently applied between naive and non-naive?
dat_subject %>% 
  compareGroups::compareGroups(
    data = .,
    formula = last_vax_type~infection_history,
    byrow = TRUE
  ) %>% 
  compareGroups::createTable(show.all = TRUE)

# cleanepi ----------------------------------------------------------------

# check sequence of events

dat_clean <- dat %>% 
  # arrange columns
  dplyr::select(
    pid, infection_history, exp_num, last_exp_date, last_vax_type,
    dplyr::everything()
  ) %>% 
  # arrange rows
  dplyr::arrange(
    pid, infection_history, exp_num, last_exp_date, last_vax_type, date
  ) %>% 
  # cleanepi
  cleanepi::check_date_sequence(
    target_columns = c("last_exp_date", "date")
  ) %>% 
  # cleanepi::print_report()
  cleanepi::timespan(
    target_column = "last_exp_date",
    end_date = "date",
    span_unit = "days",
    span_column_name = "delay_vax_obs",
    span_remainder_unit = "days"
    ) %>% 
  # extra wrangling
  mutate(
    titre_type = forcats::fct_relevel(titre_type,"Ancestral", "Alpha"),
    censored = forcats::as_factor(censored)
  )

dat_clean

# distribution of the time from the last vaccine to first observation
dat_clean %>% 
  group_by(pid) %>% 
  filter(date == min(date)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  ggplot(aes(delay_vax_obs)) + 
  geom_histogram(
    # binwidth = 5
  )


## what "censored" means? ----------------------------------------------------

# context: censored regression model
# the "value" as the outcome is censored above or below
# because the it was measured outside the limits of detection
# threshold limit below: 5
# threshold limit above: 2560

dat_clean %>% 
  ggplot(aes(value, fill = censored)) + 
  geom_histogram()

# vaccinations ------------------------------------------------------------

## by vaccine type ---------------------------------------------

dat_subject %>% 
  # aggregate
  incidence2::incidence(
    date_index = "last_exp_date", # change: "date" or "last_exp_date"
    groups = ("last_vax_type"), # change: "titre_type" or "infection_history" or "last_vax_type" or c("infection_history", "titre_type")
    interval = "month", # change: "day" or "week" or "epiweek" or "month"
    # complete_dates = TRUE, # relevant to downstream analysis [time-series data]
  ) %>% 
  # transform to cumulative per group (optional display)
  # incidence2::cumulate() %>% 
  # plot
  incidence2:::plot.incidence2(
    fill = "last_vax_type" # change: "infection_history", "titre_type", or "last_vax_type"
  )

# observations ------------------------------------------------------------

# by history-variants
# not required, this reflect the proportion of "infection_history" in the cohort

## by censored -----------------------------------------------

dat_clean %>% count(censored)

dat_clean %>% 
  incidence2::incidence(
    date_index = "date", # change: "date" or "last_exp_date"
    groups = "censored", # change: "censored" or "titre_type" or "infection_history" or "last_vax_type" or c("infection_history", "titre_type")
    interval = "month", # change: "day", "week", "month"
    # complete_dates = TRUE # relevant to downstream analysis [time-series data]
  ) %>% 
  incidence2:::plot.incidence2(
    fill = "censored" # change: "censored" or "infection_history", "titre_type", or "last_vax_type"
  )


