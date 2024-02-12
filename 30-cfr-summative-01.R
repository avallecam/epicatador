
# [3] summative 03 ------------------------------------------------------------

#' packages:
#' 
#' incidence2 -> [epiparameter -> cfr]
#' 
#' incidence2 from linelist data
#' [https://github.com/epiverse-trace/cfr/issues/79]
#' 
#' questions:
#' - why the linelist approach was removed?
#' 
#' concepts:
#' 
#' - incidence
#' - right-censoring
#' - fill NA
#' - (evaluate concept dependency for incidence2)
#' 
#' useful for:
#' 
#' - tutorial episode 03

outbreaks::fluH7N9_china_2013 # able to convert to aggregated?

# load packages ----------------------------------------------------------------

library(outbreaks)
library(incidence2)
library(tidyverse)


# read data ---------------------------------------------------------------

eboladb <- outbreaks::ebola_sim_clean$linelist

# the outcome death is in a different variable
eboladb %>% as_tibble() %>% glimpse()


# convert to incidece2 ----------------------------------------------------

eboladb_incidence_date <- incidence2::incidence(
  x = eboladb,
  date_index = c("date_of_onset",
                 "date_of_outcome"),
  groups = "outcome"
)

eboladb_incidence_date


# [O] concept: right-censoring --------------------------------------------

# [O] concept: right-censoring --------------------------------------------


# [!!] question ----------------------------------------------------------------

#' must look at
#' https://github.com/epiverse-trace/cfr/issues/79

#' should we
# drop rows with [unknown outcome]
# to correct for right-censoring bias in denominator
# ??
# eboladb_incidence_date %>% 
#   filter(!is.na(outcome)) %>%
#   identity()

# Plot the current data to 
# identify what I need to remove
eboladb_incidence_date %>% 
  # filter(!is.na(outcome)) %>% 
  plot()

# eboladb_incidence_date %>% 
#   filter(!is.na(outcome)) %>% 
#   plot()

#' interesting output!
#' 
#' thanks to the figure I found what I need to keep or drop
#' 
#' I just identified what I need to remove
#' - keep the date of onset of deaths and recovers [cases]
#' - keep only the date of outcome of deaths [deaths]
#' 
#' this means to drop 
#' the counts for the date of outcome of the recovered patients

eboladb_incidence_date_filter <- eboladb_incidence_date %>% 
  
  # here is a difference between vignette and reconlearn practical
  # this filter is added
  # to account account for censoring bias
  # https://www.reconlearn.org/solutions/real-time-response-1.html
  
  # filter(!is.na(outcome)) %>% [blocked]
  filter(!(outcome == "Recover" & count_variable == "date_of_outcome"))

# [!!] continue -----------------------------------------------------------

eboladb_incidence_date_filter

# Verify that your data have the info you want
eboladb_incidence_date_filter %>% count(outcome,count_variable)

# Regroup to remove the "group" variable "outcome"
eboladb_incidence_date_filter_regroup <- incidence2::regroup(eboladb_incidence_date_filter)

eboladb_incidence_date_filter_regroup


# Prepare incidence2 to cfr
eboladb_incidence_date_filter_regroup_prepare <- 
  
  cfr::prepare_data(
    data = eboladb_incidence_date_filter_regroup,
    cases_variable = "date_of_onset",
    deaths_variable = "date_of_outcome",
    fill_NA = TRUE
  )

eboladb_incidence_date_filter_regroup_prepare %>% as_tibble()


# [!!] question ----------------------------------------------------------

#' when to filter NA?

naniar::vis_miss(eboladb_incidence_date_filter_regroup_prepare)

# [!!] continue -----------------------------------------------------------


# import delay distribution -----------------------------------------------

library(epiparameter)

epidist_db(
  epi_dist = "onset to death",
  disease = "ebola"
) %>% 
  list_distributions() %>% 
  count(epi_distribution)

onset_to_death_ebola <- 
  epidist_db(
    epi_dist = "onset to death",
    disease = "ebola",
    single_epidist = TRUE
  )

# estimate static cfr -----------------------------------------------------

#' now lets try the cfr function!
cfr::cfr_static(data = eboladb_incidence_date_filter_regroup_prepare)

#' now reuse the delay from above!
cfr::cfr_static(
  data = eboladb_incidence_date_filter_regroup_prepare,
  delay_density = function(x) density(onset_to_death_ebola, x)
)



# evaluate bias source ----------------------------------------------------

eboladb_incidence_date_filter %>% 
  # filter(!is.na(outcome)) %>% 
  group_by(outcome) %>% 
  mutate(cum_sum = cumsum(count)) %>% 
  ggplot(aes(x = date_index,y=cum_sum, color = outcome)) +
  geom_line() +
  # scale_y_log10()
  NULL

eboladb %>% 
  as_tibble() %>% 
  select(case_id,date_of_onset,outcome,date_of_outcome)

