
# # if(!require("pak")) install.packages("pak")
# pak::pak("epiverse-trace/cfr")
# 
# # Also install R package {epiparameter} for epidemiological parameter values
# pak::pak("epiverse-trace/epiparameter")

# Load package
library(cfr)
library(epiparameter)
library(tidyverse)

# Load the Ebola 1976 data provided with the package
data("ebola1976")

#' lets look at it
ebola1976 %>% as_tibble()
#' this have the daily count of cases and deaths
#' can you plot a incidence curve of both
#' from an incidence2 object?

#' lets try
library(incidence2)
incidence(
  x = ebola1976, 
  date_index = "cases",
  interval = "day"
)
#' I got an error
#' I try to change cases by date
#' 
#' but wait!!!
#' 
#' I realise that this is not in tidy format
#' WRONG! it is not in linelist format
#' (because it is tidy, each row is a day, and each column are separate variables)
#' we do not have one patient per row
#' this is a collapsed or count-output-like data format
#' with number of cases and deaths
#' so it is already in a incidence2-like format
#' 
#' so the answer is:
#' - no, we can not do a plot from incidence2 because it is not a linelist data frame
ebola1976
#' 
#' QUESTION!
#' - is this a tidy data frame?
#' - is this a linelist data frame?
#' 
#' pay attention to this because it's relevant for using {incidence2}
#' 
#' the alternative to visualize this is

# pivot to long format
ebola1976_longer <- ebola1976 %>% 
  pivot_longer(
    cols = -date,
    names_to = "count_variable",
    values_to = "count_value"
  )

# explore with geom line
ebola1976_longer %>% 
  ggplot(aes(x = date, y = count_value)) +
  geom_line() +
  facet_wrap(vars(count_variable))

# lets explore the cheatsheet 
# from https://posit.co/resources/cheatsheets/
# to https://rstudio.github.io/cheatsheets/data-visualization.pdf
# explore with geom col, prefered
ebola1976_longer %>% 
  ggplot(aes(x = date, y = count_value)) +
  geom_col() +
  facet_wrap(vars(count_variable))

# epxlore with deaths over cases, prefered
ebola1976_longer %>% 
  mutate(count_variable = fct_relevel(count_variable,"deaths")) %>% 
  ggplot(aes(x = date, 
             y = count_value)) +
  geom_col(aes(fill = count_variable))

# let's explore the documentation
# in https://ggplot2.tidyverse.org/reference/facet_wrap.html
# explore with facet wrap with 2 rows 
ebola1976_longer %>% 
  mutate(count_variable = fct_relevel(count_variable,"deaths")) %>% 
  ggplot(aes(x = date, y = count_value)) +
  geom_col() +
  facet_wrap(vars(count_variable),nrow = 2)
  # facet_grid(vars(count_variable))

#' QUESTION!
#' why are we reading the epiparameters
#' we where reviewin the cfr package!
#' 
#' what is the general and specific step question?
#' 
#' general: what is the Overall severity of the 1976 Ebola outbreak?
#' 
#' - what do we need for this?
#' - one way to estimate the severity is by calculating the cfr
#' - the simplest approach is the ratio between deaths/cases
#' - however, this is not as strait forward
#' - the issue here is the delay from onset to report (censored bias)
#' - an also an issue is when your observation period is lower than the delay duration (truncation bias)
#' 
#' specific: ...
#' 

# read delay distribution for ebolavirus onset to death from {epiparameter}
# accesses parameters reported in https://doi.org/10.1016/S0140-6736(18)31387-4

# Load the library of parameters as an epiparam object
epiparam_all <- epiparam()

# what distributions are available in `epi_distribution` 
epiparam_all %>% count(epi_distribution) %>% arrange(epi_distribution)

# Filter by disease, epi_distribution, region, and year
epiparam_all %>% 
  filter(str_detect(disease,"Ebola")) %>% # useful, disease names can change
  filter(epi_distribution == "onset_to_death") %>% 
  as_tibble() %>% 
  view()
  # filter(region == "West Africa") %>% 
  # filter(year == 2015)

#' wait,
#' I'm surprised by the sample size of this estimate
#' lets take a look to it
epiparam_all %>% 
  filter(str_detect(disease,"Ebola")) %>% # useful, disease names can change
  filter(epi_distribution == "onset_to_death") %>% 
  filter(author == "WHO_Ebola_Response_Team") %>% 
  as_epidist()
#' use the doi for googling or
#' https://www.doi2bib.org/bib/10.1056/NEJMc1414992
#' found in table 1

#' lets continue with the vignette
onset_to_death_ebola_epiparam <- epiparam_all %>% 
  filter(str_detect(disease,"Ebola")) %>% # useful, disease names can change
  filter(epi_distribution == "onset_to_death") %>% 
  filter(author == "Barry_etal")

#' now we have only one entry
#' viable for converting 
#' a epiparam to epidist class object
onset_to_death_ebola_epiparam

onset_to_death_ebola <- as_epidist(onset_to_death_ebola_epiparam)
onset_to_death_ebola

#' let's follow for a sec the message
get_citation(onset_to_death_ebola)

#' let's continue
#' 
#' now, let's focus on how to estimate the severity with the cfr 

# question: how to estimate the overall case fatality ratio corrected for delays? -------------------------

# Calculate the static CFR without correcting for delays
estimate_static(data = ebola1976)

#' ok, my learning here is that
#' the input data frame must be a (date / cases / death) format
#' evaluate how to get here from 
#' - data frames in linelist format
#' - is incidence2 helpful to connect a linelist to cfr?
#' 

# Calculate the static CFR while correcting for delays
estimate_static(
  data = ebola1976,
  correct_for_delays = TRUE,
  epidist = onset_to_death_ebola
)


# [*] question: how to get a data input for cfr from a linelist? ----------

## 1 covid -----------------------------------------------------------------

# library(covidregionaldata)

# Load packages
library(incidence2)
library(tidyverse)
library(cfr)

# Load covid outbreak data
dat <- 
  incidence2::covidregionaldataUK %>% 
  as_tibble() %>% 
  # filter(magrittr::is_in(region,c("England", "Scotland", 
  #                                 "Northern Ireland", "Wales"))) %>% 
  # filter(date > lubridate::ymd(20200701)) %>%
  # filter(date < lubridate::ymd(20201101))
  # filter(date <= lubridate::ymd(20201231))
  filter(date <= "2020-12-31")

# Read covid outbreak data
dat

# Create a incidence2 object 
dat_cases_deaths <- 
  
  # use two variables in counts argument
  incidence2::incidence(
    x = dat, 
    counts = c("cases_new","deaths_new"),
    date_index = "date"
  ) %>% 
  
  # complete dates for all group combinations
  # missing counts filled with 0
  incidence2::complete_dates()

# Read incidence2 object
dat_cases_deaths

# Plot incidence2 object
plot(dat_cases_deaths)

# Plot only cases
dat_cases_deaths %>% 
  filter(count_variable=="cases_new") %>% 
  plot()

# Plot only deaths
dat_cases_deaths %>% 
  filter(count_variable=="deaths_new") %>% 
  plot()

# # pivot to wider
# dat_cases_deaths_wide <- 
#   dat_cases_deaths %>% 
#   pivot_wider(id_cols = "date_index",
#               names_from = "count_variable",
#               values_from = "count")
# 
# # Read 
# dat_cases_deaths_wide

# Test
# estimate_static(data = dat_cases_deaths_wide)
#' but with long a good warning message 
#' (evaluate to report if desired)
#' 

# Rearrange data to fit {cfr} data input format
uk_covid_cfr_input <- 
  
  dat_cases_deaths %>% 
  
  # Pivot to wider
  pivot_wider(id_cols = "date_index",
              names_from = "count_variable",
              values_from = "count") %>% 
  
  # Clean variable names
  rename(date = date_index,
         cases = cases_new,
         deaths = deaths_new)

uk_covid_cfr_input

#' simpler workflow

library(incidence2)
library(cfr)
library(tidyverse)

# Load covid outbreak data
dat <- 
  incidence2::covidregionaldataUK %>% 
  filter(date <= "2020-12-31")

# Create {cfr} input data from tidy data with 
# - dates as observations, and
# - counts as variables
uk_covid_cfr_input <- 
  
  # Create a incidence2 object
  # use two variables in counts argument
  incidence2::incidence(
    x = dat, 
    counts = c("cases_new","deaths_new"),
    date_index = "date"
  ) %>% 
  
  # Prepare incidence2 object to cfr input data
  cfr::prepare_data(
    cases_variable = "cases_new",
    deaths_variable = "deaths_new",
    fill_NA = TRUE
  ) %>% 
  
  as_tibble()

# Read {cfr} input data
# uk_covid_cfr_input

# Estimate naive case fatality ratio
cfr::estimate_static(data = uk_covid_cfr_input)

#' leverage the ecosystem

dat <- 
  incidence2::covidregionaldataUK

# USED
aggregate( 
  data = dat, 
  x = cbind(cases_new, deaths_new) ~ date, 
  FUN = function(x) sum(x, na.rm = TRUE) 
) %>% 
  as_tibble() %>% 
  rename(
    cases = cases_new, deaths = deaths_new
  ) %>% 
  filter(date <= "2020-12-31")

# ALTERNATIVE 01
dat %>%
  group_by(date) %>%
  summarise(cases_new = sum(cases_new, na.rm = TRUE),
            deaths_new = sum(deaths_new, na.rm = TRUE)) %>% 
  rename(
    cases = cases_new, deaths = deaths_new
  ) %>% 
  filter(date <= "2020-12-31")


# ALTERNATIVE 02
# Create a incidence2 object
# use two variables in counts argument
incidence2::incidence(
  x = dat, 
  counts = c("cases_new","deaths_new"),
  date_index = "date"
) %>% 
  # Prepare incidence2 object to cfr input data
  cfr::prepare_data(
    cases_variable = "cases_new",
    deaths_variable = "deaths_new",
    fill_NA = TRUE
  ) %>% 
  
  filter(date <= "2020-12-31") %>% 
  
  as_tibble()


## 2 ebola -----------------------------------------------------------------------

# test now with ebola

library(outbreaks)
library(incidence2)
library(tidyverse)
eboladb <- outbreaks::ebola_sim_clean$linelist

# the outcome death is in a different variable
eboladb %>% as_tibble() %>% glimpse()

# Select relevant columns and 
# keep the patient-rows with known outcome
# to avoid censoring bias of denominator
# eboladb_filter <- eboladb %>% 
#   as_tibble() %>% 
#   select(case_id, date_of_onset,date_of_outcome,outcome) %>% 
#   filter(!is.na(outcome)) %>% 
#   arrange(case_id)

#' issue found!
#' the ebola data frame is an scenario
#' for a linelist data frame
#' usually from outbreak research
#' while
#' the covid data frame is an scenario
#' for a non-linelist data frame
#' with counts per day of report
#' usually from WHO or health office outcome (anonymized)
#' 
#' now, how to solve this?

# eboladb_filter
# 
# incidence2::incidence(
#   x = eboladb_filter, 
#   date_index = c("date_of_onset","date_of_outcome"),
#   interval = "day",
#   groups = "outcome"
# )

#' try code from documentation

eboladb_incidence_date <- incidence2::incidence(
  x = eboladb,
  date_index = c("date_of_onset","date_of_outcome"),
  groups = "outcome"
)

eboladb_incidence_date

# keep the patient-rows with known outcome
# to avoid censoring bias of denominator
eboladb_incidence_date %>% 
  filter(!is.na(outcome)) %>%
  identity()

# Plot the current data to 
# identify what I need to remove
eboladb_incidence_date %>% 
  filter(!is.na(outcome)) %>% 
  plot()

#' interesting output!
#' 
#' thanks to the figure I found what I neede to keep or drop
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
  
  filter(!is.na(outcome)) %>%
  filter(!(outcome == "Recover" & count_variable == "date_of_outcome"))

eboladb_incidence_date_filter

# Verify that your data have the info you want
eboladb_incidence_date_filter %>% count(outcome,count_variable)

# Regroup to remove the "group" variable "outcome"
eboladb_incidence_date_filter_regroup <- incidence2::regroup(eboladb_incidence_date_filter)

eboladb_incidence_date_filter_regroup

# Verify that your data have the info you want
eboladb_incidence_date_filter_regroup %>% count(count_variable)

# wide the data frame
eboladb_incidence_date_filter_regroup_wide <- 
  eboladb_incidence_date_filter_regroup %>% 
  pivot_wider(id_cols = date_index,
              names_from = count_variable,
              values_from = count)

eboladb_incidence_date_filter_regroup_wide

# but
# prefer to complete the data frame
# follow https://gist.github.com/avallecam/894e0e2f5db3ec17cf78d2e6d9b04bbd
# https://tidyr.tidyverse.org/reference/complete.html
# https://tidyr.tidyverse.org/reference/full_seq.html
# used https://github.com/avallecam/shiny-server/blob/3fb6c2ad781cc85c9486fcc18763cbb5a8b246e9/c19/covidearsa/index.Rmd#L127-L131C12
# used https://github.com/avallecam/shiny-server/blob/3fb6c2ad781cc85c9486fcc18763cbb5a8b246e9/vig/plscan/index.Rmd#L136-L139
eboladb_incidence_date_filter_regroup_complete <- 
  
  eboladb_incidence_date_filter_regroup %>% 
  
  complete(nesting(count_variable),
           date_index = full_seq(date_index, 1),
           fill = list(count=0)) %>% 
  
  pivot_wider(id_cols = date_index,
              names_from = count_variable,
              values_from = count) %>% 
  
  rename(date = date_index,
         cases = date_of_onset,
         deaths = date_of_outcome)

eboladb_incidence_date_filter_regroup_complete

# or

#' I recently discover 
#' incidence2 complete_dates function
#' this simplifies the complete step only

eboladb_incidence_date_filter_regroup %>% 
  
  complete_dates() %>% 
  
  pivot_wider(id_cols = date_index,
              names_from = count_variable,
              values_from = count) %>% 
  
  rename(date = date_index,
         cases = date_of_onset,
         deaths = date_of_outcome)


# or

# Prepare incidence2 to cfr
eboladb_incidence_date_filter_regroup_prepare <- 
  
  prepare_data(
    data = eboladb_incidence_date_filter_regroup,
    cases_variable = "date_of_onset",
    deaths_variable = "date_of_outcome",
    fill_NA = TRUE
  )

eboladb_incidence_date_filter_regroup_prepare %>% as_tibble()

#' test if both are equal

all.equal(as.data.frame(eboladb_incidence_date_filter_regroup_complete),
          eboladb_incidence_date_filter_regroup_prepare)

#' now lets try the cfr function!
estimate_static(data = eboladb_incidence_date_filter_regroup_complete)

#' now reuse the delay from above!
estimate_static(
  data = eboladb_incidence_date_filter_regroup_complete,
  correct_for_delays = TRUE,
  epidist = onset_to_death_ebola)

#' one additional QUESTION!
#' 
#' does the data frame must be a complete time series?
#' with values of 0 or higher for each day?
#' 
#' - answer: apparently yes!
#' 
#' sub question: how could I assess for it for the covid data example?

#' other QUESTION!
#' 
#' for which type of bias the 
#' "correct for delays" argument adjust for?
#' censoring bias
#' or
#' truncation bias?

### test vignette code ------------------------------------------------------

# load ebola dataset from outbreak
data("ebola_sim_clean")
ebola <- ebola_sim_clean$linelist

# view ebola
head(ebola)

# create incidence2 object of ebola deaths
ebola <- incidence(
  x = ebola,
  date_index = c(
    onset = "date_of_onset",
    outcome = "date_of_outcome"
  ),
  groups = "outcome"
)

# filter for outcomes that are deaths using dplyr::filter --- death counts
# also filter for all onsets --- these are the case counts
ebola <- filter(
  ebola,
  (outcome == "Death" & count_variable == "outcome") |
    (count_variable == "onset")
)

# remove groups using incidence2::regroup()
ebola <- regroup(ebola)

# prepare data
ebola <- prepare_data(
  ebola,
  cases_variable = "onset",
  deaths_variable = "outcome",
  fill_NA = TRUE
)

onset_to_death_ebola <- epidist_db(
  disease = "Ebola Virus Disease",
  epidist = "onset_to_death",
  author = "Barry_etal"
)

# estimate static CFR as a sanity check
estimate_static(
  ebola,
  correct_for_delays = TRUE, 
  epidist = onset_to_death_ebola
)


#### test compare ------------------------------------------------------------

head(ebola)
head(eboladb_incidence_date_filter_regroup_complete)

# [*] question: how to estimate a stratified case fatality ratio? ---------



# questions: how to estimate the reporting rate? --------------------------


# question: how to estimate the time-varying case fatality ratio? ---------


# [*] new questions -----------------------------------------------------------

#' how to determine if my data frame is a linelist?
#' 
#' compare the ebola and covid data frames
