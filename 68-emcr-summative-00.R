
#' summative from tutorials-early

#' 
#' strategy: backward teaching
#' 
#' - "start with the cake" or "play first, then learn how to do it"
#' - generate groups
#' - request minimum rearrangements
#' - report what you find
#' - discuss within group
#' - discuss whole class

# 0 read --------------------------------------------------------------------

library(tidyverse)

## save this to the learner ------------------------------------------------

rawdata <- "https://epiverse-trace.github.io/tutorials-early/data/simulated_ebola_2.csv"

# dat <- readr::read_csv(rawdata)
dat <- rio::import(rawdata) %>% as_tibble()


## everyone discuss this ---------------------------------------------------

dat
#' 
#' task: list the cleaning task you would perform
#' 
#' - standardize column name
#' - convert numbers from character to numeric values (age)
#' - standardize categorical variable (gender)
#' - standardize missing values (status)
#' - standardize date columns (onset, sample)
#' - check sequence of dated events (onset -> sample)
#' - identify duplicated rows (V1 == 5)

# 1 clean and standardize ---------------------------------------------------

library(cleanepi)

## learner explore this piece of code --------------------------------------

dat_dictionary <- tibble::tribble(
  ~options,  ~values,     ~grp, ~orders,
       "1",   "male", "gender",      1L, # remove this line: how this affects the code downstream?
       "2", "female", "gender",      2L,
       "M",   "male", "gender",      3L,
       "F", "female", "gender",      4L,
       "m",   "male", "gender",      5L,
       "f", "female", "gender",      6L
)

dat %>%
  cleanepi::standardize_column_names() %>% 
  cleanepi::remove_constants() %>% # report: what changed? drop 2 columns
  cleanepi::remove_duplicates() %>% # report: what changed? drop 3 rows
  # cleanepi::print_report() # check: "Duplicates" and "Constant data" tabs
  cleanepi::replace_missing_values(na_strings = "") %>% 
  cleanepi::check_subject_ids(
    target_columns = "case_id",
    range = c(0, 15000)
  ) %>% 
  # cleanepi::print_report() # check: "Unexpected subject ids" tab > "Duplicated ids" tab
  cleanepi::standardize_dates(
    target_columns = c("date_onset"), # challenge: add "date_sample" to the vector
    timeframe = c(
      as.Date("2014-01-01"), as.Date("2015-12-01") # challenge: change the last date from year 2016 to 2015 -> how this affects the code downstream?
    ) 
  ) %>% 
  # cleanepi::print_report() # check: "Standadised dates" tab
  cleanepi::convert_to_numeric(
    target_columns = "age"
  ) %>% 
  cleanepi::clean_using_dictionary(
    dictionary = dat_dictionary
  ) %>% 
  # cleanepi::print_report() # explore all the tabs
  identity()



# 2 tag and validate --------------------------------------------------------

library(linelist)

## save this to the learner ------------------------------------------------

dat_cleaned <- dat %>% 
  cleanepi::standardize_column_names() %>% 
  cleanepi::remove_constants() %>% 
  cleanepi::remove_duplicates() %>% 
  # cleanepi::print_report()
  cleanepi::replace_missing_values(na_strings = "") %>% 
  cleanepi::check_subject_ids(
    target_columns = "case_id",
    range = c(0,15000)
  ) %>% 
  # cleanepi::print_report()
  cleanepi::standardize_dates(
    target_columns = c(
      "date_onset",
      "date_sample"
    ),
    # detects out of range dates [but NOT filter]
    timeframe = c(as.Date("2014-01-01"), as.Date("2016-12-01"))
  ) %>% 
  # cleanepi::print_report()
  cleanepi::convert_to_numeric(target_columns = "age") %>% 
  # cleanepi::check_date_sequence(
  #   target_columns = c(
  #     "date_onset",
  #     "date_sample"
  #   )
  # ) %>% 
  # cleanepi::print_report()
  cleanepi::clean_using_dictionary(dictionary = dat_dictionary) %>% 
  # cleanepi::timespan(
  #   target_column = "date_sample",
  #   end_date = Sys.Date(),
  #   span_unit = "years",
  #   span_column_name = "time_since_sampling_date",
  #   span_remainder_unit = "months"
  # )
  # categorize the age numerical variable [add as a challenge hint]
  dplyr::mutate(
    age_category = base::cut(
      x = age,
      breaks = c(0, 20, 35, 60, 100), # replace with max value if known
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>% 
  identity()

## learner explore this piece of code --------------------------------------

dat_cleaned %>% 
  linelist::make_linelist(
    id = "case_id",
    date_onset = "date_onset",
    gender = "gender",
    age = "age", # change to "age_category": how this affects the code downstream?
    date_reporting = "date_sample"
  ) %>% 
  linelist::validate_linelist() %>% 
  # dplyr::select(case_id, date_onset, gender) %>% activate this: how this affects the code downstream?
  linelist::tags_df()

linelist::get_lost_tags_action()
# linelist::lost_tags_action(action = "error") # activate this with dplyr::select(): how this affects the code downstream?

# 3 aggregate and visualize -------------------------------------------------

library(incidence2)

## save this to the learner ------------------------------------------------

dat_linelist <- dat_cleaned %>% 
  linelist::make_linelist(
    id = "case_id",
    date_onset = "date_onset",
    gender = "gender",
    age = "age", # change to "age_category": how this affects the code downstream?
    date_reporting = "date_sample",
    age_group = "age_category", allow_extra = TRUE
  ) %>% 
  linelist::validate_linelist(
    allow_extra = TRUE,
    ref_types = linelist::tags_types(
      age_group = c("factor"),
      allow_extra = TRUE
    )
  ) %>% 
  # dplyr::select(case_id, date_onset, gender) %>% activate this: how this affects the code downstream?
  linelist::tags_df()


## learner explore this piece of code --------------------------------------

dat_linelist %>% 
  incidence2::incidence(
    date_index = "date_onset", # challenge: change to "date_reporting"
    groups = "gender", # challenge: change to "age_group": how this affects the code downstream?
    interval = "day", # challenge: change to "week" or "quarter" or "epiweek" or "isoweek": how this affects the code downstream?
    complete_dates = TRUE # challenge: change to FALSE: how this affects the code downstream?
  ) %>% 
  incidence2:::plot.incidence2(
    fill = "gender", # challenge: change to "age_group": how this affects the code downstream?
    angle = 45, # challenge: change to 15 or 90
    n_breaks = 5, # challenge: change to 2 or 10
    title = "Daily incidence of cases by date of onset", # how this is affected by code upstream?
    legend = "bottom"
  )


# 4 (extra: descriptive table) ----------------------------------------------

library(compareGroups)

compareGroups(data = dat_cleaned) %>%
  createTable()
