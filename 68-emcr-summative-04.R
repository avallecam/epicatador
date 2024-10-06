
#' goal:
#' case incidence stratified by age

# To Do -------------------------------------------------------------------

#' [O] add more rows


# summative ---------------------------------------------------------------

library(cleanepi)
library(linelist)
library(incidence2)
library(tidyverse)

dat <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))

# linelist::lost_tags_action(action = "error")
# linelist::lost_tags_action(action = "warning")

dat %>% 
  dplyr::as_tibble() %>% 
  
  # 1 clean and standardise
  
  # standardize column names and dates
  cleanepi::standardize_column_names() %>% 
  cleanepi::standardize_dates(
    target_columns = c("date_of_birth","date_first_pcr_positive_test")
  ) %>% 
  # replace from strings to a valid missing entry
  cleanepi::replace_missing_values(
    target_columns = "sex",
    na_strings = "-99") %>% 
  # calculate the age in 'years' and return the remainder in 'months'
  cleanepi::timespan(
    target_column = "date_of_birth",
    end_date = Sys.Date(),
    span_unit = "years",
    span_column_name = "age_in_years",
    span_remainder_unit = "months"
  ) %>% 
  # select to conveniently view timespan output
  dplyr::select(
    study_id,
    sex,
    date_first_pcr_positive_test, 
    date_of_birth,
    age_in_years
  ) %>% 
  # categorize the age numerical variable [add as a challenge hint]
  dplyr::mutate(
    age_category = base::cut(
      x = age_in_years,
      breaks = c(0,20,35,60,Inf), # replace with max value if known
      include.lowest = TRUE,
      right = FALSE
    )
    # age_category = Hmisc::cut2(x = age_in_years,cuts = c(20,35,60))
  ) %>% 
  dplyr::mutate(
    sex = as.factor(sex)
  ) %>% 
  
  # 2 validate linelist
  
  # tag variables
  linelist::make_linelist(
    id = "study_id",
    date_reporting = "date_first_pcr_positive_test",
    gender = "sex",
    # age = "age_category", # does not pass validation 
    age = "age_in_years",
    occupation = "age_category" # (downstream implications!)
  ) %>% 
  # validate linelist
  linelist::validate_linelist() %>% 
  # safeguard
  # TRY
  # dplyr::select(date_first_pcr_positive_test,sex, age_category) %>% #class()
  # CONSEQUENCE
  # some tags can lose their variables [notified by Error or Warning]
  # INSTEAD
  linelist::tags_df() %>%  
  # relevant change: the variable names CHANGE to tag names!
  # (downstream implications!)
  
  # 3 aggregate and visualize
  
  # transform from individual-level to time-aggregate
  incidence2::incidence(
    date_index = "date_reporting", #"date_first_pcr_positive_test",
    groups = "occupation", #"age_category", # change to sex, ...
    interval = "month", # change to days, weeks, ...
    # complete_dates = TRUE # relevant to downstream analysis [time-series data]
  ) %>% 
  plot(
    fill = "occupation", # "age_category",
    show_cases = TRUE, angle = 45, n_breaks = 5 # alternative options (vignette)
  )
