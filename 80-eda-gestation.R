
library(mosaicData)
Gestation # data to use
?Gestation # ask for more info about a package object
# add to environment
#' [good practice]
# use tab!
data("Gestation")
Gestation
View(Gestation)
library(tidyverse)
# tibbles = an alternative to data frames
as_tibble(Gestation)
count(Gestation) # notice the tibble class output
# to summarize categorical variables
# use count()
count(Gestation, race)
#' [keyboard shortcut]
# the pipe
# Ctrl + Shift + M (or Cmd on Mac)
Gestation %>% 
  as_tibble() %>% 
  count(race)
#' your turn [activity 1] [solve on livecoding]
#' what is the most common 
#' pairing of racial group and mother’s education status 
#' in the data?
Gestation %>% 
  as_tibble() %>% 
  count(race, ed,sort = TRUE)
#' [new] use the argument to sort
# when we collapse data to single values
# we are doing a "summarize"
# for numerical 
Gestation %>% 
  as_tibble() %>% 
  summarise(wt_mean = mean(wt))
#' [good practice]
# place the cursor
#' your turn [activity 2] [solve on livecoding]
#' What is the mean age of the mothers ‘and weight 
#' of the babies in the data?
Gestation %>% 
  as_tibble() %>% 
  summarise(
    wt_mean = mean(wt),
    age_mean = mean(age,na.rm = TRUE)
  )
#' [new] use the na.rm for missing values
# add 
# - standard deviation
# - 95% quantile range # ask how to
Gestation %>% 
  as_tibble() %>% 
  summarise(
    wt_mean = mean(wt),
    # age_mean = mean(age,na.rm = TRUE),
    wt_sd = sd(wt),
    wt_low = quantile(wt, 0.025),
    wt_high = quantile(wt, 0.975)
  )
# but this is global, let's stratify
Gestation %>% 
  as_tibble() %>% 
  group_by(race) %>% 
  summarise(
    wt_mean = mean(wt),
    # age_mean = mean(age,na.rm = TRUE),
    wt_sd = sd(wt),
    wt_low = quantile(wt, 0.025),
    wt_high = quantile(wt, 0.975)
  )
#' your turn [activity 3] [report on mentimeter]
#' what is the mean and standard deviation of mothers’ 
#' age of Mexican race?
Gestation %>% 
  as_tibble() %>% 
  group_by(race) %>% 
  summarise(
    age_mean = mean(age,na.rm = TRUE),
    age_sd = sd(age,na.rm = TRUE)
  )
#' your turn [activity 4b] [report on mentimeter]
# Calculate the sample mean of the ages and weights of 
# the mothers in each race group
Gestation %>% 
  as_tibble() %>% 
  group_by(race) %>% 
  summarise(
    wt_mean = mean(wt.1,na.rm = TRUE),
    age_mean = mean(age,na.rm = TRUE),
  )
#' [extra] [show on livecoding]
#' count is group_by + summarise
Gestation %>% 
  count(race)
# equivalent
Gestation %>% 
  group_by(race) %>% 
  summarise(n = n())

# rows and columns --------------------------------------------------------

# all code before operate on rows (summary functions)

# now, let's operate on cols (vectorized functions)
Gestation %>% 
  select(id, race, wt, number) %>% 
  # select("Birth weight (oz)" = wt) %>% 
  rename("Birth weight (oz)" = wt) %>% 
  mutate(race = str_to_title(race)) %>% 
  count(race, number) %>% 
  filter(number == "never") %>% 
  slice_max(n)
#' your turn [activity 4d] [report on mentimeter]
#' # Calculate the 
#' mean, standard deviation, minimum, maximum and 
#' proportion of values missing for 
#' the mothers' ages for each race group.
Gestation %>%
  group_by(race) %>% 
  summarise(
    mean_age = mean(age,na.rm = TRUE),
    sd_age = sd(age,na.rm = TRUE),
    min_age = min(age,na.rm = TRUE),
    max_age = max(age,na.rm = TRUE),
    missing_age = sum(is.na(age)),
    total = n()
  ) %>%
  mutate(proportion = missing_age/total)


# reshape -----------------------------------------------------------------

# why?
# we can make same summaries
# for multiple variables
Gestation %>% 
  select(id, wt.1, dwt, dage, age) %>% 
  pivot_longer(
    cols = -id,
    names_to = "variable",
    values_to = "values"
  ) %>% 
  group_by(variable) %>% 
  summarise(mean = mean(values,na.rm = TRUE))
# or plot to compare distributions
Gestation %>% 
  select(id, wt.1, dwt, dage, age) %>% 
  pivot_longer(
    cols = -id,
    names_to = "variable",
    values_to = "values"
  ) %>% 
  ggplot(aes(x = values)) +
  geom_histogram() +
  facet_grid(variable~.)

# lets try to long it
Gestation %>% 
  select(id, wt.1, dwt, dage, age) %>% 
  pivot_longer(
    cols = -id,
    names_to = "variable",
    values_to = "values"
  )

# return to wide
Gestation %>% 
  select(id, wt.1, dwt, dage, age) %>% 
  pivot_longer(
    cols = -id,
    names_to = "variable",
    values_to = "values"
  ) %>% 
  pivot_wider(
    id_cols = id,
    names_from = variable,
    values_from = values
  )

#' your turn [activity 4c] [report on mentimeter]

# Make a wide table from the summary data frame calculated in 
# Activity 1 that has the number of observations
# for each combination of mother's education level and race. 
# Make each row is an education level and each column a race group.

# Hint: Look at the help file for `pivot_wider` for
# what to do with missing cells 
# (where there is no combination of these variables)
# and set the argument to be 0.

Gestation %>% 
  count(ed,race) %>% 
  pivot_wider(
    names_from = race,
    values_from = n,
    values_fill = 0
  )

#' [new] use values_fill = 0


# extra -------------------------------------------------------------------

# explore {skimr}

Gestation %>% 
  skimr::skim()







