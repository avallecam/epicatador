# nolint start

# Practical 4
# Activity 1

# step: fill in your room number
room_number <- 3 #valid for all

# Load packages ----------------------------------------------------------
library(epidemics)
library(socialmixr)
library(tidyverse)

# Group parameters -------------------------------------------------------

# activity 1
# socialsurvey_link <- "https://doi.org/10.5281/zenodo.3874557" # polymod
# socialsurvey_link <- "https://doi.org/10.5281/zenodo.3874802" # vietnam
socialsurvey_link <- "https://doi.org/10.5281/zenodo.3886638" # zimbabwe
# socialsurvey_country <- "Italy"
# socialsurvey_country <- "Vietnam"
socialsurvey_country <- "Zimbabwe"
age_limits <- c(0, 20, 40)
infectious_population <- 1 / 1e6 # 1 infectious out of 1,000,000

basic_reproduction_number <- 1.46
pre_infectious_period <- 3 # days
infectious_period <- 7 # days


# (1) Contact matrix ------------------------------------------------------

# step: paste the survey link for your room
socialsurvey <- socialmixr::get_survey(
  survey = socialsurvey_link
)

# step: generate contact matrix by defining
# survey class object, country name, 
# age limits, and whether to make a symmetric matrix
contact_data <- socialmixr::contact_matrix(
  survey = socialsurvey,
  countries = socialsurvey_country,
  age.limits = age_limits,
  symmetric = TRUE
)

contact_data

# Matrix are symmetric for the total number of contacts
# of one group with another is the same as the reverse
contact_data$matrix * contact_data$demography$proportion

matrix_to_ggplot <- function(matrix, digits) {
  matrix %>%
    as.data.frame.table(responseName = "value") %>%
    dplyr::mutate(text = round(value, digits = digits)) %>%
    ggplot(aes(x = contact.age.group, y = age.group)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = text)) +
    colorspace::scale_fill_continuous_sequential(
      palette = "OrYel",
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    coord_transform(reverse = "y") +
    scale_x_discrete(position = "top") +
    theme(aspect.ratio = 1) +
    labs(y = "Participant age group (years)", x = "Contacts age group (years)")
}

contact_data$matrix %>% 
  matrix_to_ggplot(digits = 2)

contact_data$matrix

(contact_data$matrix * contact_data$demography$population) %>% 
  matrix_to_ggplot(digits = 0)

contact_data$matrix * contact_data$demography$population

