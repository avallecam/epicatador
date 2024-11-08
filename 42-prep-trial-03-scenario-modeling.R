
# introduction ------------------------------------------------------------

#' what are compartmental models?
#' 
#' what model parameters we need?
#' - transmission rate
#' - contact matrix with frequency of contact
#' - infectiousness rate (inverse of latent period)
#' - recovery rate (inverse of infectious period)
#' 
#' #' rate is the inverse of the average time until that event
#' 
#' what inputs we need?
#' 
#' - contact matrix
#' - initial conditions
#' - population structure
#' - model parameters 


# load packages -----------------------------------------------------------

library(socialmixr)
library(epidemics)
library(tidyverse)

# (1) contact matrix ----------------------------------------------------------


#' what is polymod study?

polymod <- socialmixr::polymod

# socialmixr::get_survey("https://zenodo.org/records/4086739")

contact_data <- socialmixr::contact_matrix(
  survey = polymod,
  countries = "Germany",
  age.limits = c(0, 20, 40),
  symmetric = TRUE
)

# prepare contact matrix
contact_matrix <- t(contact_data$matrix)
contact_matrix

#' [QUESTION] why do we transpose the matrix?
#' 
#' {socialmixr} collect data in from-to orientation
#' 
#' It contains the mean number of contacts that 
#' each member of an age group (row) has reported with 
#' members of the same or another age group (column).
#' 
#' row: from (participants)
#' column: to (reported contacts)
#' 
#' {epidemics} needs data in to-from
#' 
#' row: to
#' column: from


# (2) initial conditions ------------------------------------------------------

#' key:
#' one set of initial conditions per age-group

## infectious population ---------
initial_i <- 1e-6

initial_conditions_inf <- c(
  S = 1 - initial_i, E = 0, I = initial_i, R = 0, V = 0
)

initial_conditions_inf

## free of infection population ---------

initial_conditions_free <- c(
  S = 1, E = 0, I = 0, R = 0, V = 0
)

initial_conditions_free

## combine initial conditions ------------

# combine the initial conditions
initial_conditions <- rbind(
  initial_conditions_free, # age group 1
  initial_conditions_inf, # age group 2
  initial_conditions_free # age group 3
)

# use contact matrix to assign age group names
rownames(initial_conditions) <- rownames(contact_matrix)

initial_conditions

# (3) population structure ----------------------------------------------------

demography_vector <- contact_data$demography$population

names(demography_vector) <- rownames(contact_matrix)

demography_vector

#' adapt to {epidemics}

uk_population <- population(
  name = "UK",
  contact_matrix = contact_matrix, #' needs transposition (why?)
  demography_vector = demography_vector,
  initial_conditions = initial_conditions
)

uk_population


# (4) model parameters --------------------------------------------------------

#' from literature!

#' [QUESTION] why each follows that definition

# time periods
preinfectious_period <- 7.0 # latent period
infectious_period <- 14.0 # duration of infectiousness
basic_reproduction <- 2

# rates
infectiousness_rate <- 1/preinfectious_period
recovery_rate <- 1/infectious_period
transmission_rate <-  basic_reproduction*recovery_rate

# run the model -----------------------------------------------------------

output <- model_default(
  # population
  population = uk_population,
  # parameters
  transmission_rate = transmission_rate,
  infectiousness_rate = infectiousness_rate,
  recovery_rate = recovery_rate,
  # time setup
  time_end = 600, increment = 1.0
)

head(output)


# [HELPER] new_infection --------------------------------------------------

#' new infections per unit time
#' daily incidence curve

output

# get new infections
output_new_infections <- new_infections(output, by_group = TRUE)

# visualise the spread of the epidemic in terms of new infections
# plot figure of epidemic curve

output_new_infections %>% 
  ggplot() +
  geom_line(
    aes(time, new_infections, colour = demography_group),
    # linetype = "dashed"
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  # scale_y_sqrt(
  #   labels = scales::comma,
  #   breaks = c(10^seq(3, 5), 5e4)
  # ) +
  # scale_colour_brewer(
  #   palette = "Dark2",
  #   name = "Age group"
  # ) +
  coord_cartesian(
    expand = FALSE
  ) +
  # theme_bw() +
  # theme(
  #   legend.position = "top"
  # ) +
  labs(
    x = "Simulation time (days)",
    linetype = "Compartment",
    y = "Individuals"
  )



# plot model --------------------------------------------------------------

library(tidyverse)

output %>% 
  filter(compartment %in% c(#"exposed", 
                            "infectious")) %>%
  ggplot(
    aes(
      x = time,
      y = value,
      col = demography_group,
      linetype = compartment
    )
  ) +
  geom_line(
    # linewidth = 1.2
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  # scale_colour_brewer(
  #   palette = "Dark2",
  #   name = "Age group"
  # ) +
  # expand_limits(
  #   y = c(0, 500e3)
  # ) +
  coord_cartesian(
    expand = FALSE
  ) +
  # theme_bw(
  #   base_size = 15
  # ) +
  # theme(
  #   legend.position = "top"
  # ) +
  labs(
    x = "Simulation time (days)",
    linetype = "Compartment",
    y = "Individuals"
  ) +
  theme_bw()


# interpret plot ----------------------------------------------------------

#' [QUESTION] how to interpret the exposed-infectious compartments in time?
