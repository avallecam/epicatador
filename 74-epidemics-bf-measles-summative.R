
# summative steps for epidemics measles -----------------------------------

# prereq ------------------------------------------------------------------

#' [R]
#' math operations: object with vector 
#' operator: ! OR AND (we can omit)
#' 
#' [IDM]
#' SEIR model compartments
#' proportion per compartment
#' rate parameters
#' values per parameter
#' contact matrix


# will learn --------------------------------------------------------------

#' [R]
#' matrix

# episode 1 -------------------------------------------------------------------

# part 1 (livecoding) ------------------------------------------------------------------

#' simulate SEIR trayectories

# Loading libraries
library(dplyr)
library(ggplot2)
library(epidemics)

bf_pop <- 22.67e6

bfaso_population <- epidemics::population(
  name = "Burkina Faso",
  contact_matrix = matrix(1),
  demography_vector = bf_pop,
  # SEIR-V
  initial_conditions = matrix(
    c(
      1 - 1/bf_pop, # S
      1/bf_pop, # E
      0, # I
      0, # R
      0 # V
    ), nrow = 1, ncol = 5
  )
)

output <- epidemics::model_default(
  population = bfaso_population,
  transmission_rate = 15/5,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  time_end = 120,
  increment = 1
)

output %>% 
  # filter(
  #   compartment == "infectious"
  #   # compartment == "infectious" | compartment == "exposed"
  #   # magrittr::is_in(compartment, c("infectious", "exposed"))
  #   # compartment %in% c("exposed", "infectious")
  #   ) %>%
  ggplot(aes(x = time, y = value, color = compartment)) +
  geom_line()

# # extra
# finalsize::final_size(r0 = 10)
# 1-(1/10)

# part 2 ------------------------------------------------------------------

## age-structured (copy/paste) -------------

contact_matrix <- matrix(
  c(
    20, 10, 5, 2, 1, # Contacts from 0-14 years
    10, 15, 10, 4, 2, # Contacts from 15-24 years
    5, 10, 15, 6, 3, # Contacts from 25-54 years
    2, 4, 6, 10, 5, # Contacts from 55-64 years
    1, 2, 3, 5, 10
  ), # Contacts from 65+ years
  nrow = 5, # Number of rows
  byrow = TRUE, # Fill the matrix by rows
  dimnames = list(
    c("[0-15)", "[15-25)", "[25-55)", "[55-65)", "65+"),
    c("[0-15)", "[15-25)", "[25-55)", "[55-65)", "65+")
  )
)

contact_matrix

## scale contact matrix (copy/paste) ----------------------------------------------------

# scale the contact matrix so the largest eigenvalue is 1.0
# this is to ensure that the overall epidemic dynamics correctly reflect
# the assumed value of R0
contact_matrix <- contact_matrix / max(Re(eigen(contact_matrix)$values))

# divide each row of the contact matrix by the corresponding demography
# this reflects the assumption that each individual in group {j} make contacts
# at random with individuals in group {i}
contact_matrix <- contact_matrix / demography_vector

contact_matrix

## demography per strata (livecoding) ---------------------------------------------------


# Define the demography vector for Burkina Faso
demography_vector <- c(0.44, 0.195, 0.29, 0.05, 0.025) * bf_pop
names(demography_vector) <- rownames(contact_matrix)

demography_vector

## initial conditions per age-strata (copy/paste/complete) -----------

# Define the population structure for Burkina Faso
initial_i <- 1 / bf_pop

initial_conditions <- c(
  S = 1 - initial_i,
  E = 0,
  I = initial_i,
  R = 0,
  V = 0
)

# replicate initial conditions for all age groups
initial_conditions_matrix <- rbind(
  initial_conditions,
  initial_conditions,
  initial_conditions,
  initial_conditions,
  initial_conditions
)

rownames(initial_conditions_matrix) <- rownames(contact_matrix)

initial_conditions_matrix

## population with age-stratification (complete) ---------

bfaso_population <- epidemics::population(
  name = "Burkina Faso",
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  # SEIR-V
  initial_conditions = initial_conditions_matrix
)

bfaso_population

# part 3 (challenge ggplot2) ------------------------------------------------------------------

output1 <- epidemics::model_default(
  population = bfaso_population,
  transmission_rate = 15/5,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  time_end = 120,
  increment = 1
)

output1 %>% 
  # filter() %>% 
  ggplot(aes(x = time, y = value, color = compartment,linetype = demography_group)) +
  geom_line()


# episode 2 ---------------------------------------------------------------

# part 4 ------------------------------------------------------------------

#' non-pharma

reduced_contacts <- epidemics::intervention(
  name = "school closure",
  type = "contacts",
  time_begin = 30,
  time_end = 30 + 60,
  reduction = matrix(c(0.5,0.01,0.01,0.01,0.01))
)

reduced_contacts

output2 <- epidemics::model_default(
  population = bfaso_population,
  transmission_rate = 15/5,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  time_end = 120,
  increment = 1,
  intervention = list(contacts = reduced_contacts)
)

output2 %>% 
  # filter() %>% 
  ggplot(aes(x = time, y = value, color = compartment,linetype = demography_group)) +
  geom_line()

## baseline vs intervention (livecoding) ----------------------------------------------------------------

data_baseline <- output1 %>% 
  epidemics::new_infections(
    by_group = FALSE
  )

data_reduced <- output2 %>% 
  epidemics::new_infections(
    by_group = FALSE
  )

## assign names and bind (copy/paste/ask) ----------

# Assign scenario names
data_baseline$scenario <- "Baseline"
data_reduced$scenario <- "Reduced Contact"

# Combine the data from both scenarios
data_combined <- bind_rows(data_baseline, data_reduced)

## plot combined (save plot/ask/change TRUE) ------------

data_combined %>%
  ggplot(aes(x = time, y = new_infections,
             # color = demography_group,
             linetype = scenario)) +
  geom_line()

# part 5 (livecoding) ------------------------------------------------------------------

#' pharma

vaccinate_population <- epidemics::vaccination(
  name = "vaccination campaign",
  time_begin = matrix(30,nrow(contact_matrix)),
  time_end = matrix(30 + 60,nrow(contact_matrix)), 
  nu = matrix(c(0.01,0.02,0.03,0.04,0.05))
)

vaccinate_population

output3 <- epidemics::model_default(
  population = bfaso_population,
  transmission_rate = 15/5,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  time_end = 120,
  increment = 1,
  vaccination = vaccinate_population 
)

output3 %>% 
  # filter() %>% 
  ggplot(aes(x = time, y = value, color = compartment,linetype = demography_group)) +
  geom_line()

## baseline vs pharma intervention (livecoding) -----------

data_vaccine <- output3 %>% 
  epidemics::new_infections(
    compartments_from_susceptible = "vaccinated",
    by_group = FALSE
  )

## assign names and bind (copy/paste) ----------

# Assign scenario names
data_vaccine$scenario <- "Vaccinate"

# Combine the data from both scenarios
data_combined <- bind_rows(data_baseline, data_vaccine)

data_combined

## plot combined (compare/ask/change TRUE) ------------

data_combined %>%
  ggplot(aes(x = time, y = new_infections,
             # color = demography_group,
             linetype = scenario)) +
  geom_line()



# part 6 (livecoding) ------------------------------------------------------------------

#' combine
#' + contacts reduction
#' + vaccination

output4 <- epidemics::model_default(
  population = bfaso_population,
  transmission_rate = 15/5,
  infectiousness_rate = 1/8,
  recovery_rate = 1/5,
  time_end = 120,
  increment = 1,
  intervention = list(contacts = reduced_contacts),
  vaccination = vaccinate_population 
)

output4 %>% 
  # filter() %>% 
  ggplot(aes(x = time, y = value, color = compartment,linetype = demography_group)) +
  geom_line()

## plot (copy/paste) -----------

data_multiple <- output4 %>% 
  epidemics::new_infections(
    compartments_from_susceptible = "vaccinated",
    by_group = FALSE
  )

## assign names and bind (copy/paste) ----------

# Assign scenario names
data_multiple$scenario <- "Contact reduction + Vaccinate"

# Combine the data from both scenarios
data_combined <- bind_rows(data_baseline, data_multiple)

data_combined

data_combined %>%
  ggplot(aes(x = time, y = new_infections,
             # color = demography_group,
             linetype = scenario)) +
  geom_line()

# (compare/ask/change TRUE) -----------------------------------------------



