
# readme ------------------------------------------------------------------

# load finalsize
library(finalsize)

# Load example POLYMOD data included with the package
data(polymod_uk)

# Define contact matrix (entry {ij} is contacts in group i reported by group j)
(contact_matrix <- polymod_uk$contact_matrix)

# Define population in each age group
(demography_vector <- polymod_uk$demography_vector)

# Define susceptibility of each group
susceptibility <- matrix(
  data = c(1.0, 0.5, 0.5),
  nrow = length(demography_vector),
  ncol = 1
)

# Assume uniform susceptibility within age groups
p_susceptibility <- matrix(
  data = 1.0,
  nrow = length(demography_vector),
  ncol = 1
)

# R0 of the disease
r0 <- 1.5 # assumed for pandemic influenza

# Calculate the proportion of individuals infected
final_size(r0 = r0,
           contact_matrix = contact_matrix,
           demography_vector = demography_vector,
           p_susceptibility = p_susceptibility,
           susceptibility = susceptibility)

# vignette 01 -------------------------------------------------------------

# load finalsize
library(finalsize)

# define r0 as 2.0
r0 <- 2.0

# get UK polymod data
polymod <- socialmixr::polymod
contact_data <- socialmixr::contact_matrix(
  survey = polymod,
  countries = "United Kingdom",
  age.limits = c(0, 5, 18, 40, 65),
  symmetric = TRUE
)

contact_data$matrix
contact_data$demography

# get the contact matrix and demography data
contact_data$matrix
contact_matrix <- t(contact_data$matrix)
contact_matrix

demography_data <- contact_data$demography
demography_data

demography_vector <- demography_data$population
demography_vector

# scale the contact matrix so the largest eigenvalue is 1.0
# this is to ensure that the overall epidemic dynamics correctly reflect
# the assumed value of R0
contact_matrix_pre <- contact_matrix
contact_matrix_pre
library(plot.matrix)
plot(contact_matrix_pre)
contact_matrix <- contact_matrix / max(eigen(contact_matrix)$values)
contact_matrix
plot(contact_matrix)
# eigen(contact_matrix)

# divide each row of the contact matrix by the corresponding demography
# this reflects the assumption that each individual in group {j} make contacts
# at random with individuals in group {i}
contact_matrix <- contact_matrix / demography_vector
plot(contact_matrix)

n_demo_groups <- length(demography_vector)

# outputs
contact_matrix
n_demo_groups


## population susceptibility --------------------------------------------

# all individuals are equally and highly susceptible
n_susc_groups <- 1L
susc_guess <- 1.0

susc_uniform <- matrix(
  data = susc_guess,
  nrow = n_demo_groups,
  ncol = n_susc_groups
)
susc_uniform

p_susc_uniform <- matrix(
  data = 1.0,
  nrow = n_demo_groups,
  ncol = n_susc_groups
)
p_susc_uniform


## running final size ------------------------------------------------------

# calculate final size
final_size_data <- final_size(
  r0 = r0,
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  susceptibility = susc_uniform,
  p_susceptibility = p_susc_uniform
)

final_size_data

### visualize final size ----------------------------------------------------


dplyr::as_tibble(final_size_data)

# order demographic groups
final_size_data$demo_grp <- factor(
  final_size_data$demo_grp,
  levels = demography_data$age.group
)

dplyr::as_tibble(final_size_data)

# plot data
library(ggplot2)

ggplot(final_size_data) +
  geom_col(
    aes(
      demo_grp, p_infected
    ),
    # fill = "lightgrey",
    # col = "black"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme_classic() +
  # coord_cartesian(
  #   expand = FALSE
  # ) +
  labs(
    x = "Age group",
    y = "% Infected"
  )

### real step ---------------------------------------------------------------

# prepare demography data
demography_data <- contact_data$demography
# merge final size counts with demography vector
final_size_data <- merge(
  final_size_data,
  demography_data,
  by.x = "demo_grp",
  by.y = "age.group"
)
# reset age group order
final_size_data$demo_grp <- factor(
  final_size_data$demo_grp,
  levels = contact_data$demography$age.group
)
# multiply counts with proportion infected
final_size_data$n_infected <- final_size_data$p_infected *
  final_size_data$population

ggplot(final_size_data) +
  geom_col(
    aes(
      x = demo_grp, y = n_infected
    ),
    # fill = "lightgrey",
    # col = "black"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(
      scale = 1e-6, suffix = "M"
    ),
    limits = c(0, 15e6)
  ) +
  theme_classic() +
  # coord_cartesian(
  #   expand = FALSE
  # ) +
  labs(
    x = "Age group",
    y = "Number infected (millions)"
  )

### proportion to counts ----------------------------------------------------

library(dplyr)

final_size_data2 <- 
  final_size_data %>% 
  # as_tibble() %>% 
  left_join(
    demography_data %>% 
      # as_tibble() %>% 
      mutate(age.group = as.factor(age.group)) %>% 
      select(demo_grp = age.group, population)
    ) %>% 
  mutate(n_infected = p_infected * population)

final_size_data2

ggplot(final_size_data2) +
  geom_col(
    aes(
      x = demo_grp, y = n_infected
    ),
    # fill = "lightgrey",
    # col = "black"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(
      scale = 1e-6, suffix = "M"
    ),
    limits = c(0, 15e6)
  ) +
  theme_classic() +
  # coord_cartesian(
  #   expand = FALSE
  # ) +
  labs(
    x = "Age group",
    y = "Number infected (millions)"
  )


### extra -------------------------------------------------------------------

ggplot(final_size_data2) +
  geom_col(
    aes(
      x = demo_grp, y = population
    ),
    # fill = "lightgrey",
    # col = "black"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(
      scale = 1e-6, suffix = "M"
    ),
    # limits = c(0, 15e6)
  )

library(plot.matrix)
plot(contact_matrix)


# vignette 02 --------------------------------------------------------------

# load finalsize
if (!require("pacman")) install.packages("pacman")
pacman::p_load(finalsize)
pacman::p_load(ggplot2)
pacman::p_load(colorspace)

# define r0 as 2.0
r0 <- 2.0

# get UK polymod data
polymod <- socialmixr::polymod
contact_data <- socialmixr::contact_matrix(
  survey = polymod,
  countries = "United Kingdom",
  age.limits = c(0, 5, 18, 40, 65),
  symmetric = TRUE
)

contact_data

# get the contact matrix and demography data
contact_matrix <- t(contact_data$matrix)
demography_vector <- contact_data$demography$population
demography_data <- contact_data$demography

# scale the contact matrix so the largest eigenvalue is 1.0
# this is to ensure that the overall epidemic dynamics correctly reflect
# the assumed value of R0
contact_matrix <- contact_matrix / max(eigen(contact_matrix)$values)
# eigen(contact_matrix)

# divide each row of the contact matrix by the corresponding demography
# this reflects the assumption that each individual in group {j} make contacts
# at random with individuals in group {i}
contact_matrix <- contact_matrix / demography_vector

n_demo_groups <- length(demography_vector)

# outputs
contact_matrix
n_demo_groups


## 2.1 susceptibility between groups -------------------------------------------

# susceptibility is higher for the old
susc_variable <- matrix(
  data = c(0.2, 0.5, 0.6, 0.9, 1.0)
)
n_susc_groups <- 1L

susc_variable
n_susc_groups

### susceptibility within groups  -------------------------------------------

#' one susceptible group
#' per demography group

p_susc_uniform <- matrix(
  data = 1.0,
  nrow = n_demo_groups,
  ncol = n_susc_groups
)

p_susc_uniform


## run finalsize -----------------------------------------------------------

### heterogeneous susceptibility between groups -----------------------------

# run final_size with default solvers and control options
# final size with heterogeneous susceptibility
final_size_heterog <- final_size(
  r0 = r0,
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  susceptibility = susc_variable,
  p_susceptibility = p_susc_uniform
)

final_size_heterog

### homogeneous susceptibility between groups --------------------------------

# prepare uniform susceptibility matrix
susc_uniform <- matrix(1.0, 
                       nrow = n_demo_groups, 
                       ncol = n_susc_groups)
susc_uniform

# run final size with uniform susceptibility
final_size_uniform <- final_size(
  r0 = r0,
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  susceptibility = susc_uniform,
  p_susceptibility = p_susc_uniform
)

final_size_uniform

## visualization -----------------------------------------------------------

### join data ouputs --------------------------------------------------------

# assign scenario name and join data
final_size_heterog$scenario <- "Heterogeneous"
final_size_uniform$scenario <- "Uniform"

# join dataframes
final_size_data <- rbind(
  final_size_heterog,
  final_size_uniform
)

final_size_data

# prepare age group order
final_size_data$demo_grp <- factor(
  final_size_data$demo_grp,
  levels = contact_data$demography$age.group
)


### plot 1 ------------------------------------------------------------------

ggplot(final_size_data) +
  geom_col(
    aes(
      x = demo_grp, y = p_infected,
      fill = scenario
    ),
    # col = "black",
    position = position_dodge(
      width = 0.75
    )
  ) +
  scale_fill_discrete_qualitative(
    # palette = "Cold",
    name = "Population\nsusceptibility"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme_classic() +
  # theme(
  #   # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #   # legend.position = "top",
  #   # legend.key.height = unit(2, "mm"),
  #   # legend.title = ggtext::element_markdown(
  #   #   vjust = 1
  #   # )
  # ) +
  # coord_cartesian(
  #   expand = FALSE
  # ) +
  labs(
    x = "Age group",
    y = "% Infected"
  )



### plot 1 alt --------------------------------------------------------------

ggplot(final_size_data) +
  geom_col(
    aes(
      x = demo_grp, y = p_infected,
      fill = scenario
    ),
    position = position_dodge(
      width = 0.75
    )
  ) +
  scale_fill_discrete_qualitative(
    # palette = "Cold",
    name = "Population\nsusceptibility"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme_classic() +
  labs(
    x = "Age group",
    y = "% Infected"
  )


## 2.2 susceptibility within and between groups  -------------------------------------------

# immunisation effect
immunisation_effect <- 0.25

# model an immunised group with a 25% lower susceptibility
susc_immunised <- cbind(
  susc_variable,
  susc_variable * (1 - immunisation_effect)
)

susc_immunised

# assign names to groups
colnames(susc_immunised) <- c("Un-immunised", "Immunised")
susc_immunised

n_risk_groups <- ncol(susc_immunised)
n_risk_groups

# immunisation increases with age between 20% (infants) and 90% (65+)
immunisation_rate <- rep(0.4, n_demo_groups)
immunisation_rate

# add a second column to p_susceptibility
p_susc_immunised <- cbind(
  susceptible = p_susc_uniform - immunisation_rate,
  immunised = immunisation_rate
)

p_susc_immunised


### understanding this ------------------------------------------------------

# model an immunised group with a 25% lower susceptibility
example01 <- cbind(
  susc_uniform,
  susc_variable,
  susc_variable * (1 - immunisation_effect)
)
colnames(example01) <- c("Immune-suppressed","Un-immunised", "Immunised")
example01


immune_supressed <- rep(0.1, n_demo_groups)

cbind(
  immunesupressed = immune_supressed,
  unimmunised = as.vector(p_susc_uniform - immune_supressed - immunisation_rate),
  immunised = immunisation_rate
)

### run finalsize -----------------------------------------------------------

# we run final size over all r0 values
final_size_immunised <- final_size(
  r0 = r0,
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  susceptibility = susc_immunised,
  p_susceptibility = p_susc_immunised
)

final_size_immunised


### visualization -----------------------------------------------------------

# add scenario identifier
final_size_immunised$scenario <- "Immunisation"

# prepare age group order
final_size_heterog$demo_grp <- factor(
  final_size_heterog$demo_grp,
  levels = contact_data$demography$age.group
)

final_size_immunised$demo_grp <- factor(
  final_size_immunised$demo_grp,
  levels = contact_data$demography$age.group
)


#### alternative -------------------------------------------------------------

pacman::p_load(dplyr)
pacman::p_load(forcats)

final_size_data3 <- 
  bind_rows(final_size_heterog,
            final_size_immunised)

# heterogeneous susceptibility between groups only, no immunization

final_size_data4 <- final_size_data3 %>%
  as_tibble() %>%
  # count(susc_grp)
  mutate(
    scenario = case_when(scenario == "Heterogeneous" ~ "Between groups only",
                         scenario == "Immunisation" ~ "Between and within groups",
                         TRUE ~ scenario),
    susc_grp = fct_recode(susc_grp,
                          "No immunization" = "susc_grp_1"),
    susc_grp = fct_relevel(susc_grp,
                           "Immunised",
                           "Un-immunised"))

final_size_data4 %>% 
  # count(susc_grp)
  ggplot() +
  geom_col(aes(x = demo_grp, 
               y = p_infected,
               fill = susc_grp),
           position = position_dodge()) +
  facet_grid(~scenario) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.5)
  ) +
  theme_bw() +
  labs(
    title = "Susceptibility scenarios",
    x = "Age group",
    y = "% Infected",
    fill = "Immunisation\nscenario"
  )

#### official plot -----------------------------------------------------------

ggplot(final_size_immunised) +
  geom_col(
    data = final_size_heterog,
    aes(
      x = demo_grp, y = p_infected,
      fill = "baseline",
      colour = "baseline"
    ),
    # width = 0.75,
    # show.legend = TRUE
  ) +
  geom_col(
    aes(
      x = demo_grp, y = p_infected,
      fill = susc_grp
    ),
    # col = "black",
    position = position_dodge()
  ) +
  scale_fill_discrete_qualitative(
    palette = "Dynamic",
    rev = TRUE,
    limits = c("Immunised", "Un-immunised"),
    name = "Immunisation scenario",
    na.value = "lightgrey"
  ) +
  scale_colour_manual(
    values = "black",
    name = NULL,
    labels = "Susceptibility:\nheterogeneous\nbetween groups,\nwithout immunisation"
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.5)
  ) +
  theme_classic() +
  # theme(
  #   # legend.position = "top",
  #   # legend.key.height = unit(2, "mm"),
  #   # legend.title = ggtext::element_markdown(
  #   #   vjust = 1
  #   )
  # ) +
  guides(
    colour = guide_legend(
      override.aes = list(fill = "lightgrey")
    ),
    # fill = guide_legend(
    #   nrow = 2,
    #   title.position = "top"
    # )
  ) +
  # coord_cartesian(
  #   expand = FALSE
  # ) +
  labs(
    x = "Age group",
    y = "% Infected",
    fill = "Immunisation\nscenario"
  )


# vignette 03 -------------------------------------------------------------

# load finalsize
if (!require("pacman")) install.packages("pacman")
pacman::p_load(finalsize)
pacman::p_load(socialmixr)
pacman::p_load(ggplot2)

# get UK polymod data
polymod <- socialmixr::polymod
contact_data <- socialmixr::contact_matrix(
  polymod,
  countries = "United Kingdom",
  age.limits = c(0, 5, 18, 40, 65),
  symmetric = TRUE
)

# get the contact matrix and demography data
contact_matrix <- t(contact_data$matrix)
demography_vector <- contact_data$demography$population

# scale the contact matrix so the largest eigenvalue is 1.0
contact_matrix <- contact_matrix / max(eigen(contact_matrix)$values)

# divide each row of the contact matrix by the corresponding demography
contact_matrix <- contact_matrix / demography_vector

n_demo_groups <- length(demography_vector)

# mean R0 is 2.0
r0_mean <- 2.0

# susceptibility is uniform
susc_uniform <- matrix(
  data = 1,
  nrow = n_demo_groups,
  ncol = 1L
)

# p_susceptibility is uniform
p_susc_uniform <- susc_uniform

# create an R0 samples vector
r0_samples <- rnorm(n = 1000, mean = r0_mean, sd = 0.1)

hist(r0_samples)

## alternative -------------------------------------------------------------

pacman::p_load(tibble)
pacman::p_load(dplyr)
pacman::p_load(tidyr)
pacman::p_load(purrr)
pacman::p_load(forcats)

# create a list of arguments
list_arguments <- list(
  contact_matrix = contact_matrix,
  demography_vector = demography_vector,
  susceptibility = susc_uniform,
  p_susceptibility = p_susc_uniform
)

final_size_output <- 
  
  # create a dataframe with values from a vector 
  tibble(r0 = r0_samples) %>% 
  rownames_to_column() %>% 
  
  # map the function to all the r0 values 
  # with the same set of arguments
  mutate(temp = map(
    .x = r0,
    .f = final_size,
    contact_matrix = list_arguments$contact_matrix,
    demography_vector = list_arguments$demography_vector,
    p_susceptibility = list_arguments$p_susceptibility,
    susceptibility = list_arguments$susceptibility)) %>% 
  
  # unnest all the dataframe outputs in temp
  unnest(temp) %>% 
  
  # relevel the factor variable
  mutate(demo_grp = fct_relevel(demo_grp,
                                contact_data %>% 
                                  pluck("demography") %>% 
                                  pluck("age.group")))

final_size_output

ggplot(final_size_output) +
  stat_summary(
    aes(
      demo_grp, p_infected
    ),
    fun = mean,
    fun.min = function(x) {
      quantile(x, 0.05)
    },
    fun.max = function(x) {
      quantile(x, 0.95)
    }
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0.25, 1)
  ) +
  theme_classic() +
  # theme(
  #   legend.position = "top",
  #   legend.key.height = unit(2, "mm"),
  #   legend.title = ggtext::element_markdown(
  #     vjust = 1
  #   )
  # ) +
  # coord_cartesian(
  #   expand = TRUE
  # ) +
  labs(
    x = "Age group",
    y = "% Infected"
  )


## original ----------------------------------------------------------------


# run final size on each sample with the same data
final_size_data <- Map(
  r0_samples, seq_along(r0_samples),
  f = function(r0, i) {
    # the i-th final size estimate
    fs <- final_size(
      r0 = r0,
      contact_matrix = contact_matrix,
      demography_vector = demography_vector,
      susceptibility = susc_uniform,
      p_susceptibility = p_susc_uniform
    )
    
    fs$replicate <- i
    fs$r0_estimate <- r0
    fs
  }
)

# combine data
final_size_data <- data.table::rbindlist(final_size_data)

final_size_data

# order age groups
final_size_data$demo_grp <- factor(
  final_size_data$demo_grp,
  levels = contact_data$demography$age.group
)

ggplot(final_size_data) +
  stat_summary(
    aes(
      demo_grp, p_infected
    ),
    fun = mean,
    fun.min = function(x) {
      quantile(x, 0.05)
    },
    fun.max = function(x) {
      quantile(x, 0.95)
    }
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0.25, 1)
  ) +
  theme_classic() +
  # theme(
  #   legend.position = "top",
  #   legend.key.height = unit(2, "mm"),
  #   legend.title = ggtext::element_markdown(
  #     vjust = 1
  #   )
  # ) +
  # coord_cartesian(
  #   expand = TRUE
  # ) +
  labs(
    x = "Age group",
    y = "% Infected"
  )




## extra -------------------------------------------------------------------

# final_size(r0 = r0_mean,
#            contact_matrix = list_arguments$contact_matrix,
#            demography_vector = list_arguments$demography_vector,
#            p_susceptibility = list_arguments$p_susceptibility,
#            susceptibility = list_arguments$susceptibility)


# vignette draft ------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr)
pacman::p_load(tibble)

# case 1: uniform susceptibility --------------------------------------------------

#' in the population,
#' all individuals from all age groups
#' have 80% susceptibility to the infection

# susceptibility matrix
susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         susceptible = c(0.8, 0.8, 0.8, 0.8, 0.8)) %>% 
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

susceptibility

# demography-susceptibility distribution matrix
p_susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         susceptible = c(1.0, 1.0, 1.0, 1.0, 1.0)) %>% 
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

p_susceptibility

# heterogeneous susceptibility --------------------------------------------

## case 2: only between groups -----------------------------------------------------

#' in the population,
#' there is different
#' susceptibility to the infection
#' between individuals of different age groups
#' from 20% (infants) to 100% (65+)

# susceptibility matrix
susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         susceptible = c(0.2, 0.5, 0.6, 0.9, 1.0)) %>% 
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

susceptibility

# demography-susceptibility distribution matrix
p_susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         susceptible = c(1.0, 1.0, 1.0, 1.0, 1.0)) %>% 
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

p_susceptibility

## case 3: within and between groups --------------------------------------------

#' in the population,
#' there is different
#' susceptibility to the infection
#' between individuals of different age groups
#' from 20% (infants) to 100% (65+)
#' and
#' within individuals of the same age group
#' due the immunization effect of 25%
#' to the 40% of each of the age groups

immunization_effect <- 0.25

# susceptibility matrix
susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         unimmunised = c(0.2, 0.5, 0.6, 0.9, 1.0)) %>% 
  mutate(immunised = unimmunised * (1 - immunization_effect)) %>%
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

susceptibility

# demography-susceptibility distribution matrix
p_susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         unimmunised = c(0.6, 0.6, 0.6, 0.6, 0.6)) %>% 
  mutate(immunised = 1 - unimmunised) %>% 
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

p_susceptibility


## case 4: within in different proportion ------------------------------------------

#' in the population,
#' there is different
#' susceptibility to the infection
#' between individuals of different age groups
#' from 20% (infants) to 100% (65+)
#' and
#' within individuals of the same age group
#' due the immunization effect of 25%
#' to different proportions for each of the age groups
#' immunisation increases with age 
#' from 20% (infants) to 90% (65+)

immunization_effect <- 0.25

# susceptibility matrix
susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         unimmunised = c(0.2, 0.5, 0.6, 0.9, 1.0)) %>% 
  mutate(immunised = unimmunised * (1 - immunization_effect)) %>%
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

susceptibility

# demography-susceptibility distribution matrix
p_susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         unimmunised = c(0.8, 0.6, 0.4, 0.3, 0.1)) %>% 
  mutate(immunised = 1 - unimmunised) %>% 
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

p_susceptibility

## case 5: within three groups -----------------------------------------------------

#' in the population,
#' there is different
#' susceptibility to the infection
#' between individuals of different age groups
#' from 20% (infants) to 100% (65+)
#' and
#' within individuals of the same age group
#' due the immunization effect of 25%
#' to the 40% of each of the age groups
#' and
#' due to 10% of individuals 
#' in each of the age groups
#' non immunised and non exposed
#' to similar infections
#' with 100% susceptibility

immunization_effect <- 0.25

# susceptibility matrix
susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         susceptible = c(1.0, 1.0, 1.0, 1.0, 1.0),
         unimmunised = c(0.2, 0.5, 0.6, 0.9, 1.0)) %>% 
  mutate(immunised = unimmunised * (1 - immunization_effect)) %>%
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

susceptibility

# demography-susceptibility distribution matrix
p_susceptibility <- 
  tibble(age_group = c("[0,5)","[5,18)","[18,40)","[40,65)","65+"),
         susceptible = c(0.1, 0.1, 0.1, 0.1, 0.1),
         unimmunised = c(0.6, 0.6, 0.6, 0.6, 0.6)) %>% 
  mutate(immunised = 1 - unimmunised - susceptible) %>% 
  column_to_rownames(var = "age_group") %>% 
  as.matrix()

p_susceptibility

