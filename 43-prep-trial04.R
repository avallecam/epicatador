
# select between 1 to 6 ---------------------------------------------------

#'     R     k
#' 1: 0.8 | 0.01
#' 2: 0.8 | 0.1
#' 3: 0.8 | 0.5
#' 4: 1.5 | 0.01
#' 5: 1.5 | 0.1
#' 6: 1.5 | 0.5

set_select <- 1

# load packages -----------------------------------------------------------

library(epiparameter)
library(epichains)
library(epicontacts)
library(tidyverse)

# variability -------------------------------------------------------------

# epidist_db(
#   disease = "mpox",
#   epi_dist = "offspring",
#   single_epidist = TRUE
# ) %>% 
#   get_parameters()
# 
# epidist_db(
#   disease = "ebola",
#   epi_dist = "offspring",
#   single_epidist = TRUE
# ) %>% 
#   get_parameters()

set_grid <- expand_grid(
  mean = c(0.8, 1.5),
  dispersion = c(0.01,0.1,0.5)
) %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_c("set_",rowname))

set_grid_select <- set_grid %>% 
  slice(set_select)

# offspring parameters ----------------------------------------------------

offspring_parameters <- c(
  mean = set_grid_select$mean,
  dispersion = set_grid_select$dispersion
)

offspring_parameters

# generation time ---------------------------------------------------------

generation_time <- epidist(
  disease = "disease x",
  epi_dist = "generation time",
  prob_distribution = "gamma",
  summary_stats = list(mean = 3, sd = 1)
  )

# plot(generation_time)

# iteration parameters ----------------------------------------------------

# Set seed for random number generator
set.seed(33)
# Number of simulation runs
number_chains <- 1000
# Number of initial cases
initial_cases <- 1

# simulate multiple chains ------------------------------------------------

simulated_chains_map <-
  # iterate one function across multiple numbers (chain IDs)
  map(
    # vector of numbers (chain IDs)
    .x = seq_len(number_chains),
    # function to iterate to each chain ID number
    .f = function(sim) {
      simulate_chains(
        # simulation controls
        index_cases = initial_cases,
        statistic = "size",
        stat_max = 500,
        # offspring
        offspring_dist = rnbinom,
        mu = offspring_parameters["mean"],
        size = offspring_parameters["dispersion"],
        # generation
        generation_time = function(x) generate(x = generation_time, times = x)
      ) %>%
        # creates a column with the chain ID number
        mutate(chain_id = sim)
    }
  ) %>%
  # combine list outputs (for each chain ID) into a single data frame
  list_rbind()


# visualize ---------------------------------------------------------------

# daily aggregate of cases
simulated_chains_day <- simulated_chains_map %>%
  # use data.frame output from <epichains> object
  as_tibble() %>%
  # transform chain ID column to factor (categorical variable)
  mutate(chain_id = as_factor(chain_id)) %>%
  # get the round number (day) of infection times
  mutate(day = ceiling(time)) %>%
  # count the daily number of cases in each simulation (chain ID)
  count(chain_id, day, name = "cases") %>%
  # calculate the cumulative number of cases for each simulation (chain ID)
  group_by(chain_id) %>%
  mutate(cases_cumsum = cumsum(cases)) %>%
  ungroup()

# Visualize transmission chains by cumulative cases
ggplot() +
  # create grouped chain trajectories
  geom_line(
    data = simulated_chains_day,
    mapping = aes(
      x = day,
      y = cases_cumsum,
      group = chain_id
    ),
    color = "black",
    alpha = 0.25,
    show.legend = FALSE
  ) +
  # define a 100-case threshold
  geom_hline(aes(yintercept = 100), lty = 2) +
  labs(
    x = "Day",
    y = "Cumulative cases"
  )


# to epicontacts ----------------------------------------------------------

# Summarise the chain duration and size
sim_chains_max <-
  simulated_chains_day %>%
  group_by(chain_id) %>%
  summarise(
    # duration
    day_max = max(day),
    # size
    cases_total = max(cases_cumsum)
  ) %>%
  ungroup()

sim_chains_max %>% 
  arrange(desc(cases_total)) %>% 
  filter(cases_total>100) %>% 
  count()

chain_to_observe <- sim_chains_max %>% 
  arrange(desc(cases_total)) %>% 
  filter(cases_total<200) %>%
  slice_max(cases_total) %>% 
  pull(chain_id)

selected_chain <- simulated_chains_map %>%
  # use data.frame output from <epichains> object
  as_tibble() %>% 
  filter(chain_id == chain_to_observe)

# selected_chain %>%
#   print(n=Inf)



# create linelist contacts ------------------------------------------------


chain_linelist <- selected_chain %>% 
  dplyr::select(id = sim_id,
         # generation,time
         )

chain_contacts <- selected_chain %>% 
  filter(!is.na(infector_id)) %>% 
  dplyr::select(from = infector_id, to = sim_id,
         # generation,
         time)

epi_contacts <- epicontacts::make_epicontacts(linelist = chain_linelist,contacts = chain_contacts)

vis_epicontacts(epi_contacts)


# save --------------------------------------------------------------------

write_rds(chain_linelist,str_c("data-out/set-0",set_select,"-linelist.rds"))
write_rds(chain_contacts,str_c("data-out/set-0",set_select,"-contacts.rds"))



# individual reproduction number ------------------------------------------

# count secondary cases per infector
infector_secondary <- epi_contacts %>%
  pluck("contacts") %>%
  count(from, name = "secondary_cases")

all_secondary <- epi_contacts %>%
  # extract ids in contact *and* linelist using "which" argument
  epicontacts::get_id(which = "all") %>%
  # transform vector to dataframe to use left_join()
  enframe(name = NULL, value = "from") %>%
  # join count secondary cases per infectee
  left_join(infector_secondary) %>%
  # infectee with missing secondary cases are replaced with zero
  replace_na(
    replace = list(secondary_cases = 0)
  )

## plot the distribution
all_secondary %>%
  ggplot(aes(secondary_cases)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Number of secondary cases",
    y = "Frequency"
  )


# fit distribution --------------------------------------------------------


library(fitdistrplus)

## fit distribution
offspring_fit <- all_secondary %>%
  pull(secondary_cases) %>%
  fitdist(distr = "nbinom")

offspring_fit


# redo chains -------------------------------------------------------------

simulated_chains_map <-
  # iterate one function across multiple numbers (chain IDs)
  map(
    # vector of numbers (chain IDs)
    .x = seq_len(number_chains),
    # function to iterate to each chain ID number
    .f = function(sim) {
      simulate_chains(
        # simulation controls
        index_cases = initial_cases,
        statistic = "size",
        stat_max = 500,
        # offspring
        offspring_dist = rnbinom,
        mu = offspring_fit$estimate["mu"],
        size = offspring_fit$estimate["size"],
        # generation
        generation_time = function(x) generate(x = generation_time, times = x)
      ) %>%
        # creates a column with the chain ID number
        mutate(chain_id = sim)
    }
  ) %>%
  # combine list outputs (for each chain ID) into a single data frame
  list_rbind()


# visualize ---------------------------------------------------------------

# daily aggregate of cases
simulated_chains_day <- simulated_chains_map %>%
  # use data.frame output from <epichains> object
  as_tibble() %>%
  # transform chain ID column to factor (categorical variable)
  mutate(chain_id = as_factor(chain_id)) %>%
  # get the round number (day) of infection times
  mutate(day = ceiling(time)) %>%
  # count the daily number of cases in each simulation (chain ID)
  count(chain_id, day, name = "cases") %>%
  # calculate the cumulative number of cases for each simulation (chain ID)
  group_by(chain_id) %>%
  mutate(cases_cumsum = cumsum(cases)) %>%
  ungroup()

# Visualize transmission chains by cumulative cases
ggplot() +
  # create grouped chain trajectories
  geom_line(
    data = simulated_chains_day,
    mapping = aes(
      x = day,
      y = cases_cumsum,
      group = chain_id
    ),
    color = "black",
    alpha = 0.25,
    show.legend = FALSE
  ) +
  # define a 100-case threshold
  geom_hline(aes(yintercept = 100), lty = 2) +
  labs(
    x = "Day",
    y = "Cumulative cases"
  )

# read --------------------------------------------------------------------

read_rds("data-out/set-01-contacts.rds")
read_rds("data-out/set-01-linelist.rds")

