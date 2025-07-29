library(deSolve)
library(tidyverse)

# Parameters
N <- 30000000
beta <- 1.46 / 7
sigma <- 1 / 3
gamma <- 1 / 7

# SEIR model function
seir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    list(c(dS, dE, dI, dR))
  })
}

# Initial conditions
init <- c(
  S = N - 1,
  E = 0,
  I = 1,
  R = 0
)

# Parameters vector
params <- c(beta = beta, sigma = sigma, gamma = gamma, N = N)

# Time steps (e.g., 365 days, 1 year)
times <- seq(0, 660, by = 1)

# Solve ODE
out <- ode(y = init, times = times, func = seir_model, parms = params)

# Convert to dataframe
out_df <- as.data.frame(out) %>%
  pivot_longer(cols = c("S", "E", "I", "R"), names_to = "Compartment", values_to = "Count")

# Plot results
ggplot(out_df, aes(x = time, y = Count, color = Compartment)) +
  geom_line(size = 1) +
  labs(title = "SEIR Model Simulation",
       x = "Days",
       y = "Number of Individuals") +
  theme_minimal()
