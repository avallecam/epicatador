
# paquetes ----------------------------------------------------------------

library(deSolve)
library(tidyverse)
library(cowplot)

# parametros --------------------------------------------------------------

# Parámetros del modelo SIRS
beta <- 0.3      # Tasa de transmisión
gamma <- 1/14    # Tasa de recuperación (período infeccioso de 14 días)
omega <- 1/365   # Tasa de pérdida de inmunidad (inmunidad promedio de 1 año)
mu <- 1/(70*365) # Tasa de natalidad (esperanza de vida promedio de 70 años)
nu <- 1/(70*365) # Tasa de mortalidad (esperanza de vida promedio de 70 años)
N <- 1000        # Tamaño de la población total


# modelo ------------------------------------------------------------------

# Modelo SIRS con natalidad y mortalidad
sirs_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- mu * N - beta * S * I / N + omega * R - nu * S
    dI <- beta * S * I / N - gamma * I - nu * I
    dR <- gamma * I - omega * R - nu * R
    list(c(dS, dI, dR))
  })
}

# resolver ecuacion -------------------------------------------------------

# Tiempo de simulación (en días)
times <- seq(0, 365*2, by = 1)  # 2 años

# Condiciones iniciales
initial_conditions <- c(
  S = N-1,
  I = 1,
  R = 0
)

# Parámetros del modelo
params <- c(
  beta = beta,
  gamma = gamma,
  omega = omega,
  mu = mu,
  nu = nu,
  N = N
)

# Resolver las ecuaciones diferenciales
out <- ode(
  y = initial_conditions,
  times = times,
  func = sirs_model,
  parms = params
)

# Convertir a data frame y agregar columnas de tiempo en años y semanas
out <- as.data.frame(out)
out$years <- out$time / 365
out$weeks <- out$time / 7

# graficos ----------------------------------------------------------------


# Gráficos
p0 <- ggplot(out, aes(x = years, y = (R + I + S))) +
  geom_line(color = 'grey68', size = 1) +
  ggtitle('Población total') + 
  ylab('Número') + xlab('Años')

p1 <- ggplot(out, aes(x = years, y = S)) + 
  geom_line(color = 'royalblue') +
  ggtitle('Población Susceptible') + 
  ylab('Número') + xlab('Años')

p2 <- ggplot(out, aes(x = years, y = I)) + 
  geom_line(color = 'firebrick') +
  ggtitle('Población Infectada') + 
  ylab('Número') + xlab('Años')

p3 <- ggplot(out, aes(x = years, y = R)) + 
  geom_line(color = 'olivedrab') +
  ggtitle('Población Recuperada') + 
  ylab('Número') + xlab('Años')

# Unir los gráficos
plot_grid(p0, p1, p2, p3, ncol = 2)
