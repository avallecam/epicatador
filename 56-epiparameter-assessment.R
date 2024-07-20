
# paquetes ----------------------------------------------------------------

library(epiparameter)
library(EpiNow2)

# extraer un parametro ----------------------------------------------------

# ebola serial interval
ebola_serial <-
  epiparameter::epidist_db(
    disease = "ebola",
    epi_dist = "serial",
    single_epidist = TRUE
  )

ebola_serial

ebola_serial_params <- epiparameter::get_parameters(ebola_serial)

plot(ebola_serial)

# adaptarlo a EpiNow2 -----------------------------------------------------

EpiNow2::Gamma(
  shape = ebola_serial_params["shape"],
  scale = ebola_serial_params["scale"],
  max = 40
  )
