
# periodo de incubacion ---------------------------------------------------

# cargar paquete
library(epiparameter)

# acceder a parametros
periodo_incubacion <- epiparameter_db(
  disease = "adenovirus",
  epi_name = "incubation period",
  single_epiparameter = TRUE
)

# graficar distribucion
plot(periodo_incubacion)

# imprimir metadatos
periodo_incubacion

# imprimir estadisticos de resumen
periodo_incubacion$summary_stats

# intervalo serial --------------------------------------------------------

# cargar paquete
library(epiparameter)

# acceder a parametros
intervalo_serial <- epiparameter_db(
  disease = "mpox",
  epi_name = "serial interval",
  single_epiparameter = TRUE
)

# graficar distribucion
plot(intervalo_serial)

# imprimir metadatos
intervalo_serial

# imprimir estadisticos de resumen
intervalo_serial$summary_stats


# intervalo de generacion -------------------------------------------------

# cargar paquete
library(epiparameter)

# acceder a parametros
intervalo_generacion <- epiparameter_db(
  disease = "influenza",
  epi_name = "generation time",
  single_epiparameter = TRUE
)

# graficar distribucion
plot(intervalo_generacion)

# imprimir metadatos
intervalo_generacion

# imprimir estadisticos de resumen
intervalo_generacion$summary_stats
