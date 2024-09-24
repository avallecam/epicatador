library(epiparameter) # access to library of parameters 
library(magrittr) # get the pipe %>% operator 

# n=1
epiparameter_db(
  disease = "adenovirus"
)

#' pero este es un caso particular
#' accedamos a los parametros de otra enfermedad

# n=14
epiparameter_db(
  disease = "mpox"
)

#' detallar distribucion epidemiologica

# n=5
epiparameter_db(
  disease = "mpox",
  epi_dist = "serial interval"
)

#' detallar que requerimos una unica distribucion

# n=1
epiparameter_db(
  disease = "mpox",
  epi_dist = "serial interval",
  single_epiparameter = TRUE
) 

# n=1 grafico
epiparameter_db(
  disease = "mpox",
  epi_dist = "serial interval",
  single_epiparameter = TRUE
) %>% 
  plot()

# cambiar enfermedad
epiparameter_db(
  disease = "ebola",
  epi_dist = "serial interval",
  single_epiparameter = TRUE
) %>% 
  plot()

# cambiar enfermedad y distribucion
epiparameter_db(
  disease = "influenza",
  epi_dist = "generation time",
  single_epiparameter = TRUE
) %>% 
  plot()

#' retornemos a mpox y serial interval
#' para extraer estadisticas resumen
#' asignar el resultado a un objeto

# extraer estadisticas de resumen
mpox_serial_time <- epiparameter_db(
  disease = "mpox",
  epi_dist = "serial interval",
  single_epiparameter = TRUE
)

# extraer media de la distribuion
mpox_serial_time$summary_stats$mean

# extraer dispersion de la distribucion
mpox_serial_time$summary_stats$sd

# tarea: como extraer la incertidumbre de la media y dispersion?
# al 95% de intervalo de confianza
mpox_serial_time$summary_stats$mean_ci_limits
mpox_serial_time$summary_stats$sd_ci_limits

# mean(mpox_serial_time)