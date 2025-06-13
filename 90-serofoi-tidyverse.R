
library(serofoi)
library(tidyverse)

data(chik2015)
chik2015

tictoc::tic()
fit_seromodel(
  serosurvey = chik2015,
  model_type = "time",
  iter = 5000
) %>%
  plot_seromodel(
    serosurvey = chik2015,
    size_text = 6,
    foi_max = 0.1
  )
tictoc::toc()