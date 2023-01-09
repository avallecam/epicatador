#' 
#' https://github.com/epiverse-trace
#' 
#' first,
#' evaluate naming conventions
#' following rOpenSci guidelines
#' 
if (!require("pacman")) install.packages("pacman")
pacman::p_install_gh("epiverse-trace/epiparameter")
pacman::p_install_gh("epiverse-trace/finalsize")
pacman::p_install("finalsize")
pacman::p_install("socialmixr")
pacman::p_install("flashr")
