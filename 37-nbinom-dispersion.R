#' summative 01
#' 
#' goal:
#' 
#' identify the relationship between
#' 
#' poisson
#' negative binomial
#' mean
#' size
#' 
#' offspring distribution
#' individual reproductive number
#' R0
#' k
#' 
#' dispersion
#' variance
#' heterogeneity
#' mean/variance ratio
#' overdispersion
#' 

# summative ---------------------------------------------------------------

#' if we set 
#' mean = 10
#' inverse-dispersion = 19.2
#' we get sd = 3.9
x <- 0:40
mu_test <- 2

size_or_inversedispersion <- 48
# dispersion is the reciprocal of size (r)
1/size_or_inversedispersion
plot(dnbinom(x, mu=mu_test, size=size_or_inversedispersion))
# sd = sqrt(variance)
get_variance <- mu_test + ((1/size_or_inversedispersion)*(mu_test)^2)
sqrt(get_variance)
# ratio mean/variance
mu_test/get_variance

size_or_inversedispersion <- 0.6
# dispersion is the reciprocal of size (r)
1/size_or_inversedispersion
plot(dnbinom(x, mu=mu_test, size=size_or_inversedispersion))
# sd = sqrt(variance)
get_variance <- mu_test + ((1/size_or_inversedispersion)*(mu_test)^2)
sqrt(get_variance)
# ratio mean/variance
mu_test/get_variance


#' a high k or size or inverse-dispersion (48)
#' a low dispersion (0.02)
#' low variance (1.44 sd)
#' ratio mean/variance closer to 1
#' (closer to the poisson special case)
#' 
#' a low k or size or inverse-dispersion (0.6)
#' a high dispersion (0.67)
#' high variance? (sd = 2.12)
#' ratio mean/variance further from 1
#' (closer to the poisson special case)
#' 
#' thus,
#' we are going to read that:
#' 
#' the "dispersion" parameter 
#' refers to the "size" for base R
#' or 
#' refer to the "k" parameter for Lloyd-Smith
#' of the negative binomial distribution
#' 
#' k is inversely-proportional 
#' to variance (i.e, to heterogeneity)
#' - high k -> get low variance (low heterogeneity)
#' - low k -> get high variance (high heterogeneity)
#' 
#' when k approches infinity,
#' the negative binomial approaches poisson
#' 
#' recap ------------------------
#' 
#' negative binomial
#' two parameters: 
#' - mean (or R0, in offspring distribution)
#' - size (or k, named also dispersion)
#' 
#' variance =
#' mean(1 + mean/k)
#' 
#' higher k, 
#' (higher inverse-dispersion),
#' lower variance,
#' (variance closer to mean)
#' lower heterogeneity
#' 
#' lower k
#' (lower the inverse-dispersion),
#' higher the variance,
#' (variance further from mean)
#' higher  heterogeneity
#' 
#' useful when variance is higher than mean
#' for discrete data 
#' when observations exceed a positive range
#' whose sample variance exceeds the
#' sample mean.
#' thus, the observations are overdispersed
#' with respect to a Poisson distribution
#' 
#' a Poisson distribution can be defined as
#' an special case of Negative Binomial
#' when k approaches infinity
#' or
#' variance gets closer to mean
#' or
#' when mean/variance ratio gets closer to 1
#' 
#' 
#' 
