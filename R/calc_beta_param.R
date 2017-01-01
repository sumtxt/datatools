#' Calculates Beta Distribution Parameters from its Mean and Variance Parameters
#' 
#' @param mu mean
#' @param var variance 
#' 
#' @return named vector with alpha/beta parameters. 
#
#' @export
calc_beta_param <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(c('alpha'=alpha, 'beta'=beta))
}

