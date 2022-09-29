#' The non-central Student distribution
#' @description Distribution function and quantile function for the non-central 
#'   Student distribution.
#'
#' @param q numeric vector of quantiles
#' @param p numeric vector of probabilities
#' @param nu degrees of freedom parameter, a positive number
#' @param ncp non-centrality parameter
#' @param lower.tail Boolean, whether to use the complementary probabilities
#'
#' @return A numeric vector of probabilities (\code{pt_boost}) or a 
#'   numeric vector of quantiles (\code{qt_boost}).
#' @export
#' 
#' @name StudentDistribution
#'
#' @examples
#' pt_boost(100, nu = 2, ncp = 100)
pt_boost <- function(q, nu, ncp = 0, lower.tail = TRUE) {
  rcpp_pt(q, nu, ncp, lower.tail)
}

#' @rdname StudentDistribution
#' @export
qt_boost <- function(p, nu, ncp = 0, lower.tail = TRUE) {
  rcpp_qt(p, nu, ncp, lower.tail)
}

#' The non-central Chi-squared distribution
#' @description Distribution function and quantile function for the non-central 
#'   Chi-squared distribution.
#'
#' @param q numeric vector of quantiles
#' @param p numeric vector of probabilities
#' @param nu degrees of freedom parameter, a positive number
#' @param ncp non-centrality parameter, a non-negative number
#' @param lower.tail Boolean, whether to use the complementary probabilities
#'
#' @return A numeric vector of probabilities (\code{pchisq_boost}) or a 
#'   numeric vector of quantiles (\code{qchisq_boost}).
#' @export
#' 
#' @name ChiSquaredDistribution
pchisq_boost <- function(q, nu, ncp = 0, lower.tail = TRUE) {
  stopifnot(nu > 0, ncp >= 0)
  rcpp_pchisq(q, nu, ncp, lower.tail)
}

#' @rdname ChiSquaredDistribution
#' @export
qchisq_boost <- function(p, nu, ncp = 0, lower.tail = TRUE) {
  stopifnot(nu > 0, ncp >= 0)
  rcpp_qchisq(p, nu, ncp, lower.tail)
}

#' The non-central Fisher distribution
#' @description Distribution function and quantile function for the non-central 
#'   Fisher distribution.
#'
#' @param q numeric vector of quantiles
#' @param p numeric vector of probabilities
#' @param nu1 numerator degrees of freedom parameter, a positive number
#' @param nu2 denominator degrees of freedom parameter, a positive number
#' @param ncp non-centrality parameter, a non-negative number
#' @param lower.tail Boolean, whether to use the complementary probabilities
#'
#' @return A numeric vector of probabilities (\code{pf_boost}) or a 
#'   numeric vector of quantiles (\code{qf_boost}).
#' @export
#' 
#' @name FisherDistribution
pf_boost <- function(q, nu1, nu2, ncp = 0, lower.tail = TRUE) {
  stopifnot(nu1 > 0, nu2 > 0, ncp >= 0)
  rcpp_pf(q, nu1, nu2, ncp, lower.tail)
}

#' @rdname FisherDistribution
#' @export
qf_boost <- function(p, nu1, nu2, ncp = 0, lower.tail = TRUE) {
  stopifnot(nu1 > 0, nu2 > 0, ncp >= 0)
  rcpp_pf(p, nu1, nu2, ncp, lower.tail)
}

pbeta_boost <- function(q, a, b, ncp = 0, lower.tail = TRUE) {
  rcpp_pf(q, a, b, ncp, lower.tail)
}

qbeta_boost <- function(p, a, b, ncp = 0, lower.tail = TRUE) {
  rcpp_pf(p, a, b, ncp, lower.tail)
}


