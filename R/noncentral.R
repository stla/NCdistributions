pt_boost <- function(q, nu, ncp = 0) {
  rcpp_pt(q, nu, ncp)
}

qt_boost <- function(p, nu, ncp = 0) {
  rcpp_qt(p, nu, ncp)
}

pchisq_boost <- function(q, nu, ncp = 0) {
  rcpp_pchisq(q, nu, ncp)
}

qchisq_boost <- function(p, nu, ncp = 0) {
  rcpp_qchisq(p, nu, ncp)
}

pf_boost <- function(q, nu1, nu2, ncp = 0) {
  rcpp_pf(q, nu1, nu2, ncp)
}

qf_boost <- function(p, nu1, nu2, ncp = 0) {
  rcpp_pf(p, nu1, nu2, ncp)
}

pbeta_boost <- function(q, a, b, ncp = 0) {
  rcpp_pf(q, a, b, ncp)
}

qbeta_boost <- function(p, a, b, ncp = 0) {
  rcpp_pf(p, a, b, ncp)
}


