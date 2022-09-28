#include <Rcpp.h>
using namespace Rcpp;
#include <boost/math/distributions/non_central_t.hpp>
#include <boost/math/distributions/non_central_chi_squared.hpp>
#include <boost/math/distributions/non_central_f.hpp>
#include <boost/math/distributions/non_central_beta.hpp>
#include <boost/math/distributions/complement.hpp>
//#include <boost/math/distributions/normal.hpp>
//#include <boost/math/distributions/chi_squared.hpp>

//double rcpp_gamma_p_inv(double a, double q){
//  return boost::math::gamma_p_inv(a, q);
//}

// [[Rcpp::export]]
double rcpp_pt(double q, double nu, double delta){
  return boost::math::cdf(boost::math::non_central_t(nu, delta), q);
}

// [[Rcpp::export]]
double rcpp_qt(double p, double nu, double delta){
  return boost::math::quantile(boost::math::non_central_t(nu, delta), p);
}

// [[Rcpp::export]]
double rcpp_pchisq(double q, double nu, double delta){
  return boost::math::cdf(boost::math::non_central_chi_squared(nu, delta), q);
}

// [[Rcpp::export]]
double rcpp_pchisq_upper(double q, double nu, double delta){
  return boost::math::cdf(boost::math::complement(boost::math::non_central_chi_squared(nu, delta), q));
}

// [[Rcpp::export]]
double rcpp_qchisq(double p, double nu, double delta){
  return boost::math::quantile(boost::math::non_central_chi_squared(nu, delta), p);
}

// [[Rcpp::export]]
double rcpp_chisq_ncp(double nu, double q, double p){
  // gives lambda such that pchisq(q, nu, lambda) = p
  return boost::math::non_central_chi_squared::find_non_centrality(nu, q, p);
}

// [[Rcpp::export]]
double rcpp_chisq_nu(double ncp, double q, double p){
  // gives nu such that pchisq(q, nu, ncp) = p
  return boost::math::non_central_chi_squared::find_degrees_of_freedom(ncp, q, p);
}

// [[Rcpp::export]]
double rcpp_pf(double q, double nu1, double nu2, double ncp){
  return boost::math::cdf(boost::math::non_central_f(nu1, nu2, ncp), q);
}

// [[Rcpp::export]]
double rcpp_qf(double p, double nu1, double nu2, double ncp){
  return boost::math::quantile(boost::math::non_central_f(nu1, nu2, ncp), p);
}

// [[Rcpp::export]]
double rcpp_pbeta(double q, double a, double b, double ncp){
  return boost::math::cdf(boost::math::non_central_beta(a, b, ncp), q);
}

// [[Rcpp::export]]
double rcpp_qbeta(double p, double a, double b, double ncp){
  return boost::math::quantile(boost::math::non_central_beta(a, b, ncp), p);
}

