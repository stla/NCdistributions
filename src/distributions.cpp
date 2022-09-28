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
NumericVector rcpp_pt(NumericVector q, double nu, double delta, bool lower){
  int n = q.size();
  NumericVector out(n);
  boost::math::non_central_t dist(nu, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(dist, q(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(boost::math::complement(dist, q(i)));
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector rcpp_qt(NumericVector p, double nu, double delta, bool lower){
  int n = p.size();
  NumericVector out(n);
  boost::math::non_central_t dist(nu, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(dist, p(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(boost::math::complement(dist, p(i)));
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector rcpp_pchisq(NumericVector q, double nu, double delta, bool lower) {
  int n = q.size();
  NumericVector out(n);
  boost::math::non_central_chi_squared dist(nu, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(dist, q(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(boost::math::complement(dist, q(i)));
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector rcpp_qchisq(NumericVector p, double nu, double delta, bool lower) {
  int n = p.size();
  NumericVector out(n);
  boost::math::non_central_chi_squared dist(nu, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(dist, p(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(boost::math::complement(dist, p(i)));
    }
  }
  return out;
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
NumericVector rcpp_pf(NumericVector q, double nu1, double nu2, double delta, bool lower){
  int n = q.size();
  NumericVector out(n);
  boost::math::non_central_f dist(nu1, nu2, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(dist, q(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(boost::math::complement(dist, q(i)));
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector rcpp_qf(NumericVector p, double nu1, double nu2, double delta, bool lower) {
  int n = p.size();
  NumericVector out(n);
  boost::math::non_central_f dist(nu1, nu2, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(dist, p(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(boost::math::complement(dist, p(i)));
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector rcpp_pbeta(NumericVector q, double a, double b, double delta, bool lower) {
  int n = q.size();
  NumericVector out(n);
  boost::math::non_central_beta dist(a, b, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(dist, q(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::cdf(boost::math::complement(dist, q(i)));
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector rcpp_qbeta(NumericVector p, double a, double b, double delta, bool lower) {
  int n = p.size();
  NumericVector out(n);
  boost::math::non_central_beta dist(a, b, delta);
  if(lower) {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(dist, p(i));
    }
  } else {
    for(int i = 0; i < n; i++) {
      out(i) = boost::math::quantile(boost::math::complement(dist, p(i)));
    }
  }
  return out;
}

