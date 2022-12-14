% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/noncentral.R
\name{ChiSquaredDistribution}
\alias{ChiSquaredDistribution}
\alias{pchisq_boost}
\alias{qchisq_boost}
\title{The non-central Chi-squared distribution}
\usage{
pchisq_boost(q, nu, ncp = 0, lower.tail = TRUE)

qchisq_boost(p, nu, ncp = 0, lower.tail = TRUE)
}
\arguments{
\item{q}{numeric vector of quantiles}

\item{nu}{degrees of freedom parameter, a positive number}

\item{ncp}{non-centrality parameter, a non-negative number}

\item{lower.tail}{Boolean, whether to use the complementary probabilities}

\item{p}{numeric vector of probabilities}
}
\value{
A numeric vector of probabilities (\code{pchisq_boost}) or a 
  numeric vector of quantiles (\code{qchisq_boost}).
}
\description{
Distribution function and quantile function for the non-central 
  Chi-squared distribution.
}
