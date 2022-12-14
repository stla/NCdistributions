% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/noncentral.R
\name{find_chisq_ncp}
\alias{find_chisq_ncp}
\title{Find non-centrality parameter}
\usage{
find_chisq_ncp(nu, q, p)
}
\arguments{
\item{nu}{degrees of freedom, a positive number}

\item{q}{a quantile}

\item{p}{probability corresponding to the quantile \code{q}}
}
\value{
The non-centrality parameter of the Chi-squared distribution with 
  cumulative probability \code{p} at the quantile \code{q}.
}
\description{
Find the non-centrality parameter of a Chi-squared distribution 
  given a quantile and its corresponding probability.
}
\examples{
ncp <- find_chisq_ncp(1, 3, 0.1)
pchisq(3, df = 1, ncp = ncp) # should be 0.1
}
