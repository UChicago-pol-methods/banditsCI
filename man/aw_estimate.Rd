% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adaptive_utils.R
\name{aw_estimate}
\alias{aw_estimate}
\title{Estimate policy value via non-contextual adaptive weighting.}
\usage{
aw_estimate(scores, policy, evalwts = NULL)
}
\arguments{
\item{scores}{Numeric matrix. AIPW scores, shape \code{[A, K]}, where \code{A} is the number of observations and \code{K} is the number of arms. Must not contain NA values.}

\item{policy}{Numeric matrix. Policy matrix \eqn{\pi(X_t, w)}, shape \code{[A, K]}. Must have the same shape as \code{scores} and must not contain NA values.}

\item{evalwts}{Optional numeric vector. Non-contextual adaptive weights \eqn{h_t}, length \code{A}, or \code{NULL}. Default is \code{NULL}.}
}
\value{
Numeric scalar. Estimated policy value.
}
\description{
Estimates the value of a policy based on AIPW scores and a policy matrix using non-contextual
adaptive weighting. If \code{evalwts} is not provided, uses equal weights for all observations.
}
\examples{
scores <- matrix(c(0.5, 0.8, 0.6,
                   0.3, 0.9, 0.2,
                   0.5, 0.7, 0.4,
                   0.8, 0.2, 0.6), ncol = 3, byrow = TRUE)
policy <- matrix(c(0.2, 0.3, 0.5,
                   0.6, 0.1, 0.3,
                   0.4, 0.5, 0.1,
                   0.2, 0.7, 0.1), ncol = 3, byrow = TRUE)
aw_estimate(scores = scores, policy = policy, evalwts = c(0.5, 1, 0.5, 1.5))

}
