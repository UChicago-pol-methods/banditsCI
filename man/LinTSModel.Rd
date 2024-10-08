% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment_utils.R
\name{LinTSModel}
\alias{LinTSModel}
\title{Linear Thompson Sampling model.}
\usage{
LinTSModel(
  K,
  p = NULL,
  floor_start,
  floor_decay,
  num_mc = 100,
  is_contextual = TRUE
)
}
\arguments{
\item{K}{Integer. Number of arms. Must be a positive integer.}

\item{p}{Integer. Dimension of the contextual vector, if \code{is_contextual} is set to \code{TRUE}. Otherwise, \code{p} is ignored. Must be a positive integer.}

\item{floor_start}{Numeric. Starting value of the floor. Must be a positive number.}

\item{floor_decay}{Numeric. Decay rate of the floor. Must be a positive number between 0 and 1.}

\item{num_mc}{Integer. Number of Monte Carlo simulations used to approximate the expected reward. Must be a positive integer. Default is 100.}

\item{is_contextual}{Logical. Indicates whether the problem is contextual or not. Default is \code{TRUE}.}
}
\value{
A list containing the parameters of the LinTSModel.
}
\description{
Creates a linear Thompson Sampling model for multi-armed bandit problems.
}
\examples{
model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100,
                    is_contextual = TRUE)

}
