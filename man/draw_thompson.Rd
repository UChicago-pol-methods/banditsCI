% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment_utils.R
\name{draw_thompson}
\alias{draw_thompson}
\title{Thompson Sampling draws.}
\usage{
draw_thompson(model, start, end, xs = NULL)
}
\arguments{
\item{model}{List. Contains the parameters of the model, generated by \code{LinTSModel()}.}

\item{start}{Integer. Starting index of observations for which arms are to be drawn. Must be a positive integer.}

\item{end}{Integer. Ending index of the observations for which arms are to be drawn. Must be an integer greater than or equal to \code{start}.}

\item{xs}{Optional matrix. Covariates of shape \code{[A, p]}, where \code{p} is the number of features, if the LinTSModel is contextual. Default is \code{NULL}. Must not contain NA values.}
}
\value{
A list containing the drawn arms (\code{w}) and their corresponding probabilities (\code{ps}).
}
\description{
Draws arms from a LinTS or non-contextual TS agent for multi-armed bandit problems.
}
\examples{
set.seed(123)
model <- LinTSModel(K = 5, p = 3, floor_start = 1, floor_decay = 0.9, num_mc = 100,
                    is_contextual = TRUE)
draws <- draw_thompson(model = model, start = 1, end = 10,
                       xs = matrix(rnorm(30), ncol = 3))

}
