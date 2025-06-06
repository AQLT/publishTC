% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turningpoints.R
\name{turning_points}
\alias{turning_points}
\alias{upturn}
\alias{downturn}
\alias{unwanted_ripples}
\title{Detect turning points in a time series}
\usage{
turning_points(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)

upturn(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)

downturn(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)

unwanted_ripples(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)
}
\arguments{
\item{x}{the input time series.}

\item{start, end}{the interval where to find turning points.}

\item{digits}{number of digits used for the comparison of the values.}

\item{k, m}{number of observation before and after the turning point (see details).}
}
\description{
\code{turning_points()} returns the number of downturns (\code{downturn()}) and upturns (\code{upturn()}) in a time series.
\code{unwanted_ripples()} returns the number of unwanted ripples in a time series.
}
\details{
Zellner, Hong, et Min (1991) definition is used \eqn{k=3}, \eqn{m=1}:
\itemize{
\item we have an upturn at date \eqn{t} when
\deqn{
y_{t-k}\geq\cdots\geq y_{t-1}<y_t\leq y_{t+1}\leq\cdots y_{t+m}
}
\item we have a downturn at date \eqn{t} when
}

\deqn{
y_{t-k}\leq\cdots\leq y_{t-1}>y_t\geq y_{t+1}\geq\cdots y_{t+m}
}

An unwanted ripple is defined whenever two downturns or upturns occur within a 10 month period
(i.e.: small cycles of less than 11 months).
}
