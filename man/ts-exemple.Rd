% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ts-exemple}
\alias{ts-exemple}
\alias{cars_registrations}
\alias{french_ipi}
\alias{fred}
\alias{simulated_data}
\alias{etip}
\title{Data set examples}
\format{
An object of class \code{ts} of length 177.

An object of class \code{mts} (inherits from \code{ts}, \code{matrix}, \code{array}) with 416 rows and 3 columns.

An object of class \code{mts} (inherits from \code{ts}, \code{matrix}, \code{array}) with 766 rows and 2 columns.

An object of class \code{mts} (inherits from \code{ts}, \code{matrix}, \code{array}) with 84 rows and 6 columns.

An object of class \code{ts} of length 590.
}
\usage{
cars_registrations

french_ipi

fred

simulated_data

etip
}
\description{
All data sets examples used in the paper Quartier-la-Tente (2025).
}
\details{
\itemize{
\item \code{cars_registrations}: monthly new passenger cars registrations in France, published in October 2024.
\item \code{french_ipi}: monthly industrial production index in France for Crude Petroleum, Motor Vehicles and Manufacturing published in October 2024.
\item \code{fred}: the series CE16OV (Civilian Employment Level) and RETAILx (Retail and Food Services Sales) from the FRED-MD database published in November 2022.
\item \code{simulated_data}: simulated trends of degree 0, 1 and 2 with an Additive Outlier (AO) or Level Shift (LS) in January 2022
}

\code{etip} corresponds to the Expected trend in production (balance of opinion) in the French Manufacturing industry, published in May 2025 in
the monthly business survey in goods-producing industries by the INSEE.
}
\references{
Quartier-la-Tente, A. (2025). Estimation de la tendance-cycle avec des méthodes robustes aux points atypiques. \url{https://github.com/AQLT/robustMA}.
McCracken, Michael W., et Serena Ng. 2016. FRED-MD: A Monthly Database for Macroeconomic Research. Journal of Business & Economic Statistics 34 (4): 574‑89. \url{https://doi.org/10.1080/07350015.2015.1086655}.
}
\keyword{datasets}
