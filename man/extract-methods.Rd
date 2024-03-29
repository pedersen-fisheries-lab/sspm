% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-extract.R
\name{$,sspm_boundary-method}
\alias{$,sspm_boundary-method}
\alias{extract}
\alias{$,sspm_discrete_boundary-method}
\alias{$,sspm_dataset-method}
\alias{$,sspm-method}
\title{Extract methods}
\usage{
\S4method{$}{sspm_boundary}(x, name)

\S4method{$}{sspm_discrete_boundary}(x, name)

\S4method{$}{sspm_dataset}(x, name)

\S4method{$}{sspm}(x, name)
}
\arguments{
\item{x}{\strong{[sspm_...]} An object from this package.}

\item{name}{\strong{[character]} The name of the column}
}
\value{
The \code{data.frame} matching the request.
}
\description{
WIP extract variables from sspm objects
}
\examples{
\dontrun{
sspm_boundary$lat
sspm_dataset$biomass
}

}
