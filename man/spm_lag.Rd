% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spm_lag.R
\name{spm_lag}
\alias{spm_lag}
\alias{spm_lag,sspm-method}
\alias{spm_lag,sspm_fit-method}
\title{Create lagged columns in a sspm smoothed data slot}
\usage{
spm_lag(sspm_object, vars, n = 1, default = "mean", ...)

\S4method{spm_lag}{sspm}(sspm_object, vars, n = 1, default = "mean", ...)

\S4method{spm_lag}{sspm_fit}(sspm_object, vars, n = 1, default = "mean", ...)
}
\arguments{
\item{sspm_object}{\strong{[sspm_dataset]} An object of class
\link[=sspm_dataset-class]{sspm_dataset}.}

\item{vars}{\strong{[character]} Names of the variables to lag.}

\item{n}{Positive integer of length 1, giving the number of positions to
lead or lag by}

\item{default}{Value used for non-existent rows. Defaults to \code{NA}.}

\item{...}{ a list of variables that are the covariates that this
    smooth is a function of. Transformations whose form depends on
    the values of the data are best avoided here: e.g. \code{s(log(x))}
    is fine, but \code{s(I(x/sd(x)))} is not (see \code{\link[mgcv]{predict.gam}}).}
}
\value{
Updated \code{sspm_object}.
}
\description{
This function is a wrapper around \link[dplyr:lead-lag]{lag} (note that not all
arguments are supported). The default value for the lag is the mean of the
series.
}
\examples{
\dontrun{
sspm_model <- sspm_model \%>\%
    spm_lag(vars = c("weight_per_km2_borealis_with_catch",
                     "weight_per_km2_all_predators"),
                     n = 1)
}

}
