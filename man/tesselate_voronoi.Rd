% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tesselate_voronoi.R
\name{tesselate_voronoi}
\alias{tesselate_voronoi}
\title{Perform voronoi tesselation}
\usage{
tesselate_voronoi(
  boundaries,
  with,
  boundary = "sfa",
  sample_surface = FALSE,
  sample_points = TRUE,
  nb_samples = NULL,
  min_size = 1500,
  stratify = TRUE,
  seed = 1
)
}
\arguments{
\item{boundaries}{\strong{[sf]} The boundaries to be used.}

\item{with}{\strong{[sf]} A set of data points to use for voronoisation.}

\item{boundary}{\strong{[character]} The column in \code{boundaries} that is to
be used for the stratified sampling.}

\item{sample_surface}{\strong{[logical]} Whether to sample the surfaces in
\code{boundaries}, Default to \code{FALSE}.}

\item{sample_points}{\strong{[logical]} Whether to sample points from \code{with} or
to take all points in \code{with}. Default to \code{TRUE}.}

\item{nb_samples}{\strong{[named character vector]} The number of samples to draw
by boundary polygons (must bear the levels of \code{boundary} as names
or be a single value to be applied to each level).}

\item{min_size}{\strong{[numeric]} The minimum size for a polygon above which it
will be merged (in km2).}

\item{stratify}{\strong{[logical]} Whether the discretization happens within the
boundaries or whether the whole area is to be used (default to TRUE).}

\item{seed}{\strong{[numeric]} Passed onto \code{\link[base:Random]{set.seed()}},
important for reproducibility of sampling.}
}
\value{
A named list with three elements (each an \code{sf} object):
* \code{patches}, the voronoi polygons generated
* \code{points}, the points used for the tessellation.
}
\description{
Generates voronoi polygons by first performing stratified sampling across
boundary polygons, then by running  the voronoisation with
\link[sf:geos_unary]{st_voronoi()}.
}
\examples{
data(borealis_simulated, package = "sspm")
data(sfa_boundaries, package = "sspm")
tesselate_voronoi(sfa_boundaries, with = borealis, sample_surface = TRUE,
                  boundary = "sfa", nb_samples = 10)

}
