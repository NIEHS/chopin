% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gridding.R
\name{par_merge_grid}
\alias{par_merge_grid}
\title{Merge adjacent grid polygons with given rules}
\usage{
par_merge_grid(
  points_in = NULL,
  grid_in = NULL,
  grid_min_features = NULL,
  merge_max = 4L
)
}
\arguments{
\item{points_in}{\code{sf} or \code{SpatVector} object. Target points of computation.}

\item{grid_in}{\code{sf} or \code{SpatVector} object.
The grid generated by \code{\link{par_make_grid}}.}

\item{grid_min_features}{integer(1). Threshold to merge adjacent grids.}

\item{merge_max}{integer(1).
Maximum number of grids to merge per merged set. Default is 4.
For example, if the number of grids to merge is 20 and \code{merge_max} is 10,
the function will split the 20 grids into two sets of 10 grids.}
}
\value{
A \code{sf} or \code{SpatVector} object of computation grids.
}
\description{
Merge boundary-sharing (in "Rook" contiguity) grids with
fewer target features than the threshold.
This function strongly assumes that the input
is returned from the par_make_grid,
which has \code{"CGRIDID"} as the unique id field.
}
\note{
This function will not work properly if \code{grid_in} has
more than one million grids.
}
\examples{
\dontrun{
library(sf)
library(igraph)
library(dplyr)
library(spatstat.random)
dg <- sf::st_as_sfc(st_bbox(c(xmin = 0, ymin = 0, xmax = 8e5, ymax = 6e5)))
sf::st_crs(dg) <- 5070
dgs <- sf::st_as_sf(st_make_grid(dg, n = c(20, 15)))
dgs$CGRIDID <- seq(1, nrow(dgs))

dg_sample <- sf::st_sample(dg, kappa = 5e-9, mu = 15,
scale = 15000, type = "Thomas")
sf::st_crs(dg_sample) <- sf::st_crs(dg)
dg_merged <- par_merge_grid(sf::st_as_sf(dg_sample), dgs, 100)
plot(dg_merged$geometry)
}
}
\references{
\itemize{
\item Polsby DD, Popper FJ. (1991).
The Third Criterion: Compactness as a Procedural Safeguard Against
Partisan Gerrymandering. \emph{Yale Law & Policy Review},
9(2), 301–353. \href{http://hdl.handle.net/20.500.13051/17448}{Link}
}
}
\seealso{
Other Parallelization: 
\code{\link{par_cut_coords}()},
\code{\link{par_fallback}()},
\code{\link{par_grid}()},
\code{\link{par_group_grid}()},
\code{\link{par_hierarchy}()},
\code{\link{par_make_grid}()},
\code{\link{par_make_gridset}()},
\code{\link{par_multirasters}()}
}
\author{
Insang Song
}
\concept{Parallelization}
