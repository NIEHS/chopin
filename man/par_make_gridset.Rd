% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gridding.R
\name{par_make_gridset}
\alias{par_make_gridset}
\title{Get a set of computational grids}
\usage{
par_make_gridset(
  input,
  mode = c("grid", "grid_advanced", "grid_quantile"),
  nx = 10L,
  ny = 10L,
  grid_min_features = 30L,
  padding = NULL,
  unit = NULL,
  quantiles = NULL,
  merge_max = NULL,
  ...
)
}
\arguments{
\item{input}{sf or Spat* object.}

\item{mode}{character(1). Mode of region construction.
One of
\itemize{
\item \code{"grid"} (simple grid regardless of the number of features in each grid)
\item \code{"grid_advanced"} (merging adjacent grids with
smaller number of features than \code{grid_min_features}).
The argument \code{grid_min_features} should be specified.
\item \code{"grid_quantile"} (x and y quantiles): an argument \code{quantiles} should
be specified.
}}

\item{nx}{integer(1). The number of grids along x-axis.}

\item{ny}{integer(1). The number of grids along y-axis.}

\item{grid_min_features}{integer(1). A threshold to merging adjacent grids}

\item{padding}{numeric(1). A extrusion factor to make buffer to
clip actual datasets. Depending on the length unit of the CRS of input.}

\item{unit}{character(1). The length unit for padding (optional).
units::set_units is used for padding when sf object is used.
See \href{https://cran.r-project.org/web/packages/units/vignettes/measurement_units_in_R.html}{link}
for the list of acceptable unit forms.}

\item{quantiles}{numeric. Quantiles for \code{grid_quantile} mode.}

\item{merge_max}{integer(1). Maximum number of grids to merge
per merged set.}

\item{...}{arguments passed to the internal function}
}
\value{
A list of two,
\itemize{
\item \code{original}: exhaustive and non-overlapping
grid polygons in the class of input
\item \code{padded}: a square buffer of each polygon in
\code{original}. Used for computation.
}
}
\description{
Using input points, the bounding box is split to
the predefined numbers of columns and rows.
Each grid will be buffered by the radius.
}
\examples{
# data
library(sf)
ncpath <- system.file("shape/nc.shp", package = "sf")
nc <- read_sf(ncpath)
nc <- st_transform(nc, "EPSG:5070")
# run: nx and ny should strictly be integers
# In the example below, nx is 12L, not 12.
nc_comp_region <-
  par_make_gridset(
    nc,
    mode = "grid",
    nx = 12L, ny = 8L,
    padding = 10000)
par(mfcol = c(1, 2))
plot(nc_comp_region$original)
plot(nc_comp_region$padded)
}
\seealso{
\link{par_cut_coords}, \link{par_merge_grid}

Other Parallelization: 
\code{\link{par_cut_coords}()},
\code{\link{par_fallback}()},
\code{\link{par_grid}()},
\code{\link{par_group_grid}()},
\code{\link{par_hierarchy}()},
\code{\link{par_make_grid}()},
\code{\link{par_merge_grid}()},
\code{\link{par_multirasters}()}
}
\author{
Insang Song
}
\concept{Parallelization}
