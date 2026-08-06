# futurize compatibility helpers for chopin
#
# Futurize transpiles known apply-style calls (e.g., lapply/Map/foreach).
# chopin is not currently a futurize-registered package, so the wrappers below
# use supported outer calls and invoke chopin functions inside worker tasks.

if (!requireNamespace("futurize", quietly = TRUE)) {
  install.packages("futurize")
}

if (!requireNamespace("future.apply", quietly = TRUE)) {
  install.packages("future.apply")
}

library(chopin)
library(futurize)
library(future)


# Batch raster extraction using futurize over base::lapply
fz_extract_at_many <- function(
    rasters,
    y,
    id,
    func = "mean",
    ..., 
    .bind = TRUE
) {
  if (inherits(y, "SpatVector")) {
    y <- sf::st_as_sf(y)
  }

  out <- futurize::futurize(
    lapply(rasters, function(xi) {
      res <- chopin::extract_at(x = xi, y = y, id = id, func = func, ...)
      res$base_raster <- if (is.character(xi)) xi else NA_character_
      res
    }),
    options = futurize::futurize_options(seed = TRUE)
  )

  if (!.bind) {
    return(out)
  }

  collapse::rowbind(out, fill = TRUE)
}


# Batch point/polygon summaries using futurize over base::Map
fz_summarize_pp_many <- function(
    polys,
    points,
    target_fields = NULL,
    id_x = "ID",
    fun = mean,
    ...,
    .bind = TRUE
) {
  stopifnot(length(polys) == length(points))

  out <- futurize::futurize(
    Map(
      f = function(x, y) {
        chopin::summarize_pp(
          x = x,
          y = y,
          target_fields = target_fields,
          id_x = id_x,
          fun = fun,
          ...
        )
      },
      polys,
      points
    ),
    options = futurize::futurize_options(seed = TRUE)
  )

  if (!.bind) {
    return(out)
  }

  collapse::rowbind(out, fill = TRUE)
}


# Example setup ---------------------------------------------------------------

set.seed(2026)
options(sf_use_s2 = FALSE)

ncpath <- system.file("gpkg/nc.gpkg", package = "sf")
nc <- terra::vect(ncpath)
nc <- terra::project(nc, "EPSG:5070")

rrast <- terra::rast(nc, nrow = 300, ncol = 660)
terra::values(rrast) <- rgamma(n = terra::ncell(rrast), shape = 4, rate = 2)

rpnt <- terra::spatSample(rrast, 16L, as.points = TRUE)
rpnt$pid <- sprintf("ID-%02d", seq_len(16L))
rpnt <- sf::st_as_sf(rpnt)

tdir <- tempdir(check = TRUE)
rpaths <- file.path(tdir, sprintf("test_%02d.tif", 1:3))
invisible(lapply(rpaths, function(p) terra::writeRaster(rrast, p, overwrite = TRUE)))

# Use one future layer at a time to avoid nested oversubscription.
future::plan(future::multisession, workers = 2L)

res_extract <-
  fz_extract_at_many(
    rasters = rpaths,
    y = rpnt,
    id = "pid",
    func = "mean",
    radius = 1000
  )

print(utils::head(res_extract))

future::plan(future::sequential)


# Diagnostics ----------------------------------------------------------------

print(futurize::futurize_supported_packages())
print(try(futurize::futurize_supported_functions("chopin"), silent = TRUE))
