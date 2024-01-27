# Generated from scomps_rmarkdown_litr.rmd: do not edit by hand

testthat::test_that("Processes are properly spawned and compute", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  ncpath <- system.file("shape/nc.shp", package = "sf")
  ncpoly <- terra::vect(ncpath) |>
    terra::project("EPSG:5070")
  ncpnts <-
    readRDS(
            testthat::test_path("..", "testdata", "nc_random_point.rds"))
  ncpnts <- terra::vect(ncpnts)
  ncpnts <- terra::project(ncpnts, "EPSG:5070")
  ncelev <-
    terra::unwrap(
      readRDS(testthat::test_path("..", "testdata", "nc_srtm15_otm.rds"))
    )
  terra::crs(ncelev) <- "EPSG:5070"
  names(ncelev) <- c("srtm15")

  ncsamp <-
    terra::spatSample(
      terra::ext(ncelev),
      1e4L,
      lonlat = FALSE,
      as.points = TRUE
    )
  ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))

  nccompreg <-
    get_computational_regions(
      input = ncpnts,
      mode = "grid",
      nx = 6L,
      ny = 4L,
      padding = 3e4L
    )
  res <-
    suppressWarnings(
      distribute_process_grid(
        grids = nccompreg,
        grid_target_id = NULL,
        fun_dist = extract_with_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )

  testthat::expect_error(
    suppressWarnings(
      distribute_process_grid(
        grids = nccompreg,
        grid_target_id = "1/10",
        fun_dist = extract_with_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )

  testthat::expect_error(
    suppressWarnings(
      distribute_process_grid(
        grids = nccompreg,
        grid_target_id = c(1, 100, 125),
        fun_dist = extract_with_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )


  testthat::expect_no_error(
    suppressWarnings(
      distribute_process_grid(
        grids = nccompreg,
        grid_target_id = "1:10",
        fun_dist = extract_with_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )


  testthat::expect_no_error(
    suppressWarnings(
      distribute_process_grid(
        grids = nccompreg,
        grid_target_id = c(1, 3),
        fun_dist = extract_with_buffer,
        points = ncpnts,
        surf = ncelev,
        qsegs = 90L,
        radius = 5e3L,
        id = "pid"
      )
    )
  )

  testthat::expect_true(is.list(nccompreg))
  testthat::expect_s4_class(nccompreg$original, "SpatVector")
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_true(!anyNA(unlist(res)))

  testthat::expect_no_error(
    suppressWarnings(
      resnas <-
        distribute_process_grid(
          grids = nccompreg,
          grid_target_id = "1:10",
          fun_dist = extract_with_buffer,
          points = ncpnts,
          surf = ncelev,
          qsegs = 90L,
          radius = -5e3L,
          id = "pid"
        )
    )
  )

  testthat::expect_s3_class(resnas, "data.frame")
  testthat::expect_true(anyNA(resnas))

  testthat::expect_no_error(
    suppressWarnings(
      resnas0 <-
        distribute_process_grid(
          grids = nccompreg,
          grid_target_id = "1:10",
          fun_dist = terra::nearest,
          x = ncpnts,
          y = ncsamp,
          id = "pid"
        )
    )
  )

})



testthat::test_that(
  "Processes are properly spawned and compute over hierarchy", {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    withr::local_options(list(sf_use_s2 = FALSE))

    ncpath <- testthat::test_path("..", "testdata", "nc_hierarchy.gpkg")
    nccnty <- terra::vect(ncpath, layer = "county")
    nctrct <- sf::st_read(ncpath, layer = "tracts")
    nctrct <- terra::vect(nctrct)
    ncelev <-
      terra::unwrap(
        readRDS(
          testthat::test_path("..", "testdata", "nc_srtm15_otm.rds")
        )
      )
    terra::crs(ncelev) <- "EPSG:5070"
    names(ncelev) <- c("srtm15")

    ncsamp <-
      terra::spatSample(
        terra::ext(ncelev),
        1e4L,
        lonlat = FALSE,
        as.points = TRUE
      )
    ncsamp$kid <- sprintf("K-%05d", seq(1, nrow(ncsamp)))

    testthat::expect_no_error(
      res <-
        suppressWarnings(
          distribute_process_hierarchy(
            regions = nccnty,
            split_level = "GEOID",
            fun_dist = extract_with_polygons,
            polys = nctrct,
            surf = ncelev,
            id = "GEOID",
            func = "mean"
          )
        )
    )

    testthat::expect_error(
      suppressWarnings(
        distribute_process_hierarchy(
          regions = nccnty,
          split_level = c(1, 2, 3),
          fun_dist = extract_with_polygons,
          polys = nctrct,
          surf = ncelev,
          id = "GEOID",
          func = "mean"
        )
      )
    )

    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_equal(!any(is.na(unlist(res))), TRUE)

    testthat::expect_no_error(
      suppressWarnings(
        resnas <-
          distribute_process_hierarchy(
            regions = nccnty,
            split_level = "GEOID",
            fun_dist = terra::nearest,
            polys = nctrct,
            surf = ncelev
          )
      )
    )

    testthat::expect_s3_class(resnas, "data.frame")
    testthat::expect_true(anyNA(resnas))

    testthat::expect_no_error(
      suppressWarnings(
        resnasx <-
          distribute_process_hierarchy(
            regions = nccnty,
            debug = TRUE,
            split_level = "GEOID",
            fun_dist = extract_with_buffer,
            points = terra::centroids(nctrct),
            surf = ncelev,
            id = "GEOID",
            radius = -1e3L
          )
      )
    )

    testthat::expect_no_error(
      suppressWarnings(
        resnasz <-
          distribute_process_hierarchy(
            regions = nccnty,
            debug = TRUE,
            split_level = "GEOID",
            fun_dist = terra::nearest,
            x = nctrct,
            y = ncsamp
          )
      )
    )
  }
)




testthat::test_that("generic function should be parallelized properly", {
  withr::local_package("terra")
  withr::local_package("sf")
  withr::local_package("future")
  withr::local_package("future.apply")
  withr::local_package("dplyr")
  withr::local_options(list(sf_use_s2 = FALSE))

  # main test
  pnts <- readRDS(testthat::test_path("..", "testdata", "nc_random_point.rds"))
  pnts <- terra::vect(pnts)
  rd1 <-
    terra::vect(testthat::test_path("..", "testdata", "ncroads_first.gpkg"))

  pnts <- terra::project(pnts, "EPSG:5070")
  rd1 <- terra::project(rd1, "EPSG:5070")
  # expect

  nccompreg <-
    get_computational_regions(
      input = pnts,
      mode = "grid",
      nx = 6L,
      ny = 4L,
      padding = 3e4L
    )
  future::plan(future::multicore, workers = 6L)
  testthat::expect_no_error(
    res <-
      suppressWarnings(
        distribute_process_grid(
          grids = nccompreg,
          fun_dist = terra::nearest,
          x = pnts,
          y = rd1
        )
      )
  )
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), nrow(pnts))

})


testthat::test_that(
  "Processes are properly spawned and compute over multirasters",
  {
    withr::local_package("terra")
    withr::local_package("sf")
    withr::local_package("future")
    withr::local_package("future.apply")
    withr::local_package("dplyr")
    withr::local_options(
      list(
        sf_use_s2 = FALSE,
        future.resolve.recursive = 2L
      )
    )

    ncpath <- testthat::test_path("..", "testdata", "nc_hierarchy.gpkg")
    nccnty <- terra::vect(ncpath, layer = "county")
    ncelev <-
      terra::unwrap(
        readRDS(
          testthat::test_path("..", "testdata", "nc_srtm15_otm.rds")
        )
      )
    terra::crs(ncelev) <- "EPSG:5070"
    names(ncelev) <- c("srtm15")
    tdir <- tempdir(check = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test1.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test2.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test3.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test4.tif"), overwrite = TRUE)
    terra::writeRaster(ncelev, file.path(tdir, "test5.tif"), overwrite = TRUE)

    testfiles <- list.files(tdir, pattern = "tif$", full.names = TRUE)
    testthat::expect_no_error(
      res <- distribute_process_multirasters(
        filenames = testfiles,
        fun_dist = extract_with_polygons,
        polys = nccnty,
        surf = ncelev,
        id = "GEOID",
        func = "mean"
      )
    )
    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_true(!anyNA(res))

    testfiles_corrupted <- c(testfiles, "/home/runner/fallin.tif")
    testthat::expect_condition(
      resnas <- distribute_process_multirasters(
        filenames = testfiles_corrupted,
        fun_dist = extract_with_polygons,
        polys = nccnty,
        surf = ncelev,
        id = "GEOID",
        func = "mean"
      )
    )

    testthat::expect_s3_class(resnas, "data.frame")
    testthat::expect_true(anyNA(resnas))

    # error case
    future::plan(future::sequential)
    testthat::expect_condition(
      resnasx <- distribute_process_multirasters(
        filenames = testfiles_corrupted,
        debug = TRUE,
        fun_dist = extract_with_polygons,
        polys = nccnty,
        surf = ncelev,
        id = "GEOID",
        func = "mean"
      )
    )
  }
)
