test_that("identifier selection and global reading agree across backends", {
  path <- system.file("extdata", "sample_camels_pe", package = "rcamelspe")
  for (backend in c(TRUE, FALSE)) {
    individual <- load_pe_timeseries(
      "PE_110139", c("date", "gauge_id", "prec_var", "flow_obs"),
      "2000-01-01", "2000-01-10", path, use_arrow = backend
    )
    master <- load_pe_timeseries(
      "PE_110139", c("date", "gauge_id", "prec_var", "flow_obs"),
      "2000-01-01", "2000-01-10", path, use_arrow = backend, global = TRUE
    )
    expect_equal(individual, master, ignore_attr = TRUE)
    expect_equal(nrow(master), 10)
    expect_equal(names(master), c("date", "gauge_id", "prec_var", "flow_obs"))
    expect_true(anyNA(master$flow_obs))
  }
  expect_equal(nrow(read_timeseries("PE_110139", global = TRUE,
    vars = "date", path = path)), 366)
})

test_that("invalid dates and partial variable names are rejected", {
  path <- system.file("extdata", "sample_camels_pe", package = "rcamelspe")
  expect_error(load_pe_timeseries(path = path, start_date = NA), "single valid")
  expect_error(load_pe_timeseries(path = path, start_date = character()), "single valid")
  expect_error(load_pe_timeseries(path = path, start_date = "2001-01-01",
    end_date = "2000-01-01"), "must not be after")
  expect_error(load_pe_timeseries(path = path, variables = "pre"), "Unknown")
})

test_that("download wrappers forward release and path preferences", {
  captured <- NULL
  local_mocked_bindings(download_pe_data = function(...) {
    captured <<- list(...)
    "archive.zip"
  })
  expect_equal(download_camels_pe(tempdir(), version = "1.0", unzip = FALSE,
    set_path = FALSE), "archive.zip")
  expect_identical(captured$version, "1.0")
  expect_false(captured$set_path)
})

test_that("archive-only cached downloads preserve the configured path", {
  dest <- tempfile()
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE))
  archive <- file.path(dest, "CAMELS-PE_v1.0.1.zip")
  file.create(archive)
  old <- getOption("rcamelspe.path")
  on.exit(options(rcamelspe.path = old), add = TRUE)
  options(rcamelspe.path = "unchanged")
  expect_equal(download_pe_data(dest, unzip = FALSE, quiet = TRUE), archive)
  expect_identical(getOption("rcamelspe.path"), "unchanged")
  expect_error(download_pe_data(dest, version = "unknown"), "Unsupported")
  expect_error(download_pe_data(), "Please provide a valid destination directory")
})
