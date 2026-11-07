# Helper function to locate the test dataset
find_test_data_path <- function() {
  paths <- c(
    "raw-data/CAMELS-PE",
    "../../raw-data/CAMELS-PE",
    "../../../raw-data/CAMELS-PE",
    "../../../../raw-data/CAMELS-PE",
    "data-raw/CAMELS-PE",
    "../../data-raw/CAMELS-PE",
    "../../../data-raw/CAMELS-PE"
  )
  for (p in paths) {
    if (dir.exists(p)) {
      return(normalizePath(p))
    }
  }
  NULL
}

scrub_path <- function(lines) {
  # Replace absolute/relative path to dummy-path with [path]/dummy-path
  lines <- gsub("does not exist: ['\"`]?.*dummy-path['\"`]?", "does not exist: [path]/dummy-path", lines)
  lines
}

test_that("set_camels_pe_path and get_camels_pe_path work", {
  old_path <- getOption("rcamelspe.path")
  on.exit(options(rcamelspe.path = old_path))

  # Test setting path (expecting warning for non-existent path)
  expect_snapshot(set_camels_pe_path("dummy-path"), transform = scrub_path)
  expect_equal(getOption("rcamelspe.path"), normalizePath("dummy-path", mustWork = FALSE))

  # Test setting existing path with missing directories
  temp_dir <- tempfile("camels_test")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  expect_snapshot(set_camels_pe_path(temp_dir))

  # Test compatibility aliases
  suppressWarnings(set_camels_path(temp_dir))
  expect_equal(get_camels_path(), normalizePath(temp_dir, mustWork = FALSE))

  # Fallback to sample dataset when option is NULL
  options(rcamelspe.path = NULL)
  sample_p <- system.file("extdata", "sample_camels_pe", package = "rcamelspe")
  if (nzchar(sample_p)) {
    expect_equal(get_camels_pe_path(), normalizePath(sample_p))
  }
})

test_that("bundled sample dataset works out-of-the-box", {
  sample_path <- system.file("extdata", "sample_camels_pe", package = "rcamelspe")
  skip_if(!nzchar(sample_path) || !dir.exists(sample_path), "Sample dataset not found")

  old_path <- getOption("rcamelspe.path")
  on.exit(options(rcamelspe.path = old_path))
  set_camels_pe_path(sample_path)

  # 1. Metadata & Dictionary
  stations <- load_pe_metadata(type = "stations")
  expect_equal(nrow(stations), 2)
  expect_true(all(c("PE_110139", "PE_111151") %in% stations$gauge_id))

  dict <- read_dictionary()
  expect_s3_class(dict, "data.frame")
  expect_true("variable" %in% names(dict))

  stations_alias <- read_metadata()
  expect_equal(nrow(stations_alias), 2)

  # 2. Attributes
  attrs <- read_attributes(type = "topographic")
  expect_s3_class(attrs, "data.frame")
  expect_equal(nrow(attrs), 2)
  expect_true("area" %in% names(attrs))

  attrs_vars <- load_pe_attributes(attributes = "topographic", variables = "area")
  expect_equal(names(attrs_vars), c("gauge_id", "area"))

  # 3. Timeseries with date filters
  ts_all <- load_pe_timeseries()
  expect_equal(nrow(ts_all), 732) # 366 days leap year 2000 x 2 stations

  # Test date filtering
  ts_sub_date <- load_pe_timeseries(
    gauge_ids = "PE_110139",
    start_date = "2000-01-01",
    end_date = "2000-01-31"
  )
  expect_equal(nrow(ts_sub_date), 31)
  expect_s3_class(ts_sub_date$date, "Date")

  # Test read_timeseries alias
  ts_alias <- read_timeseries(
    gauge_id = "PE_110139",
    vars = c("date", "gauge_id", "prec"),
    start_date = "2000-01-01",
    end_date = "2000-01-10"
  )
  expect_equal(nrow(ts_alias), 10)
  expect_equal(names(ts_alias), c("date", "gauge_id", "prec"))

  # 4. Geospatial
  catchments <- read_geospatial(type = "catchments")
  expect_s3_class(catchments, "sf")
  expect_equal(nrow(catchments), 2)

  gauges <- read_geospatial(type = "gauges")
  expect_s3_class(gauges, "sf")
  expect_equal(nrow(gauges), 2)

  # 5. Plots
  p1 <- plot_timeseries(ts_alias, variable = "prec")
  expect_s3_class(p1, "ggplot")

  p2 <- plot_catchments(catchments, gauges = gauges)
  expect_s3_class(p2, "ggplot")

  p3 <- plot_attribute_map(catchments, attrs, variable = "area")
  expect_s3_class(p3, "ggplot")
})

test_that("load_pe_metadata works on full dataset if present", {
  path <- find_test_data_path()
  skip_if(is.null(path), "CAMELS-PE raw data directory not found")
  old_path <- getOption("rcamelspe.path")
  on.exit(options(rcamelspe.path = old_path), add = TRUE)
  set_camels_pe_path(path)

  # Load stations
  stations <- load_pe_metadata(type = "stations")
  expect_s3_class(stations, "data.frame")
  expect_true("gauge_id" %in% names(stations))
  expect_true("gauge_name" %in% names(stations))
  expect_equal(nrow(stations), 136)

  # Load dictionary
  dict <- load_pe_metadata(type = "dictionary")
  expect_s3_class(dict, "data.frame")
  expect_true("variable" %in% names(dict))
  expect_true("description" %in% names(dict))

  # Test filtering
  dict_filtered <- load_pe_metadata(type = "dictionary", category = "climatic")
  expect_true(all(dict_filtered$category == "climatic"))

  dict_var <- load_pe_metadata(type = "dictionary", variable = "flow_obs")
  expect_true(all(dict_var$variable == "flow_obs"))

  dict_file <- load_pe_metadata(type = "dictionary", file = "stations.csv")
  expect_true(all(dict_file$file == "stations.csv"))
})

test_that("load_pe_attributes works on full dataset if present", {
  path <- find_test_data_path()
  skip_if(is.null(path), "CAMELS-PE raw data directory not found")
  old_path <- getOption("rcamelspe.path")
  on.exit(options(rcamelspe.path = old_path), add = TRUE)
  set_camels_pe_path(path)

  # Load specific attribute
  topo <- load_pe_attributes("topographic")
  expect_s3_class(topo, "data.frame")
  expect_true("gauge_id" %in% names(topo))
  expect_true("area" %in% names(topo))

  # Load multiple attributes
  sub_attrs <- load_pe_attributes(c("topographic", "climatic"))
  expect_s3_class(sub_attrs, "data.frame")
  expect_true("area" %in% names(sub_attrs))
  expect_true("p_mean" %in% names(sub_attrs))

  # Load all attributes merged
  all_attrs <- load_pe_attributes("all")
  expect_s3_class(all_attrs, "data.frame")
  expect_true("area" %in% names(all_attrs))
  expect_true("p_mean" %in% names(all_attrs))
  expect_true("soil_dominant_class" %in% names(all_attrs))
  expect_true("forest_perc" %in% names(all_attrs))

  # Test filtering by gauge_ids
  attrs_sub_ids <- load_pe_attributes("topographic", gauge_ids = c("PE_110139", "PE_111151"))
  expect_s3_class(attrs_sub_ids, "data.frame")
  expect_equal(nrow(attrs_sub_ids), 2)
  expect_true(all(attrs_sub_ids$gauge_id %in% c("PE_110139", "PE_111151")))

  # Test category aliases from RCamelsPE
  attrs_hydro <- load_pe_attributes("hydrological")
  expect_s3_class(attrs_hydro, "data.frame")
  expect_true("q_mean" %in% names(attrs_hydro) || "q_mean_obs" %in% names(attrs_hydro))

  attrs_human <- load_pe_attributes("human_intervention")
  expect_s3_class(attrs_human, "data.frame")
})

test_that("load_pe_timeseries works on full dataset if present", {
  path <- find_test_data_path()
  skip_if(is.null(path), "CAMELS-PE raw data directory not found")
  old_path <- getOption("rcamelspe.path")
  on.exit(options(rcamelspe.path = old_path), add = TRUE)
  set_camels_pe_path(path)

  # 1. Load specific stations (reads from by_catchment)
  ts_sub <- load_pe_timeseries(gauge_ids = c("PE_110139", "PE_111151"))
  expect_s3_class(ts_sub, "data.frame")
  expect_true("date" %in% names(ts_sub))
  expect_true("gauge_id" %in% names(ts_sub))
  expect_true("prec" %in% names(ts_sub))
  expect_s3_class(ts_sub$date, "Date")
  expect_true(all(ts_sub$gauge_id %in% c("PE_110139", "PE_111151")))

  # 2. Test variable subset selection
  ts_vars <- load_pe_timeseries(gauge_ids = "PE_110139", variables = c("prec", "flow_obs"))
  expect_s3_class(ts_vars, "data.frame")
  expect_equal(names(ts_vars), c("date", "gauge_id", "prec", "flow_obs"))

  # 3. Test loading without date parsing
  ts_no_parse <- load_pe_timeseries(gauge_ids = "PE_110139", parse_dates = FALSE)
  expect_false(inherits(ts_no_parse$date, "Date"))

  # 4. Load full dataset (using arrow dataset pushdown)
  ts_all_prec <- load_pe_timeseries(variables = "prec")
  expect_s3_class(ts_all_prec, "data.frame")
  expect_equal(names(ts_all_prec), c("date", "gauge_id", "prec"))
})
