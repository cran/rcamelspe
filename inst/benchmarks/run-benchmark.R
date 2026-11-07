# Run from the repository root. See vignettes/benchmark.Rmd for preparation.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 3L)
path <- normalizePath(args[1], mustWork = TRUE)
upstream <- normalizePath(args[2], mustWork = TRUE)
out <- args[3]
repetitions <- if (length(args) >= 4L) as.integer(args[4]) else 7L
stopifnot(repetitions >= 5L)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(upstream, quiet = TRUE)
pkgload::load_all('.', quiet = TRUE)
old_opts <- options(readr.num_threads = 2L, readr.read_lazy = FALSE)
old_cpu <- if (requireNamespace("arrow", quietly = TRUE)) arrow::cpu_count() else NULL
arrow::set_cpu_count(2L)
on.exit({
  options(old_opts)
  if (!is.null(old_cpu)) arrow::set_cpu_count(old_cpu)
}, add = TRUE)
set.seed(20260905)
stations <- rcamelspe::read_metadata(path = path)
ids <- sort(stations$gauge_id)
stopifnot(length(ids) == 136L)
vars <- c('date', 'gauge_id', 'prec', 'flow_obs')
cases <- list(
  metadata = list(
    RCamelsPE = function() RCamelsPE::read_metadata(path = path),
    rcamelspe = function() rcamelspe::read_metadata(path = path)),
  attributes = list(
    RCamelsPE = function() RCamelsPE::read_attributes(path = path),
    rcamelspe = function() rcamelspe::read_attributes(path = path)),
  one = list(
    RCamelsPE = function() RCamelsPE::read_timeseries(ids[1], path = path),
    rcamelspe = function() rcamelspe::read_timeseries(ids[1], path = path)),
  five = list(
    RCamelsPE = function() RCamelsPE::read_timeseries(ids[1:5], vars = vars, path = path),
    rcamelspe = function() rcamelspe::read_timeseries(ids[1:5], vars = vars, path = path)),
  ten = list(
    RCamelsPE = function() RCamelsPE::read_timeseries(ids[1:10], vars = vars, path = path),
    rcamelspe = function() rcamelspe::read_timeseries(ids[1:10], vars = vars, path = path)),
  twenty = list(
    RCamelsPE = function() RCamelsPE::read_timeseries(ids[1:20], vars = vars, path = path),
    rcamelspe = function() rcamelspe::read_timeseries(ids[1:20], vars = vars, path = path)),
  forty = list(
    RCamelsPE = function() RCamelsPE::read_timeseries(ids[1:40], vars = vars, path = path),
    rcamelspe = function() rcamelspe::read_timeseries(ids[1:40], vars = vars, path = path)),
  global_all = list(
    RCamelsPE = function() RCamelsPE::read_timeseries(global = TRUE, path = path),
    rcamelspe = function() rcamelspe::read_timeseries(global = TRUE, path = path)),
  global_projected = list(
    RCamelsPE = function() RCamelsPE::read_timeseries(global = TRUE, vars = vars, path = path),
    rcamelspe = function() rcamelspe::read_timeseries(global = TRUE, vars = vars, path = path)),
  global_window = list(
    RCamelsPE = function() {
      x <- RCamelsPE::read_timeseries(global = TRUE, vars = vars, path = path)
      dplyr::filter(x, date >= as.Date('2000-01-01'), date <= as.Date('2000-12-31'))
    },
    rcamelspe = function() rcamelspe::read_timeseries(global = TRUE, vars = vars,
      start_date = '2000-01-01', end_date = '2000-12-31', path = path)),
  gauges = list(
    RCamelsPE = function() RCamelsPE::read_geospatial(type = 'gauges', path = path),
    rcamelspe = function() rcamelspe::read_geospatial(type = 'gauges', path = path))
)

# Compare all columns and values after normalizing row/column order and classes.
# No columns are dropped. NA locations and geometry are checked as well.
canonical <- function(x) {
  # Normalize an independent snapshot: never mutate the reader's return value
  # through shared attributes during repeated validation in the same R session.
  x <- unserialize(serialize(x, NULL))
  if (inherits(x, 'sf')) {
    x$geometry_wkt <- sf::st_as_text(sf::st_geometry(x), digits = 15)
    x <- sf::st_drop_geometry(x)
  }
  x <- as.data.frame(x)
  x <- x[, sort(names(x)), drop = FALSE]
  x[] <- lapply(x, function(v) {
    if (inherits(v, 'Date')) return(as.character(v))
    if (is.factor(v)) return(as.character(v))
    if (is.numeric(v)) return(as.double(v))
    v
  })
  keys <- intersect(c('gauge_id', 'date'), names(x))
  x <- x[do.call(order, x[keys]), , drop = FALSE]
  rownames(x) <- NULL
  x
}
raw <- list()
checks <- list()
for (scenario in names(cases)) {
  message('Benchmark: ', scenario)
  f <- cases[[scenario]]
  a <- f$RCamelsPE()
  b <- f$rcamelspe()
  stopifnot(is.data.frame(a), is.data.frame(b))
  dimensions <- dim(b)
  result_mib <- as.numeric(object.size(b)) / 1024^2
  equivalent <- isTRUE(all.equal(canonical(a), canonical(b), tolerance = 1e-8,
                                check.attributes = FALSE))
  if (!equivalent) stop(scenario, ': ', paste(all.equal(canonical(a), canonical(b),
    tolerance = 1e-8, check.attributes = FALSE), collapse = '; '))
  if (inherits(a, 'sf')) stopifnot(sf::st_crs(a) == sf::st_crs(b))
  stopifnot(identical(dim(b), dimensions))
  checks[[scenario]] <- data.frame(scenario, rows = dimensions[1], columns = dimensions[2],
    equivalent, result_mib)
  rm(a, b)
  # Both functions have just been warmed once; GC occurs outside the timed call.
  for (iteration in seq_len(repetitions)) {
    for (package in sample(names(f))) {
      gc()
      calls <- if (scenario %in% c('metadata', 'gauges')) 20L else 1L
      started <- Sys.time()
      for (call in seq_len(calls)) value <- f[[package]]()
      elapsed <- as.numeric(difftime(Sys.time(), started, units = 'secs')) / calls
      raw[[length(raw) + 1L]] <- data.frame(scenario, iteration, package, elapsed)
      rm(value)
    }
  }
}
raw <- do.call(rbind, raw)
checks <- do.call(rbind, checks)
rownames(checks) <- NULL
summary <- do.call(rbind, lapply(split(raw, interaction(raw$scenario, raw$package)), function(x) {
  data.frame(scenario = x$scenario[1], package = x$package[1],
    median_s = median(x$elapsed), q25_s = unname(quantile(x$elapsed, .25)),
    q75_s = unname(quantile(x$elapsed, .75)), min_s = min(x$elapsed), max_s = max(x$elapsed))
}))
write.csv(raw, file.path(out, 'timings.csv'), row.names = FALSE)
write.csv(summary, file.path(out, 'summary.csv'), row.names = FALSE)
write.csv(checks, file.path(out, 'equivalence.csv'), row.names = FALSE)
files <- list.files(path, recursive = TRUE, full.names = TRUE)
files <- files[!dir.exists(files)]
write.csv(data.frame(file = substring(files, nchar(path) + 2L),
  bytes = file.info(files)$size, md5 = unname(tools::md5sum(files))),
  file.path(out, 'data-manifest.csv'), row.names = FALSE)
local_files <- list.files('R', full.names = TRUE)
write.csv(data.frame(file = local_files, md5 = unname(tools::md5sum(local_files))),
  file.path(out, 'code-manifest.csv'), row.names = FALSE)
capture.output({
  cat('UTC: ', format(Sys.time(), tz = 'UTC'), '\n')
  cat('Repetitions: ', repetitions, '\nThreads: Arrow=2, readr=2\n')
  cat('Timer: Sys.time; metadata/gauges: 20 calls per repetition, others: 1.\n')
  cat('CPU: ', Sys.getenv('PROCESSOR_IDENTIFIER'), '\n')
  cat('Dataset: local CAMELS-PE; version not independently verified; see hashes.\n')
  print(Sys.info()[c('sysname', 'release', 'machine')])
  cat('Logical CPUs: ', parallel::detectCores(), '\n')
  print(sessionInfo())
}, file = file.path(out, 'session.txt'))

options(old_opts)
if (!is.null(old_cpu)) arrow::set_cpu_count(old_cpu)
