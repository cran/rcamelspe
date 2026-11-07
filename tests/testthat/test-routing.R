test_that("ten catchments work without a national CSV and preserve values", {
  path <- tempfile()
  folder <- file.path(path, '03_timeseries', 'by_catchment')
  dir.create(folder, recursive = TRUE)
  on.exit(unlink(path, recursive = TRUE))
  ids <- paste0('PE_', seq_len(10))
  for (id in ids) {
    utils::write.csv(data.frame(date = c('2000-01-01', '2000-01-02'),
      prec = c(1, NA), flow_obs = c(NA_real_, NA_real_)),
      file.path(folder, paste0(id, '.csv')), row.names = FALSE)
  }
  for (backend in c(TRUE, FALSE)) {
    x <- load_pe_timeseries(c(ids, ids[1]), variables = c('prec', 'flow_obs'),
      path = path, use_arrow = backend, start_date = '2000-01-02')
    expect_equal(nrow(x), 10)
    expect_setequal(x$gauge_id, ids)
    expect_true(all(is.na(x$prec)))
    expect_type(x$flow_obs, 'double')
    expect_s3_class(x$date, 'Date')
    expect_identical(unserialize(serialize(x, NULL)), x)
  }
  expect_error(load_pe_timeseries(ids, path = path, global = TRUE), 'Main timeseries')
})

test_that("missing individual files fall back to the complete national selection", {
  path <- tempfile()
  dir.create(file.path(path, '03_timeseries', 'by_catchment'), recursive = TRUE)
  on.exit(unlink(path, recursive = TRUE))
  utils::write.csv(data.frame(date = '2000-01-01', gauge_id = c('PE_1', 'PE_2'),
    prec = c(2, 3)), file.path(path, '03_timeseries', 'timeseries.csv'), row.names = FALSE)
  for (backend in c(TRUE, FALSE)) {
    x <- load_pe_timeseries('PE_2', path = path, use_arrow = backend)
    expect_equal(x$gauge_id, 'PE_2')
    expect_equal(x$prec, 3)
  }
})
