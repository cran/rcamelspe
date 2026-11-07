#' Load CAMELS-PE daily timeseries data
#'
#' Efficiently loads daily hydroclimatic timeseries for Peruvian catchments.
#' Features an optimized dual-pathway execution engine using 'arrow' and 'collapse':
#' \itemize{
#'   \item For selective requests, reads individual catchment files with column projection.
#'   \item For global or multi-catchment requests, uses `arrow::open_dataset()` with
#'         predicate pushdown (filtering stations and date ranges directly at scan time)
#'         to minimize memory footprint and maximize throughput.
#' }
#'
#' @param gauge_ids Character vector. Station/catchment identifiers (e.g. `"PE_110139"`).
#'   If `NULL` (default), loads timeseries for all available catchments.
#' @param variables Character vector. Names of hydroclimatic variables to select.
#'   If `NULL` (default), all variables are loaded. Allowed variables:
#'   `"prec"`, `"prec_var"`, `"flow_obs"`, `"flow_sim"`, `"pet"`, `"tmin"`,
#'   `"tmean"`, `"tmax"`, `"srad"`, `"vprp"`.
#' @param start_date Optional character string or Date object (`"YYYY-MM-DD"`).
#'   Start date for temporal filtering.
#' @param end_date Optional character string or Date object (`"YYYY-MM-DD"`).
#'   End date for temporal filtering.
#' @param path Character string. Path to the CAMELS-PE dataset directory. If `NULL`,
#'   retrieved automatically via [get_camels_pe_path()].
#' @param parse_dates Logical. Should the `date` column be parsed into R Date objects?
#'   Default is `TRUE`.
#' @param use_arrow Logical. Should the 'arrow' package be used for reading?
#'   Default is `TRUE` (recommended for high performance).
#' @param global Logical. If `TRUE`, read the master CSV even for a small selection.
#' @details The common calendar is 1981-2025; unavailable observations remain `NA`.
#'   Streamflow is expressed in mm/day, precipitation variance (`prec_var`) in
#'   mm^2/day^2, solar radiation in MJ/m^2/day, and vapor pressure in hPa.
#'   Identifier columns `date` and `gauge_id` may also be requested in `variables`.
#'   Automatic routing compares selected CSV bytes plus a 4 MiB per-file opening
#'   cost estimate against the master CSV size. This heuristic avoids scanning
#'   the national file for small subsets while retaining it for large requests.
#'   Missing individual files fall back to the master when it is available.
#'   `global = TRUE` always forces the master. No data cache is created.
#'
#' @return A `data.frame` containing daily hydroclimatic timeseries data with at least
#'   `date` and `gauge_id` columns, plus the requested hydrometeorological variables.
#' @export
#'
#' @examples
#' # Load timeseries for a specific station
#' ts_station <- load_pe_timeseries(gauge_ids = "PE_110139")
#' head(ts_station)
#'
#' # Load only precipitation and observed streamflow with date range filter
#' ts_sub <- load_pe_timeseries(
#'   gauge_ids = "PE_110139",
#'   variables = c("prec", "flow_obs"),
#'   start_date = "2000-01-01",
#'   end_date = "2000-03-31"
#' )
#' head(ts_sub)
load_pe_timeseries <- function(gauge_ids = NULL,
                               variables = NULL,
                               start_date = NULL,
                               end_date = NULL,
                               path = get_camels_pe_path(),
                               parse_dates = TRUE,
                               use_arrow = TRUE,
                               global = FALSE) {
  if (is.null(path) || !dir.exists(path)) {
    cli::cli_abort(c(
      "CAMELS-PE dataset path not found or invalid.",
      "i" = "Please download the dataset using {.fn download_pe_data} or configure it via {.fn set_camels_pe_path}."
    ))
  }

  valid_vars <- c(
    "prec", "prec_var", "flow_obs", "flow_sim", "pet",
    "tmin", "tmean", "tmax", "srad", "vprp"
  )

  if (!is.null(variables)) {
    if (!is.character(variables) || anyNA(variables) ||
        any(!variables %in% c("date", "gauge_id", valid_vars))) {
      cli::cli_abort("Unknown timeseries variable. Consult read_dictionary(category = 'timeseries').")
    }
    variables <- unique(setdiff(variables, c("date", "gauge_id")))
  }

  # Columns to keep (always keep date and gauge_id)
  keep_cols <- c("date", "gauge_id")
  if (!is.null(variables)) {
    keep_cols <- unique(c(keep_cols, variables))
  }

  # Parse date filter bounds if supplied
  parse_bound <- function(x) {
    if (is.null(x)) return(NULL)
    value <- tryCatch(as.Date(x), error = function(e) NA)
    if (length(value) != 1L || is.na(value)) {
      cli::cli_abort("Date bounds must be single valid dates (YYYY-MM-DD).")
    }
    value
  }
  start_date <- parse_bound(start_date)
  end_date <- parse_bound(end_date)
  if (!is.null(start_date) && !is.null(end_date) && start_date > end_date) {
    cli::cli_abort("start_date must not be after end_date.")
  }

  # Estimate scan volume plus CSV opening/inference overhead instead of a fixed
  # station-count threshold. Only stat the requested files; do not scan metadata.
  catchment_dir <- file.path(path, "03_timeseries", "by_catchment")
  main_file <- file.path(path, "03_timeseries", "timeseries.csv")
  read_by_catchment <- FALSE
  if (!global && !is.null(gauge_ids) && dir.exists(catchment_dir)) {
    gauge_ids <- unique(gauge_ids)
    selected_files <- file.path(catchment_dir, paste0(gauge_ids, ".csv"))
    sizes <- file.info(selected_files)$size
    master_size <- file.info(main_file)$size
    read_by_catchment <- is.na(master_size) ||
      (all(!is.na(sizes)) && sum(sizes + 4 * 1024^2) < master_size)
  }

  if (read_by_catchment) {
    loaded_dfs <- list()
    cols_to_read <- setdiff(keep_cols, "gauge_id")

    for (gid in gauge_ids) {
      file_path <- file.path(catchment_dir, paste0(gid, ".csv"))
      if (!file.exists(file_path)) {
        cli::cli_warn("Timeseries file not found for station {.val {gid}} at {.path {file_path}}")
        next
      }

      if (use_arrow) {
        if (!is.null(variables)) {
          df <- arrow::read_csv_arrow(file_path, col_select = tidyselect::any_of(cols_to_read))
        } else {
          df <- arrow::read_csv_arrow(file_path)
        }
        df <- as.data.frame(df)
      } else {
        df <- utils::read.csv(file_path, stringsAsFactors = FALSE)
        if (!is.null(variables)) {
          df <- df[, intersect(names(df), cols_to_read), drop = FALSE]
        }
      }

      df$gauge_id <- gid

      # Temporal filtering
      if (!is.null(start_date) || !is.null(end_date)) {
        if (!inherits(df$date, "Date")) {
          df$date <- as.Date(df$date)
        }
        if (!is.null(start_date) && !is.null(end_date)) {
          df <- collapse::fsubset(df, date >= start_date & date <= end_date)
        } else if (!is.null(start_date)) {
          df <- collapse::fsubset(df, date >= start_date)
        } else if (!is.null(end_date)) {
          df <- collapse::fsubset(df, date <= end_date)
        }
      }

      # Reorder columns so date, gauge_id are first
      col_names <- names(df)
      col_order <- c("date", "gauge_id", setdiff(col_names, c("date", "gauge_id")))
      df <- df[, col_order, drop = FALSE]

      loaded_dfs[[gid]] <- df
    }

    if (length(loaded_dfs) == 0) {
      return(data.frame())
    }

    # Use a type-stable binder that owns its result attributes. Repeated larger
    # batches with collapse::rowbind() can leave invalid shared attributes.
    res_df <- as.data.frame(dplyr::bind_rows(unname(loaded_dfs)))

  } else {
    # Read the main timeseries.csv file using Arrow dataset predicate pushdown
    main_file <- file.path(path, "03_timeseries", "timeseries.csv")
    if (!file.exists(main_file)) {
      cli::cli_abort("Main timeseries file not found at: {.path {main_file}}")
    }

    if (use_arrow) {
      # Use arrow::open_dataset for predicate pushdown and column projection
      ds <- arrow::open_dataset(main_file, format = "csv")

      # Build query
      query <- ds

      if (!is.null(gauge_ids)) {
        target_ids <- gauge_ids
        query <- query |> dplyr::filter(.data$gauge_id %in% target_ids)
      }

      if (!is.null(start_date)) {
        s_date <- start_date
        query <- query |> dplyr::filter(.data$date >= s_date)
      }

      if (!is.null(end_date)) {
        e_date <- end_date
        query <- query |> dplyr::filter(.data$date <= e_date)
      }

      if (!is.null(variables)) {
        cols_present <- intersect(keep_cols, names(ds))
        missing_vars <- setdiff(keep_cols, names(ds))
        if (length(missing_vars) > 0) {
          cli::cli_warn(c(
            "Some columns were not found in the timeseries dataset:",
            "x" = "Missing column{?s}: {.field {missing_vars}}"
          ))
        }
        query <- query |> dplyr::select(tidyselect::all_of(cols_present))
      }

      res_df <- as.data.frame(dplyr::collect(query))

    } else {
      df <- utils::read.csv(main_file, stringsAsFactors = FALSE)
      if (!is.null(gauge_ids)) {
        df <- collapse::fsubset(df, gauge_id %in% gauge_ids)
      }
      if (!is.null(variables)) {
        missing_vars <- setdiff(keep_cols, names(df))
        if (length(missing_vars) > 0) {
          cli::cli_warn(c(
            "Some columns were not found in the main timeseries file:",
            "x" = "Missing column{?s}: {.field {missing_vars}}"
          ))
        }
        df <- df[, intersect(names(df), keep_cols), drop = FALSE]
      }
      if (!is.null(start_date) || !is.null(end_date)) {
        if (!inherits(df$date, "Date")) {
          df$date <- as.Date(df$date)
        }
        if (!is.null(start_date) && !is.null(end_date)) {
          df <- collapse::fsubset(df, date >= start_date & date <= end_date)
        } else if (!is.null(start_date)) {
          df <- collapse::fsubset(df, date >= start_date)
        } else if (!is.null(end_date)) {
          df <- collapse::fsubset(df, date <= end_date)
        }
      }
      res_df <- df
    }
  }

  # Ensure hydroclimate columns are numeric even if all values are NA
  for (v in intersect(valid_vars, names(res_df))) {
    if (!is.numeric(res_df[[v]])) {
      res_df[[v]] <- as.numeric(res_df[[v]])
    }
  }

  # Ensure date is Date class if requested, or character if parse_dates = FALSE
  if ("date" %in% names(res_df)) {
    if (parse_dates) {
      if (!inherits(res_df$date, "Date")) {
        res_df$date <- as.Date(res_df$date)
      }
    } else {
      if (inherits(res_df$date, "Date") || inherits(res_df$date, "POSIXt")) {
        res_df$date <- as.character(res_df$date)
      }
    }
  }

  return(res_df)
}

#' Read CAMELS-PE time series
#'
#' Compatibility alias matching the `RCamelsPE::read_timeseries()` interface,
#' powered by the high-performance 'arrow' dataset and 'collapse' query engine.
#'
#' @param gauge_id Character vector or `NULL`. Gauge identifiers to read.
#' @param global Logical value. If `TRUE`, forces reading from the global file.
#'   If `FALSE`, reads individual catchment files when available.
#' @param vars Character vector or `NULL`. Optional variable names to retain.
#' @param start_date Optional character string or Date (`"YYYY-MM-DD"`).
#' @param end_date Optional character string or Date (`"YYYY-MM-DD"`).
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'
#' @return A `data.frame` containing CAMELS-PE time series data.
#' @export
#'
#' @examples
#' # Read timeseries using RCamelsPE compatible syntax
#' ts <- read_timeseries(gauge_id = "PE_110139", vars = c("date", "prec", "flow_obs"))
#' head(ts)
read_timeseries <- function(gauge_id = NULL,
                            global = FALSE,
                            vars = NULL,
                            start_date = NULL,
                            end_date = NULL,
                            path = get_camels_pe_path()) {
  load_pe_timeseries(
    gauge_ids = gauge_id,
    variables = vars,
    start_date = start_date,
    end_date = end_date,
    path = path,
    global = global
  )
}
