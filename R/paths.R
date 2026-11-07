#' Set the path to the CAMELS-PE dataset
#'
#' Sets the path to the directory containing the CAMELS-PE dataset.
#' The path is stored in the package session options.
#'
#' @param path Character string. Path to the CAMELS-PE dataset directory.
#'
#' @return Invisible `NULL`. Called for its side effect of setting the dataset path.
#' @export
#'
#' @examples
#' old_path <- getOption("rcamelspe.path")
#' sample_path <- system.file("extdata", "sample_camels_pe", package = "rcamelspe")
#' set_camels_pe_path(sample_path)
#' # Reset options
#' options(rcamelspe.path = old_path)
set_camels_pe_path <- function(path) {
  if (!is.null(path)) {
    path <- normalizePath(path, mustWork = FALSE)
    if (!dir.exists(path)) {
      cli::cli_warn("The provided CAMELS-PE path does not exist: {.path {path}}")
    } else {
      required_dirs <- c(
        "01_metadata",
        "02_attributes",
        "03_timeseries",
        "04_geospatial"
      )
      missing_dirs <- required_dirs[!dir.exists(file.path(path, required_dirs))]
      if (length(missing_dirs) > 0) {
        cli::cli_warn(
          "The following required CAMELS-PE folder{?s} {?is/are} missing: {.file {missing_dirs}}"
        )
      }
    }
  }
  options(rcamelspe.path = path)
  invisible(NULL)
}

#' @rdname set_camels_pe_path
#' @export
set_camels_path <- set_camels_pe_path

#' Get the path to the CAMELS-PE dataset
#'
#' Retrieves the path to the CAMELS-PE dataset directory. It looks up paths in
#' the following order of precedence:
#' 1. The package option `rcamelspe.path`.
#' 2. The environment variable `CAMELS_PE_PATH`.
#' 3. The persistent user data directory (`tools::R_user_dir("rcamelspe", "data")`).
#' 4. Default search paths in the current working directory (`raw-data/CAMELS-PE`,
#'    `data-raw/CAMELS-PE`, etc.).
#' 5. The bundled minimal sample dataset in `inst/extdata/sample_camels_pe`.
#'
#' @return Character string containing the directory path where the dataset is located,
#'   or `NULL` if no valid path is found.
#' @export
#'
#' @examples
#' camels_path <- get_camels_pe_path()
#' is.character(camels_path)
get_camels_pe_path <- function() {
  # 1. Option
  path <- getOption("rcamelspe.path")
  if (!is.null(path) && dir.exists(path)) {
    return(path)
  }

  # 2. Env var
  env_path <- Sys.getenv("CAMELS_PE_PATH")
  if (nzchar(env_path) && dir.exists(env_path)) {
    return(normalizePath(env_path))
  }

  # 3. Persistent user data directory
  user_dir <- tryCatch(
    file.path(tools::R_user_dir("rcamelspe", which = "data"), "CAMELS-PE"),
    error = function(e) NULL
  )
  if (!is.null(user_dir) && dir.exists(user_dir)) {
    return(normalizePath(user_dir))
  }

  # 4. Default workspace checks
  defaults <- c(
    "raw-data/CAMELS-PE",
    "data-raw/CAMELS-PE",
    "raw-data/camels-pe",
    "data-raw/camels-pe",
    "CAMELS-PE"
  )

  for (d in defaults) {
    if (dir.exists(d)) {
      return(normalizePath(d))
    }
  }

  # 5. Bundled sample dataset fallback
  sample_dir <- system.file("extdata", "sample_camels_pe", package = "rcamelspe")
  if (nzchar(sample_dir) && dir.exists(sample_dir)) {
    return(normalizePath(sample_dir))
  }

  NULL
}

#' @rdname get_camels_pe_path
#' @export
get_camels_path <- get_camels_pe_path
