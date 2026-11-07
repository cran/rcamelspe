#' Download the CAMELS-PE dataset from Zenodo
#'
#' Downloads the CAMELS-PE dataset zip file from the Zenodo repository
#' (\doi{10.5281/zenodo.21195425}) and extracts it in the destination folder.
#'
#' @param dest_dir Character string. Destination directory where the dataset should be
#'   saved.
#' @param unzip Logical. Should the downloaded file be unzipped? Default is `TRUE`.
#' @param overwrite Logical. If destination files already exist, should they be overwritten?
#'   Default is `FALSE`.
#' @param quiet Logical. Should download progress be suppressed? Default is `FALSE`.
#' @param version Character string. Dataset release: `"1.0.1"` (default) or `"1.0"`.
#' @param set_path Logical. Configure the session path after extraction only.
#'
#' @return Character string indicating the directory path where the dataset is located
#'   or extracted; the ZIP path when `unzip = FALSE`. Use a separate destination
#'   for each release, or `overwrite = TRUE` to replace an existing extraction.
#' @export
#'
#' @examples
#' \donttest{
#' # Download to a temporary folder
#' download_pe_data(dest_dir = tempdir())
#' }
download_pe_data <- function(dest_dir,
                             unzip = TRUE,
                             overwrite = FALSE,
                             quiet = FALSE,
                             version = "1.0.1",
                             set_path = TRUE) {
  records <- c("1.0" = "20058779", "1.0.1" = "21195425")
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
      !version %in% names(records)) {
    cli::cli_abort("Unsupported CAMELS-PE version. Available versions: 1.0, 1.0.1.")
  }
  if (missing(dest_dir) || !is.character(dest_dir) || length(dest_dir) != 1L ||
      is.na(dest_dir) || !nzchar(dest_dir)) {
    cli::cli_abort("Please provide a valid destination directory through {.arg dest_dir}.")
  }
  archive <- paste0("CAMELS-PE_v", version, ".zip")
  url <- paste0("https://zenodo.org/api/records/", records[[version]],
                "/files/", archive, "/content")

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  zip_file <- file.path(dest_dir, archive)

  # Download the zip file
  if (!file.exists(zip_file) || overwrite) {
    if (!quiet) {
      cli::cli_inform("Downloading CAMELS-PE dataset from Zenodo...")
    }
    # Set timeout to a high number to avoid timeouts with large files
    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout), add = TRUE)
    options(timeout = max(3600, old_timeout))

    partial <- tempfile("camels-download-", tmpdir = dest_dir)
    on.exit(unlink(partial), add = TRUE)
    status <- utils::download.file(
      url = url,
      destfile = partial,
      mode = "wb",
      quiet = quiet
    )
    if (status != 0L || !file.exists(partial) || file.info(partial)$size == 0) {
      cli::cli_abort("CAMELS-PE download failed.")
    }
    utils::unzip(partial, list = TRUE)
    if (!file.copy(partial, zip_file, overwrite = TRUE)) {
      cli::cli_abort("Could not save the downloaded archive.")
    }
  } else {
    if (!quiet) {
      cli::cli_inform("Zip file already exists. Use {.code overwrite = TRUE} to download again.")
    }
  }

  # Unzip the file
  if (unzip) {
    expected_dir <- file.path(dest_dir, "CAMELS-PE")
    if (!dir.exists(expected_dir) || overwrite) {
      if (!quiet) {
        cli::cli_inform("Unzipping dataset to {.path {dest_dir}}...")
      }
      utils::unzip(zipfile = zip_file, exdir = dest_dir, overwrite = overwrite)
    } else {
      if (!quiet) {
        cli::cli_inform("Extracted directory already exists. Use {.code overwrite = TRUE} to extract again.")
      }
    }
    pe_dir <- expected_dir
    required <- c("01_metadata", "02_attributes", "03_timeseries", "04_geospatial")
    if (!all(dir.exists(file.path(pe_dir, required)))) {
      cli::cli_abort("Extracted CAMELS-PE dataset is incomplete. Retry with overwrite = TRUE.")
    }
    if (set_path) set_camels_pe_path(pe_dir)
    return(pe_dir)
  }

  return(zip_file)
}

#' Download CAMELS-PE dataset from Zenodo
#'
#' Compatibility alias matching `RCamelsPE::download_camels_pe()`.
#'
#' @param path Character string. Destination directory where dataset will be stored.
#' @param version Character string. Dataset release: `"1.0.1"` (default) or `"1.0"`.
#' @param unzip Logical. Should the downloaded file be extracted? Default is `TRUE`.
#' @param overwrite Logical. If `TRUE`, existing files will be overwritten.
#' @param set_path Logical. If `TRUE`, sets dataset path in session options.
#'
#' @return Character string indicating the path to the downloaded archive or directory.
#' @export
#'
#' @examples
#' \donttest{
#' download_camels_pe(path = tempdir(), unzip = FALSE)
#' }
download_camels_pe <- function(path,
                               version = "1.0.1",
                               unzip = TRUE,
                               overwrite = FALSE,
                               set_path = TRUE) {
  if (missing(path) || is.null(path) || length(path) != 1L || is.na(path)) {
    cli::cli_abort("Please provide a valid destination directory through {.arg path}.")
  }

  res <- download_pe_data(
    dest_dir = path,
    unzip = unzip,
    overwrite = overwrite,
    quiet = FALSE,
    version = version,
    set_path = set_path
  )

  invisible(res)
}
