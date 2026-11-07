#' Load CAMELS-PE geospatial data
#'
#' Loads geospatial catchment boundary polygons or gauging station point locations
#' from the CAMELS-PE dataset as `sf` spatial objects.
#'
#' @param type Character string. Either `"catchments"` to load catchment boundary polygons
#'   or `"gauges"` to load gauge point locations.
#' @param gauge_ids Character vector. Station identifiers to filter the spatial features.
#'   If `NULL` (default), loads all spatial features.
#' @param path Character string. Path to the CAMELS-PE dataset directory. If `NULL`,
#'   retrieved automatically via [get_camels_pe_path()].
#'
#' @return An `sf` spatial data frame object containing the requested geometries and
#'   station attributes.
#' @export
#'
#' @examples
#' # Load all catchment boundaries
#' catchments <- load_pe_geospatial(type = "catchments")
#' catchments
#'
#' # Load specific gauging stations
#' gauges_sub <- load_pe_geospatial(type = "gauges", gauge_ids = "PE_110139")
#' gauges_sub
load_pe_geospatial <- function(type = c("catchments", "gauges"),
                               gauge_ids = NULL,
                               path = get_camels_pe_path()) {
  type <- match.arg(type)

  if (is.null(path) || !dir.exists(path)) {
    cli::cli_abort(c(
      "CAMELS-PE dataset path not found or invalid.",
      "i" = "Please download the dataset using {.fn download_pe_data} or configure it via {.fn set_camels_pe_path}."
    ))
  }

  file_name <- switch(
    type,
    catchments = "camels_pe_catchments.gpkg",
    gauges = "camels_pe_gauges.gpkg"
  )

  file_path <- file.path(path, "04_geospatial", file_name)

  if (!file.exists(file_path)) {
    cli::cli_abort("Geospatial file not found at: {.path {file_path}}")
  }

  sf_obj <- sf::st_read(file_path, quiet = TRUE)

  if (!is.null(gauge_ids)) {
    sf_obj <- collapse::fsubset(sf_obj, gauge_id %in% gauge_ids)
  }

  return(sf_obj)
}

#' Read CAMELS-PE geospatial data
#'
#' Compatibility alias matching `RCamelsPE::read_geospatial()`.
#'
#' @param type Character string. Either `"gauges"` or `"catchments"`.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'
#' @return An `sf` spatial object.
#' @export
#'
#' @examples
#' catchments <- read_geospatial(type = "catchments")
#' head(catchments)
read_geospatial <- function(type = c("gauges", "catchments"),
                            path = get_camels_pe_path()) {
  type <- match.arg(type)
  load_pe_geospatial(type = type, path = path)
}
