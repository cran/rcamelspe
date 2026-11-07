#' Load CAMELS-PE metadata
#'
#' Loads gauging station metadata or the data dictionary from the CAMELS-PE dataset.
#'
#' @param type Character string. Either `"stations"` to load gauging station metadata
#'   or `"dictionary"` to load the data dictionary.
#' @param category Character vector or `NULL`. Optional category filter for the dictionary (e.g. `"climatic"`).
#' @param variable Character vector or `NULL`. Optional variable filter for the dictionary (e.g. `"flow_obs"`).
#' @param file Character vector or `NULL`. Optional file filter for the dictionary (e.g. `"stations.csv"`).
#' @param path Character string. Path to the CAMELS-PE dataset directory. If `NULL`,
#'   retrieved automatically via [get_camels_pe_path()].
#'
#' @return A `data.frame` containing the requested metadata:
#'   \itemize{
#'     \item When `type = "stations"`: station metadata including `gauge_id`, `gauge_name`,
#'       geographic coordinates, elevation, and hydrologic regions.
#'     \item When `type = "dictionary"`: dictionary entries with columns `folder`, `file`,
#'       `category`, `variable`, `description`, `unit`, and `source`.
#'   }
#' @export
#'
#' @examples
#' # Load stations metadata
#' stations <- load_pe_metadata(type = "stations")
#' head(stations)
#'
#' # Load data dictionary
#' data_dict <- load_pe_metadata(type = "dictionary")
#' head(data_dict)
#'
#' # Load data dictionary filtered by category
#' clim_dict <- load_pe_metadata(type = "dictionary", category = "climatic")
load_pe_metadata <- function(type = c("stations", "dictionary"),
                             category = NULL,
                             variable = NULL,
                             file = NULL,
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
    stations = "stations.csv",
    dictionary = "data_dictionary.csv"
  )

  file_path <- file.path(path, "01_metadata", file_name)

  if (!file.exists(file_path)) {
    cli::cli_abort("Metadata file not found at: {.path {file_path}}")
  }

  # Load efficiently using arrow
  df <- arrow::read_csv_arrow(file_path)
  df <- as.data.frame(df)

  if (type == "dictionary") {
    required_cols <- c(
      "folder",
      "file",
      "category",
      "variable",
      "description",
      "unit",
      "source"
    )

    missing_cols <- setdiff(required_cols, names(df))

    if (length(missing_cols) > 0) {
      cli::cli_abort(c(
        "The dictionary file is missing required column{?s}.",
        "x" = "Missing column{?s}: {.field {missing_cols}}"
      ))
    }

    if (!is.null(category)) {
      cat_filter <- category
      df <- collapse::fsubset(df, category %in% cat_filter)
    }

    if (!is.null(variable)) {
      var_filter <- variable
      df <- collapse::fsubset(df, variable %in% var_filter)
    }

    if (!is.null(file)) {
      file_filter <- file
      df <- collapse::fsubset(df, file %in% file_filter)
    }
  }

  return(df)
}

#' Read CAMELS-PE station metadata
#'
#' Convenience alias for `load_pe_metadata(type = "stations")`, compatible
#' with the `RCamelsPE` interface.
#'
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, retrieved automatically via [get_camels_pe_path()].
#'
#' @return A `data.frame` containing gauging station metadata.
#' @export
#'
#' @examples
#' stations <- read_metadata()
#' head(stations)
read_metadata <- function(path = get_camels_pe_path()) {
  load_pe_metadata(type = "stations", path = path)
}

#' Read CAMELS-PE data dictionary
#'
#' Reads the CAMELS-PE data dictionary with optional filters by category,
#' variable, or source file. Compatible with the `RCamelsPE` interface.
#'
#' @param category Character vector or `NULL`. Optional category filter.
#' @param variable Character vector or `NULL`. Optional variable filter.
#' @param file Character vector or `NULL`. Optional file filter.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'   If not provided, retrieved automatically via [get_camels_pe_path()].
#'
#' @return A `data.frame` containing the CAMELS-PE data dictionary entries.
#' @export
#'
#' @examples
#' # Read full dictionary
#' dict <- read_dictionary()
#' head(dict)
#'
#' # Read dictionary for topographic category
#' topo_dict <- read_dictionary(category = "topographic")
#' head(topo_dict)
read_dictionary <- function(category = NULL,
                            variable = NULL,
                            file = NULL,
                            path = get_camels_pe_path()) {
  load_pe_metadata(
    type = "dictionary",
    category = category,
    variable = variable,
    file = file,
    path = path
  )
}
