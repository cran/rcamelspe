#' Load CAMELS-PE catchment attributes
#'
#' Loads one or more catchment attribute files and merges them by `gauge_id`.
#' High-performance implementation using 'arrow' and 'collapse'.
#' @details CAMELS-PE v1.0.1 documents 79 attributes in seven thematic tables.
#'   Hydrological signatures are derived from simulated streamflow, not observations.
#'
#' @param attributes Character vector. The attribute groups to load. Can be any combination of
#'   `"topographic"`, `"climatic"`, `"geologic"`, `"soil"`, `"landcover"`, `"intervention"`,
#'   and `"signatures"`, or `"all"` (default) to load and merge all attribute tables.
#'   Aliases `"hydrological"` (for `"signatures"`) and `"human_intervention"` (for `"intervention"`)
#'   are also supported.
#' @param gauge_ids Character vector. Optional gauge identifiers to filter the returned attributes.
#'   If `NULL`, attributes for all catchments are returned.
#' @param variables Character vector or `NULL`. Optional variable names to retain in the returned
#'   table. If `NULL`, all variables in the selected attribute groups are returned.
#' @param path Character string. Path to the CAMELS-PE dataset directory. If `NULL`,
#'   retrieved automatically via [get_camels_pe_path()].
#'
#' @return A `data.frame` containing the merged catchment attributes with `gauge_id`
#'   as primary identifier.
#' @export
#'
#' @examples
#' # Load all attributes merged
#' attrs_all <- load_pe_attributes()
#' head(attrs_all)
#'
#' # Load only topographic and climatic attributes
#' attrs_sub <- load_pe_attributes(c("topographic", "climatic"))
#' head(attrs_sub)
#'
#' # Load attributes for specific stations
#' attrs_sel <- load_pe_attributes(gauge_ids = c("PE_110139", "PE_111151"))
#' attrs_sel
load_pe_attributes <- function(attributes = "all",
                               gauge_ids = NULL,
                               variables = NULL,
                               path = get_camels_pe_path()) {
  if (is.null(path) || !dir.exists(path)) {
    cli::cli_abort(c(
      "CAMELS-PE dataset path not found or invalid.",
      "i" = "Please download the dataset using {.fn download_pe_data} or configure it via {.fn set_camels_pe_path}."
    ))
  }

  # Map category aliases from RCamelsPE
  attributes[attributes == "hydrological"] <- "signatures"
  attributes[attributes == "human_intervention"] <- "intervention"

  valid_attrs <- c(
    "topographic", "climatic", "geologic", "soil",
    "landcover", "intervention", "signatures"
  )

  if (length(attributes) == 1 && attributes == "all") {
    selected_attrs <- valid_attrs
  } else {
    selected_attrs <- match.arg(attributes, choices = valid_attrs, several.ok = TRUE)
  }

  # File mapping
  attr_files <- list(
    topographic = "topographic_attributes.csv",
    climatic = "climatic_indices.csv",
    geologic = "geologic_attributes.csv",
    soil = "soil_attributes.csv",
    landcover = "landcover_attributes.csv",
    intervention = "human_intervention_attributes.csv",
    signatures = "hydrological_signatures.csv"
  )

  loaded_dfs <- list()

  for (attr in selected_attrs) {
    file_name <- attr_files[[attr]]
    file_path <- file.path(path, "02_attributes", file_name)

    if (!file.exists(file_path)) {
      cli::cli_warn("Attribute file not found: {.path {file_path}}")
      next
    }

    # Load efficiently using arrow
    df <- arrow::read_csv_arrow(file_path)
    df <- as.data.frame(df)

    # Filter by gauge_ids early to minimize join overhead
    if (!is.null(gauge_ids) && nrow(df) > 0) {
      df <- collapse::fsubset(df, gauge_id %in% gauge_ids)
    }

    loaded_dfs[[attr]] <- df
  }

  if (length(loaded_dfs) == 0) {
    return(data.frame())
  }

  # Merge all loaded data frames using collapse::join (fast full outer join)
  merged_df <- loaded_dfs[[1]]

  if (length(loaded_dfs) > 1) {
    for (i in 2:length(loaded_dfs)) {
      merged_df <- collapse::join(
        merged_df,
        loaded_dfs[[i]],
        on = "gauge_id",
        how = "full",
        verbose = FALSE
      )
    }
  }

  # Select specific variables if requested (always keep gauge_id)
  if (!is.null(variables)) {
    target_cols <- unique(c("gauge_id", variables))
    keep_cols <- intersect(target_cols, names(merged_df))
    merged_df <- collapse::fselect(merged_df, keep_cols)
  }

  return(merged_df)
}

#' Read CAMELS-PE catchment attributes
#'
#' Compatibility alias matching the `RCamelsPE::read_attributes()` interface,
#' powered by the high-performance 'arrow' and 'collapse' backend of `rcamelspe`.
#'
#' @param type Character string. Attribute group to read. One of
#'   `"topographic"`, `"climatic"`, `"hydrological"`, `"landcover"`,
#'   `"geologic"`, `"soil"`, `"human_intervention"`, or `"all"`. Default is `"all"`.
#' @param gauge_id Character vector or `NULL`. Optional gauge identifiers.
#' @param vars Character vector or `NULL`. Optional variable names to retain.
#' @param path Character string. Optional path to the CAMELS-PE root directory.
#'
#' @return A `data.frame` with CAMELS-PE catchment attributes.
#' @export
#'
#' @examples
#' # Read all attributes using RCamelsPE compatible syntax
#' attrs <- read_attributes(type = "topographic")
#' head(attrs)
read_attributes <- function(type = "all",
                            gauge_id = NULL,
                            vars = NULL,
                            path = get_camels_pe_path()) {
  load_pe_attributes(
    attributes = type,
    gauge_ids = gauge_id,
    variables = vars,
    path = path
  )
}
