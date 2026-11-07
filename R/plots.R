#' Plot CAMELS-PE Time Series
#'
#' Creates a time series plot for one CAMELS-PE variable, such as precipitation,
#' observed streamflow, simulated streamflow, or temperature. The function
#' returns a `ggplot` object, styled with the project color palette.
#'
#' @param data A data frame containing CAMELS-PE time series.
#' @param variable Character string. Name of the variable to plot. Default is `"flow_obs"`.
#' @param gauge_id Optional character vector. Gauge IDs used to filter the data
#'   before plotting. If `NULL`, all available gauges in `data` are plotted.
#' @param date_col Character string. Name of the date column. Default is `"date"`.
#' @param facet Logical value. If `TRUE` (default), creates one panel per gauge ID.
#' @param scales Character string. Scales passed to \code{ggplot2::facet_wrap()}.
#'   One of `"fixed"`, `"free"`, `"free_x"`, or `"free_y"` (default).
#' @param line_color Character string. Line color for the time series. Default is `"#2F4156"` (Navy).
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}.
#'
#' @return A `ggplot` visualization object representing the time series.
#' @export
#'
#' @examples
#' ts <- load_pe_timeseries(
#'   gauge_ids = "PE_110139",
#'   variables = c("prec", "flow_sim")
#' )
#' plot_pe_timeseries(ts, variable = "prec")
plot_pe_timeseries <- function(data,
                               variable = "flow_obs",
                               gauge_id = NULL,
                               date_col = "date",
                               facet = TRUE,
                               scales = "free_y",
                               line_color = "#2F4156",
                               ...) {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (!is.character(variable) || length(variable) != 1L || is.na(variable)) {
    cli::cli_abort("{.arg variable} must be a single character string.")
  }

  if (!is.character(date_col) || length(date_col) != 1L || is.na(date_col)) {
    cli::cli_abort("{.arg date_col} must be a single character string.")
  }

  if (!date_col %in% names(data)) {
    cli::cli_abort("Date column {.field {date_col}} not found in {.arg data}.")
  }

  if (!"gauge_id" %in% names(data)) {
    cli::cli_abort("Column {.field gauge_id} not found in {.arg data}.")
  }

  if (!variable %in% names(data)) {
    cli::cli_abort("Variable {.field {variable}} not found in {.arg data}.")
  }

  if (!is.null(gauge_id) && (!is.character(gauge_id) || anyNA(gauge_id))) {
    cli::cli_abort("{.arg gauge_id} must be a character vector without NA values or NULL.")
  }

  if (!is.logical(facet) || length(facet) != 1L || is.na(facet)) {
    cli::cli_abort("{.arg facet} must be TRUE or FALSE.")
  }

  allowed_scales <- c("fixed", "free", "free_x", "free_y")
  if (!is.character(scales) || length(scales) != 1L || is.na(scales) || !scales %in% allowed_scales) {
    cli::cli_abort("{.arg scales} must be one of: {.val {allowed_scales}}.")
  }

  data <- as.data.frame(data)

  if (!is.null(gauge_id)) {
    data <- data[data[["gauge_id"]] %in% gauge_id, , drop = FALSE]
  }

  if (nrow(data) == 0L) {
    cli::cli_abort("No data available for the selected {.arg gauge_id}.")
  }

  data[[date_col]] <- as.Date(data[[date_col]])

  if (anyNA(data[[date_col]])) {
    cli::cli_abort("Date column contains values that cannot be converted to Date.")
  }

  if (!is.numeric(data[[variable]])) {
    cli::cli_abort("{.arg variable} must refer to a numeric column.")
  }

  data[["gauge_id"]] <- as.character(data[["gauge_id"]])
  data <- data[order(data[["gauge_id"]], data[[date_col]]), , drop = FALSE]

  if (facet) {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[date_col]],
        y = .data[[variable]]
      )
    ) +
      ggplot2::geom_line(color = line_color, ...) +
      ggplot2::facet_wrap(ggplot2::vars(.data$gauge_id), scales = scales)
  } else {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[date_col]],
        y = .data[[variable]],
        color = .data$gauge_id
      )
    ) +
      ggplot2::geom_line(...) +
      ggplot2::labs(color = "Gauge ID")
  }

  p +
    ggplot2::labs(x = NULL, y = variable) +
    ggplot2::theme_bw()
}

#' @rdname plot_pe_timeseries
#' @export
plot_timeseries <- plot_pe_timeseries


#' Plot CAMELS-PE Catchments
#'
#' Creates a map of CAMELS-PE catchments and optionally overlays gauge
#' locations. If `gauge_id` is provided, only one catchment and its outlet
#' are plotted, with the gauge ID shown as a panel header.
#'
#' @param catchments An `sf` object with catchment polygons.
#' @param gauges Optional `sf` object with gauge point locations.
#' @param gauge_id Optional character string. Gauge ID used to filter one
#'   catchment and its gauge before plotting. If `NULL`, all catchments are
#'   plotted.
#' @param fill Optional character string. Name of a catchment column used to
#'   fill polygons. If `NULL`, a constant fill is used.
#' @param ... Additional arguments passed to \code{ggplot2::geom_sf()}.
#'
#' @return A `ggplot` object with the spatial catchment geometries.
#' @export
#'
#' @examples
#' catchments <- load_pe_geospatial(type = "catchments")
#' gauges <- load_pe_geospatial(type = "gauges")
#' plot_pe_catchments(catchments, gauges)
plot_pe_catchments <- function(catchments,
                               gauges = NULL,
                               gauge_id = NULL,
                               fill = NULL,
                               ...) {

  if (!inherits(catchments, "sf")) {
    cli::cli_abort("{.arg catchments} must be an {.cls sf} object.")
  }

  if (!is.null(gauges) && !inherits(gauges, "sf")) {
    cli::cli_abort("{.arg gauges} must be an {.cls sf} object.")
  }

  if (!"gauge_id" %in% names(catchments)) {
    cli::cli_abort("Column {.field gauge_id} not found in {.arg catchments}.")
  }

  if (!is.null(gauges) && !"gauge_id" %in% names(gauges)) {
    cli::cli_abort("Column {.field gauge_id} not found in {.arg gauges}.")
  }

  if (!is.null(gauge_id) && (!is.character(gauge_id) || length(gauge_id) != 1 || is.na(gauge_id))) {
    cli::cli_abort("{.arg gauge_id} must be a single character string or NULL.")
  }

  if (!is.null(fill)) {
    if (!is.character(fill) || length(fill) != 1 || is.na(fill)) {
      cli::cli_abort("{.arg fill} must be a single character string or NULL.")
    }

    if (!fill %in% names(catchments)) {
      cli::cli_abort("Fill variable {.field {fill}} not found in {.arg catchments}.")
    }
  }

  catchments$gauge_id <- as.character(catchments$gauge_id)

  if (!is.null(gauges)) {
    gauges$gauge_id <- as.character(gauges$gauge_id)
  }

  if (!is.null(gauge_id)) {
    catchments <- catchments[catchments[["gauge_id"]] == gauge_id, ]

    if (!is.null(gauges)) {
      gauges <- gauges[gauges[["gauge_id"]] == gauge_id, ]
    }
  }

  if (nrow(catchments) == 0) {
    cli::cli_abort("No catchment available for the selected {.arg gauge_id}.")
  }

  if (is.null(fill)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = catchments,
        fill = "#F5EFEB",
        color = "#567C8D",
        linewidth = 0.3,
        ...
      )
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = catchments,
        ggplot2::aes(fill = .data[[fill]]),
        color = "#567C8D",
        linewidth = 0.3,
        ...
      ) +
      ggplot2::scale_fill_viridis_c(na.value = "grey80") +
      ggplot2::labs(fill = fill)
  }

  if (!is.null(gauges) && nrow(gauges) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = gauges,
        size = 2,
        color = "#2F4156"
      )
  }

  if (!is.null(gauge_id)) {
    p <- p + ggplot2::facet_wrap(~gauge_id)
  }

  p +
    ggplot2::theme_bw() +
    ggplot2::labs(x = NULL, y = NULL)
}

#' @rdname plot_pe_catchments
#' @export
plot_catchments <- plot_pe_catchments


#' Plot CAMELS-PE Attribute Map
#'
#' Creates a thematic map of CAMELS-PE catchments using a selected attribute.
#' The function joins attribute data with catchment geometries by `gauge_id`
#' using `collapse::join` and returns a `ggplot` object.
#'
#' @param catchments An `sf` object with catchment polygons.
#' @param attributes A data frame containing CAMELS-PE attributes.
#' @param variable Character string. Name of the attribute to visualize.
#' @param gauges Optional `sf` object with gauge point locations.
#' @param na_color Character string. Color for missing values. Default is `"grey80"`.
#' @param ... Additional arguments passed to \code{ggplot2::geom_sf()} for catchments.
#'
#' @return A `ggplot` object visualizing the attribute map.
#' @export
#'
#' @examples
#' catchments <- load_pe_geospatial(type = "catchments")
#' attrs <- load_pe_attributes(attributes = "topographic")
#' plot_pe_attribute_map(catchments, attrs, variable = "area")
plot_pe_attribute_map <- function(catchments,
                                  attributes,
                                  variable,
                                  gauges = NULL,
                                  na_color = "grey80",
                                  ...) {

  if (!inherits(catchments, "sf")) {
    cli::cli_abort("{.arg catchments} must be an {.cls sf} object.")
  }

  if (!is.data.frame(attributes)) {
    cli::cli_abort("{.arg attributes} must be a data frame.")
  }

  if (!is.character(variable) || length(variable) != 1) {
    cli::cli_abort("{.arg variable} must be a single character string.")
  }

  if (!"gauge_id" %in% names(catchments)) {
    cli::cli_abort("{.arg catchments} must contain a {.field gauge_id} column.")
  }

  if (!"gauge_id" %in% names(attributes)) {
    cli::cli_abort("{.arg attributes} must contain a {.field gauge_id} column.")
  }

  if (!variable %in% names(attributes)) {
    cli::cli_abort("Variable {.field {variable}} not found in {.arg attributes}.")
  }

  if (!is.null(gauges) && !inherits(gauges, "sf")) {
    cli::cli_abort("{.arg gauges} must be an {.cls sf} object.")
  }

  if (!is.character(na_color) || length(na_color) != 1) {
    cli::cli_abort("{.arg na_color} must be a single character string.")
  }

  catchments_min <- catchments[, "gauge_id"]

  # Use fast collapse::join for data merging, and ensure it remains an sf object
  data <- collapse::join(catchments_min, attributes, on = "gauge_id", how = "left", verbose = FALSE)
  if (!inherits(data, "sf")) {
    data <- sf::st_as_sf(data)
  }

  if (!variable %in% names(data)) {
    cli::cli_abort("Variable {.field {variable}} not found after join.")
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = data,
      ggplot2::aes(fill = .data[[variable]]),
      color = "#567C8D",
      linewidth = 0.2,
      ...
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "viridis",
      na.value = na_color
    )

  if (!is.null(gauges) && nrow(gauges) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = gauges,
        size = 1.5,
        color = "#2F4156"
      )
  }

  p +
    ggplot2::labs(x = NULL, y = NULL, fill = variable) +
    ggplot2::theme_bw()
}

#' @rdname plot_pe_attribute_map
#' @export
plot_attribute_map <- plot_pe_attribute_map
