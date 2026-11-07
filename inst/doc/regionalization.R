## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  out.width = "100%"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
if (!requireNamespace("rcamelspe", quietly = TRUE)) {
  pkg_dir <- if (file.exists("../DESCRIPTION")) ".." else if (file.exists("DESCRIPTION")) "." else NULL
  if (!is.null(pkg_dir)) {
    r_files <- list.files(file.path(pkg_dir, "R"), full.names = TRUE, pattern = "\\.[rR]$")
    invisible(lapply(r_files, sys.source, envir = globalenv()))
  }
}
if (requireNamespace("rcamelspe", quietly = TRUE)) {
  library(rcamelspe)
}
library(ggplot2)
library(sf)

old_camels_path <- getOption("rcamelspe.path")

# Configurar la ruta al dataset CAMELS-PE si está disponible localmente
# Buscamos rutas relativas comunes en el espacio de trabajo
path <- "raw-data/CAMELS-PE"
if (dir.exists(path) && exists("set_camels_pe_path", mode = "function")) {
  set_camels_pe_path(path)
} else if (dir.exists("../raw-data/CAMELS-PE") && exists("set_camels_pe_path", mode = "function")) {
  set_camels_pe_path("../raw-data/CAMELS-PE")
} else if (dir.exists("../../raw-data/CAMELS-PE") && exists("set_camels_pe_path", mode = "function")) {
  set_camels_pe_path("../../raw-data/CAMELS-PE")
}

# Verificar si el dataset está disponible antes de ejecutar
has_data <- exists("get_camels_pe_path", mode = "function") &&
  !is.null(get_camels_pe_path()) &&
  dir.exists(get_camels_pe_path())

## ----load_data, message=FALSE-------------------------------------------------
# Verificar si el dataset está disponible antes de ejecutar
has_data <- exists("get_camels_pe_path", mode = "function") &&
  !is.null(get_camels_pe_path()) &&
  dir.exists(get_camels_pe_path())

if (has_data) {
  # Cargar atributos topográficos y climáticos mezclados por gauge_id
  attrs <- load_pe_attributes(attributes = c("topographic", "climatic"))
  
  # Cargar geometrías vectoriales de las cuencas (polígonos GPKG)
  catchments <- load_pe_geospatial(type = "catchments")
  
  # Seleccionar las variables numéricas para el análisis
  clustering_vars <- c("area", "elev_mean", "slope_mean", "p_mean", "aridity")
  
  # Filtrar registros completos
  data_clustering <- attrs[, c("gauge_id", clustering_vars)]
  data_clustering <- na.omit(data_clustering)
  
  # Vista rápida de los atributos cargados
  head(data_clustering)
}

## ----clustering---------------------------------------------------------------
if (has_data) {
  # Estandarizar atributos
  scaled_data <- scale(data_clustering[, clustering_vars])
  
  # Ejecutar algoritmo K-means si hay suficientes cuencas, o partición adaptativa
  set.seed(123) # Para reproducibilidad
  if (nrow(data_clustering) >= 3) {
    k_centers <- min(3, nrow(data_clustering))
    kmeans_res <- kmeans(scaled_data, centers = k_centers, nstart = 25)
    data_clustering$region <- as.factor(kmeans_res$cluster)
  } else {
    data_clustering$region <- as.factor(seq_len(nrow(data_clustering)))
  }
  
  # Contar cuántas cuencas pertenecen a cada región
  table(data_clustering$region)
}

## ----geospatial_join----------------------------------------------------------
if (has_data) {
  # Realizar un Left Join rápido entre el objeto espacial y los resultados de clustering
  catchments_regions <- collapse::join(
    catchments, 
    data_clustering[, c("gauge_id", "region")], 
    on = "gauge_id", 
    how = "left"
  )
  
  # Forzar a sf si collapse limpió los atributos espaciales
  if (!inherits(catchments_regions, "sf")) {
    catchments_regions <- sf::st_as_sf(catchments_regions)
  }
}

## ----plot_map, fig.align='center', eval = isTRUE(has_data)--------------------

  ggplot(catchments_regions) +
    geom_sf(aes(fill = region), color = "grey40", linewidth = 0.15) +
    scale_fill_brewer(palette = "Set2", name = "Región Hidroclimática\n(Grupo)") +
    labs(
      title = "Regionalización Hidroclimática de Cuencas en el Perú",
      subtitle = "Clasificación basada en K-means (K = 3) sobre atributos físicos y climáticos",
      caption = "Datos físicos y climáticos extraídos de CAMELS-PE",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(color = "grey30", size = 9),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
    )


## ----interpretation-----------------------------------------------------------
if (has_data) {
  # Calcular promedios grupales utilizando collapse::collap
  resumen_regiones <- collapse::collap(
    data_clustering[, c("region", clustering_vars)],
    by = ~region,
    FUN = mean
  )
  
  print(resumen_regiones)
}

## ----cleanup, include=FALSE---------------------------------------------------
options(rcamelspe.path = old_camels_path)

