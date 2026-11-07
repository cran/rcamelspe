## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = '#>', fig.width = 8,
                     fig.height = 6.5, dpi = 144)
candidates <- c(system.file('benchmarks', package = 'rcamelspe'),
                '../inst/benchmarks', 'inst/benchmarks')
bench_dir <- candidates[file.exists(file.path(candidates, 'results', 'summary.csv'))][1]
stopifnot(!is.na(bench_dir))
results <- read.csv(file.path(bench_dir, 'results', 'summary.csv'))
checks <- read.csv(file.path(bench_dir, 'results', 'equivalence.csv'))
manifest <- read.csv(file.path(bench_dir, 'results', 'data-manifest.csv'))
commit <- trimws(readLines(file.path(bench_dir, 'upstream-commit.txt'))[1])
labels <- c(metadata = 'Metadatos: 136 estaciones', attributes = 'Atributos: siete tablas',
  one = 'Una cuenca: todas las variables', five = 'Cinco cuencas: dos variables',
  ten = 'Diez cuencas: dos variables',
  twenty = 'Veinte cuencas: dos variables', forty = 'Cuarenta cuencas: dos variables',
  global_all = 'Nacional: todas las variables',
  global_projected = 'Nacional: dos variables', global_window = 'Nacional: dos variables, año 2000',
  gauges = 'Geoespacial: estaciones')
official <- results[results$package == 'RCamelsPE', ]
ours <- results[results$package == 'rcamelspe', ]
comparison <- merge(official, ours, by = 'scenario', suffixes = c('_ref', '_ours'))
comparison <- comparison[match(names(labels), comparison$scenario), ]
comparison$speedup <- comparison$median_s_ref / comparison$median_s_ours
comparison$saving <- 100 * (1 - comparison$median_s_ours / comparison$median_s_ref)
best <- comparison[which.max(comparison$speedup), ]

## ----results-table, echo=FALSE------------------------------------------------
format_time <- function(m, q1, q3) sprintf('%.3f [%.3f–%.3f]', m, q1, q3)
table <- data.frame(
  Escenario = unname(labels[comparison$scenario]),
  RCamelsPE = format_time(comparison$median_s_ref, comparison$q25_s_ref, comparison$q75_s_ref),
  rcamelspe = format_time(comparison$median_s_ours, comparison$q25_s_ours, comparison$q75_s_ours),
  Aceleracion = sprintf('%.2fx', comparison$speedup), check.names = FALSE)
knitr::kable(table)

## ----speedup-plot, echo=FALSE, fig.cap='Aceleración calculada a partir de las medianas. La línea vertical indica igualdad de tiempo.'----
library(ggplot2)
comparison$label <- factor(unname(labels[comparison$scenario]), levels = rev(unname(labels)))
ggplot(comparison, aes(speedup, label)) +
  geom_vline(xintercept = 1, colour = '#89939E', linetype = 2) +
  geom_segment(aes(x = 1, xend = speedup, yend = label), colour = '#A8BCCB', linewidth = 1) +
  geom_point(colour = '#245A73', size = 3) +
  geom_text(aes(label = sprintf('%.2fx', speedup)), nudge_y = .23, size = 3.2) +
  scale_x_log10() +
  labs(x = 'Aceleración (escala logarítmica; mayor favorece a rcamelspe)', y = NULL) +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())

## ----interpretation, echo=FALSE, results='asis'-------------------------------
for (key in c('global_all', 'global_projected', 'global_window', 'ten')) {
  row <- comparison[comparison$scenario == key, ]
  cat(sprintf('- **%s:** %.3f s con `rcamelspe` y %.3f s con `RCamelsPE` (%.2fx).\n',
      labels[key], row$median_s_ours, row$median_s_ref, row$speedup))
}

## ----before-after, echo=FALSE-------------------------------------------------
previous <- read.csv(file.path(bench_dir, 'before-routing', 'summary.csv'))
old <- previous$median_s[previous$scenario == 'ten' & previous$package == 'rcamelspe']
new <- comparison$median_s_ours[comparison$scenario == 'ten']
knitr::kable(data.frame(Consulta = 'Diez cuencas',
  Anterior_s = round(old, 3), Actual_s = round(new, 3),
  Mejora_interna = sprintf('%.2fx', old / new)))

## ----equivalence-table, echo=FALSE--------------------------------------------
checks <- checks[match(names(labels), checks$scenario), ]
knitr::kable(data.frame(Escenario = unname(labels[checks$scenario]),
  Filas = checks$rows, Columnas = checks$columns,
  Equivalente = ifelse(checks$equivalent, 'Sí', 'No')))

## ----reproduce, eval=FALSE----------------------------------------------------
# # Preparación (solo una vez):
# install.packages(c('pkgload', 'remotes'))
# remotes::install_deps('.', dependencies = TRUE)
# remotes::install_deps('tmp/benchmark-reference', dependencies = TRUE)
# # Desde una terminal, sustituir las rutas:
# # Rscript inst/benchmarks/run-benchmark.R /datos/CAMELS-PE tmp/benchmark-reference inst/benchmarks/results 7
# 
# rmarkdown::render('vignettes/benchmark.Rmd')

## ----environment, echo=FALSE--------------------------------------------------
cat(readLines(file.path(bench_dir, 'results', 'session.txt')), sep = '\n')

## ----example, eval=FALSE------------------------------------------------------
# path <- rcamelspe::get_camels_pe_path()
# daily <- rcamelspe::read_timeseries(
#   global = TRUE,
#   vars = c('date', 'gauge_id', 'prec', 'flow_obs'),
#   start_date = '2000-01-01', end_date = '2000-12-31', path = path
# )

