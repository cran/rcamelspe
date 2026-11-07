
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcamelspe

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/rcamelspe)](https://CRAN.R-project.org/package=rcamelspe) -->
<!-- badges: end -->

The goal of `rcamelspe` is to provide an ultra-fast, computationally
efficient, and user-friendly R interface to download, load, filter, and
manage the **CAMELS-PE** (Catchment Attributes and Meteorology for
Large-sample Studies - Peru) dataset.

CAMELS-PE includes daily meteorological and hydrological time series,
static catchment attributes, and geospatial representations for **136
catchments** distributed across the Pacific, Atlantic, and Titicaca
hydrographic regions of Peru.

To handle large volumes of records (over 2.2 million rows of daily
climate observations and simulations) with maximum speed and minimum
memory footprint, the package is engineered using: - **`arrow`**: for
memory-mapped dataset reading, predicate pushdown (filtering stations
and dates directly at the C++ scan level), and zero-overhead column
projection. - **`collapse`**: for fast C/C++ data frame transformations,
grouped calculations, and relational joins. Individual time-series
batches use `dplyr::bind_rows()` for stable output types. -
**`RCamelsPE` compatibility**: convenience wrappers and aliases
(`read_timeseries()`, `read_attributes()`, `read_metadata()`,
`read_dictionary()`, `read_geospatial()`, `set_camels_path()`,
`get_camels_path()`).

------------------------------------------------------------------------

## Installation

When available on CRAN, install with:

``` r
install.packages("rcamelspe")
```

And the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("PaulESantos/rcamelspe")
```

------------------------------------------------------------------------

## Bundled Sample Dataset & Instant Exploration

`rcamelspe` includes a lightweight, self-contained sample dataset in
`inst/extdata/sample_camels_pe` (~120 KB) covering 2 representative
catchments (`PE_110139` and `PE_111151`). This allows you to explore all
functions immediately after installation without waiting for large
downloads.

When you are ready to work with the complete 136-catchment dataset
(approx. 120 MB zip, extracting to 186 MB), you can download it directly
from Zenodo (DOI: 10.5281/zenodo.21195425):

``` r
library(rcamelspe)

# Download and extract the full dataset to a destination directory
download_pe_data(dest_dir = "path/to/CAMELS-PE", version = "1.0.1")
```

------------------------------------------------------------------------

## Quick Start Example

``` r
library(rcamelspe)
```

### 1. Load Station Metadata & Data Dictionary

Read the gauging station metadata or inspect the unified data
dictionary:

``` r
# Gauging stations
stations <- load_pe_metadata(type = "stations")
head(stations[, c("gauge_id", "gauge_name", "gauge_region", "gauge_elev")])
#>    gauge_id       gauge_name gauge_region gauge_elev
#> 1 PE_110139            Picoy      Pacific       3037
#> 2 PE_111151 Puente Magdalena      Pacific        872

# Data dictionary
dict <- read_dictionary(category = "climatic")
head(dict[, c("variable", "description", "unit")])
#>         variable                                   description      unit
#> 1       gauge_id                          Catchment identifier      <NA>
#> 2         p_mean                      Mean daily precipitation    mm/day
#> 3       pet_mean Mean daily potential evapotranspiration (PET)    mm/day
#> 4        aridity       Ratio of mean PET to mean precipitation      <NA>
#> 5  p_seasonality       Seasonality and timing of precipitation      <NA>
#> 6 high_prec_freq          Frequency of high precipitation days days/year
```

------------------------------------------------------------------------

### 2. Load and Merge Catchment Attributes

Retrieve topographic, geologic, soil, land cover, climatic indices,
hydrological signatures, or human intervention attributes:

``` r
# Load merged topographic and landcover attributes
attrs <- load_pe_attributes(c("topographic", "landcover"))
head(attrs[, c("gauge_id", "area", "elev_mean", "forest_perc")])
#>    gauge_id     area elev_mean forest_perc
#> 1 PE_110139  360.584  4534.981       1.827
#> 2 PE_111151 1263.804  3542.298       3.511
```

------------------------------------------------------------------------

### 3. Load Daily Time Series (Highly Optimized)

Load time series for specific catchments, filter by date range, and
select only required variables at scan time:

``` r
# Load precipitation and observed streamflow with date filter
ts_data <- load_pe_timeseries(
  gauge_ids = "PE_110139", 
  variables = c("prec", "flow_obs"),
  start_date = "2000-01-01",
  end_date = "2000-06-30"
)
head(ts_data)
#>         date  gauge_id  prec flow_obs
#> 1 2000-01-01 PE_110139 5.758       NA
#> 2 2000-01-02 PE_110139 3.417       NA
#> 3 2000-01-03 PE_110139 5.581       NA
#> 4 2000-01-04 PE_110139 5.377       NA
#> 5 2000-01-05 PE_110139 3.782       NA
#> 6 2000-01-06 PE_110139 7.771       NA
```

------------------------------------------------------------------------

### 4. Access Geospatial Layers & Plotting

Load catchment boundaries and station locations as `sf` objects, and
plot them:

``` r
# Catchment boundaries
catchments <- load_pe_geospatial(type = "catchments")
gauges <- load_pe_geospatial(type = "gauges")

# Plot time series
p_ts <- plot_pe_timeseries(ts_data, variable = "prec")

# Plot catchments with gauge overlays
p_map <- plot_pe_catchments(catchments, gauges = gauges)
```

------------------------------------------------------------------------

### 5. Compatibility Wrappers with `RCamelsPE`

The following wrappers support common `RCamelsPE` workflows:

``` r
# Compatible syntax:
stations <- read_metadata()
dict     <- read_dictionary(category = "topographic")
attrs    <- read_attributes(type = "topographic")
ts       <- read_timeseries(gauge_id = "PE_110139", vars = c("date", "prec", "flow_obs"))
spatial  <- read_geospatial(type = "catchments")
```

## CAMELS-PE data reference

CAMELS-PE v1.0.1, produced by SENAMHI, describes **136 catchments**: 67
Pacific, 56 Atlantic and 13 Titicaca. It provides **79 static
attributes** in seven thematic groups and **10 daily variables** on the
1981-2025 calendar. The calendar does not imply complete observations:
unavailable dates remain `NA`. Check `gauge_record_start`,
`gauge_record_end` and `gauge_perc_obs` in station metadata.
Hydrological signatures are derived from **simulated streamflow**.
Spatial layers are GeoPackages in WGS84 (EPSG:4326), linked by
`gauge_id`.

| Daily variable | Meaning | Unit | Source |
|----|----|----|----|
| `prec` | Precipitation | mm/day | PISCOp v2.1 |
| `prec_var` | Precipitation variance | mm²/day² | PISCOp v2.1 |
| `pet` | Potential evapotranspiration | mm/day | PISCOeo_pm v1.0 |
| `tmin`, `tmean`, `tmax` | Minimum, mean and maximum temperature | °C | PISCOt v1.2 |
| `srad` | Surface solar radiation | MJ/m²/day | ERA5-Land |
| `vprp` | Vapor pressure | hPa | ERA5-Land |
| `flow_obs` | Observed streamflow | mm/day | SENAMHI |
| `flow_sim` | Simulated streamflow | mm/day | PISCO-ARNOVIC v1.1 |

Attribute sources are FABDEM v1.2 (topography), MapBiomas Peru and
Ecuador (land cover), GLiM v1.0 and GLHYMPS v2.0 (geology), DSOLMap
(soil orders), and ANA (human intervention). Consult `read_dictionary()`
for file-level definitions.

### Batch access

``` r
# Explicitly use the bundled two-catchment, year-2000 sample
sample_path <- system.file("extdata", "sample_camels_pe", package = "rcamelspe")
stations <- read_metadata(path = sample_path)
daily <- read_timeseries(
  gauge_id = stations$gauge_id, global = TRUE,
  vars = c("date", "prec", "prec_var", "flow_obs", "flow_sim"),
  start_date = "2000-01-01", end_date = "2000-01-31", path = sample_path
)
# For the full release, use the directory returned by download_pe_data().
```

### Sources, license and citation

The data (including the bundled subset) are licensed under **CC BY
4.0**; the `rcamelspe` package code uses the license in DESCRIPTION.
This package is a separate project from Harold Llauca’s
[RCamelsPE](https://github.com/hllauca/RCamelsPE), linked by SENAMHI.

Official references: [CAMELS-PE
portal](https://www.senamhi.gob.pe/camels-pe/) and [technical
description](https://zenodo.org/records/21195425/files/CAMELS-PE_Technical_Data_Description.pdf).

When using CAMELS-PE, cite both:

- Llauca, H., Montesinos-Caceres, C., Gutierrez-Reynaga, M., &
  Lavado-Casimiro, W. (2026). CAMELS-PE: Hydrometeorological time series
  and catchment attributes for 136 catchments in Peru \[Preprint\].
  Earth System Science Data Discussions.
  [doi:10.5194/essd-2026-386](https://doi.org/10.5194/essd-2026-386).
- Llauca, H., Montesinos-Caceres, C., Gutierrez-Reynaga, M., &
  Lavado-Casimiro, W. (2026). CAMELS-PE: Catchment Attributes and
  Meteorology for Large-sample Studies in Peru (Version 1.0.1) \[Data
  set\]. Zenodo.
  [doi:10.5281/zenodo.21195425](https://doi.org/10.5281/zenodo.21195425).
"# rcamelspe" 
