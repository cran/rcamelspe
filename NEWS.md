# rcamelspe 0.1.0

* Initial release of `rcamelspe`.
* High-performance data reading engine:
  * Uses `arrow::open_dataset()` with predicate pushdown and column projection for lightning-fast querying and minimal memory overhead on daily hydroclimatic records.
  * Adaptive routing based on CSV size and opening-cost heuristic, avoiding unnecessary scans for medium-sized selections.
  * Added `start_date` and `end_date` parameters to `load_pe_timeseries()` for temporal slicing at scan time.
  * Accelerated `load_pe_attributes()` by filtering target gauges prior to relational joining with `collapse::join()`.
  * Preserves original data integrity when binding tables.
* Default downloads use CAMELS-PE 1.0.1 (Zenodo 21195425), with 1.0 remaining selectable.
  * Download wrappers honor `version` and `set_path`.
* Drop-in compatibility with `RCamelsPE`:
  * Added function aliases `read_timeseries()`, `read_attributes()`, `read_metadata()`, `read_dictionary()`, `read_geospatial()`, `download_camels_pe()`, `set_camels_path()`, `get_camels_path()`, `plot_timeseries()`, `plot_catchments()`, and `plot_attribute_map()`.
* Bundled sample dataset:
  * Added a lightweight self-contained sample dataset in `inst/extdata/sample_camels_pe` (~120 KB) enabling autonomous execution of examples and tests without requiring external internet downloads.
* Added visualization utilities:
  * `plot_pe_timeseries()` with curated color palette and faceting options.
  * `plot_pe_catchments()` for boundaries and gauging station spatial inspection.
  * `plot_pe_attribute_map()` for choropleth mapping of catchment attributes.
