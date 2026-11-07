# set_camels_pe_path and get_camels_pe_path work

    Code
      set_camels_pe_path("dummy-path")
    Condition
      Warning:
      The provided CAMELS-PE path does not exist: [path]/dummy-path

---

    Code
      set_camels_pe_path(temp_dir)
    Condition
      Warning:
      The following required CAMELS-PE folders are missing: '01_metadata', '02_attributes', '03_timeseries', and '04_geospatial'

