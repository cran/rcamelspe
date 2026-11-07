# Global variables for CRAN check compliance
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "gauge_id",
    "date",
    "category",
    "variable",
    "file",
    ".data"
  ))
}
