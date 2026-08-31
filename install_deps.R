# Install and verify R package dependencies for the Shiny app.

# All packages referenced in app.R (and their namespaces)
required_packages <- c(
  "markdown",
  "shiny",
  "DT",
  "ggplot2",
  "plotly",
  "dplyr",
  "countrycode",
  "gridExtra",
  "scales",
  "jsonlite",
  "readr",
  "stringr"
)

installed <- rownames(utils::installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  utils::install.packages(to_install, Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L))
}