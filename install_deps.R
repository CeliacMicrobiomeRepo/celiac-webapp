# Install and verify R package dependencies for the Shiny app.

# Pinned to a fixed CRAN snapshot date (via Posit Package Manager) instead of "latest",
# so every package -- including transitive deps like terra -- resolves consistently from
# one point in time. terra >= 1.8 doesn't compile against the GDAL on shinyapps.io's build
# servers; this date keeps terra on the 1.7.x line without needing to pin it by hand.
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2024-08-01"))

# All packages referenced in app.R (and their namespaces)
required_packages <- c(
  "markdown",
  "shiny",
  "DT",
  "ggplot2",
  "plotly",
  "rworldmap",
  "rworldxtra",
  "dplyr",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "countrycode",
  "gridExtra",
  "reshape2",
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