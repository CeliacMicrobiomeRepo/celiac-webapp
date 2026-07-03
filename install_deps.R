# Install and verify R package dependencies for the Shiny app.

# Rcpp is a build dependency for terra. Install first since terra builds from source below.
if (!requireNamespace("Rcpp", quietly = TRUE)) {
  utils::install.packages("Rcpp", repos = "https://cloud.r-project.org")
}

# Pin terra to a version that compiles against older GDAL. terra >= 1.8 fails to compile.
# rnaturalearth hard-imports terra, so this must run before it's installed.
if (!requireNamespace("terra", quietly = TRUE) || utils::packageVersion("terra") >= "1.8") {
  utils::install.packages(
    "https://cran.r-project.org/src/contrib/Archive/terra/terra_1.7-78.tar.gz",
    repos = NULL,
    type = "source"
  )
}

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
  utils::install.packages(to_install, repos = "https://cloud.r-project.org", Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L))
}