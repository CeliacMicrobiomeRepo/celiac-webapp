# The Celiac Microbiome Repository Web App
This is the R Shiny web application used to view the Celiac Microbiome Repository

View the live web app [here](https://celiac.shinyapps.io/celiac-webapp/)

## The Celiac Microbiome Repository
The Celiac Microbiome Repository (CMR) is the best effort to comprehensively combine all high throughput sequencing datasets of the gut microbiome related to celiac disease. This [publication](https://paper.link) used data from version 1.0 of The Celiac Microbiome Repository (CMR), which was up to date as of **15th July 2025**. CMR is being extended beyond this date and is accessible on the [CMR GitHub Repo](https://github.com/CeliacMicrobiomeRepo/celiac-repository/tree/main/)


## Authors
- **Haig Bishop**:   haig.bishop@pg.canterbury.ac.nz
- **Peter Prendergast**


---


## Requirements & Licenses

## Project License
This code for this project is licensed under the **GNU General Public License v3.0 (GPL-3)** to ensure compatibility.

### R Dependencies
- **shiny**: [GPL-3](https://github.com/rstudio/shiny/blob/main/LICENSE)
- **DT**: [GPL-3](https://github.com/rstudio/DT/blob/main/LICENSE)
- **ggplot2**: [MIT](https://github.com/tidyverse/ggplot2/blob/main/LICENSE.md) (Code is MIT, package is GPL-3 due to dependencies)
- **dplyr**: [MIT](https://github.com/tidyverse/dplyr/blob/main/LICENSE.md)
- **sf**: [GPL-2 | GPL-3 | MIT](https://cran.r-project.org/web/packages/sf/index.html) (Complex licensing, see CRAN page)
- **rworldmap**: [GPL-2 | GPL-3](https://cran.r-project.org/web/packages/rworldmap/index.html) (See CRAN page)
- **rworldxtra**: [GPL-2 | GPL-3](https://cran.r-project.org/web/packages/rworldxtra/index.html) (See CRAN page)
- **rnaturalearth**: [MIT](https://cran.r-project.org/web/packages/rnaturalearth/index.html)
- **rnaturalearthdata**: [CC0 1.0](https://cran.r-project.org/web/packages/rnaturalearthdata/index.html)
- **countrycode**: [GPL-3](https://cran.r-project.org/web/packages/countrycode/index.html) (See CRAN page)
- **gridExtra**: [GPL-2 | GPL-3](https://cran.r-project.org/web/packages/gridExtra/index.html) (See CRAN page)
- **reshape2**: [MIT](https://github.com/cran/reshape2/blob/master/LICENSE)
- **markdown**: [MIT](https://github.com/rstudio/markdown/blob/master/LICENSE.md)
- **stringr**: [MIT](https://github.com/tidyverse/stringr/blob/main/LICENSE.md)

### Python Dependencies (for data fetching)
- **requests**: [Apache 2.0](https://github.com/psf/requests/blob/main/LICENSE)

### JavaScript Dependencies
- **jQuery**: [MIT](https://jquery.org/license/)


## Deployment
1. Your system needs R some R packages
   - R 4.4.1 is proven to be stable
   - Use install_deps.R to see R packages
2. You'll need to obtain the `.tsv` data files from the CMR
   - The Python script `fetch_repo_data.py` does this automatically.
   - The 4 `.tsv` files sit in `repo_data`
3. To deploy the application to shinyapps.io run this using RStudio Connect:
  ```r
  library(rsconnect)
  rsconnect::deployApp(
    appDir = "/Users/haig/Repos/celiac-webapp",
    account = "celiac"
  )
  ```