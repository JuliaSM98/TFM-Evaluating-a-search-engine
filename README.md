# TFM: Evaluating a search engine

## Description
This repository stores the code of a web application designed to perform user experience experiments that will allow two search engines to be compared. The application allows you to configure and monitor the tasks that each user performs with the search engines, in addition to allowing the administrator user to view and download all the data collected from the experiments. Additionally, the admin user can configure many aspects of the application, such as the search engines url or text within the website.
 
## Dependencies
The project has been developed using the following sotware:
<!--- Check how dependencies are usually specified in R apps --->
- [R](https://www.r-project.org/) version 4.1.2 with the following packages:
  - [shiny](https://shiny.rstudio.com/) (>= 1.7.1).
  - [shinydashboard](https://rstudio.github.io/shinydashboard/get_started.html) (>= 0.7.2).
  - [shinyjs](https://www.rdocumentation.org/packages/shinyjs/versions/2.1.0) (>= 2.1.0).
  - [data.table](https://cran.r-project.org/web/packages/data.table/data.table.pdf) (>= 1.14.2).
  - [DT](https://www.rdocumentation.org/packages/DT/versions/0.16) (>= 0.23).
  - [lubridate](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0) (>=1.8.0).
  - [base](https://stat.ethz.ch/R-manual/R-devel/library/base/html/00Index.html) (>=4.2.0).
  - [tidyverse](https://www.rdocumentation.org/packages/tidyverse/versions/1.3.1) (>1.3.1).
  - [xlsx](https://cran.r-project.org/web/packages/xlsx/xlsx.pdf) (>=0.6.5).
  - [shinysurveys](https://cran.r-project.org/web/packages/shinysurveys/vignettes/surveying-shinysurveys.html) (>=0.2.0).
  - [bcrypt](https://cran.r-project.org/web/packages/bcrypt/bcrypt.pdf) (>=1.1).
  - [xml2](https://cran.r-project.org/web/packages/xml2/xml2.pdf) (>=1.3.3).
  - [shinyBS](https://cran.r-project.org/web/packages/shinyBS/shinyBS.pdf) (>=0.61.1).

<!--- sessionInfo output:
```
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.3 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=es_ES.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=es_ES.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=es_ES.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=es_ES.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] dttr2_0.4.0          geojsonio_0.9.4      gdata_2.18.0         dplyr_1.0.7          rgdal_1.5-28         sp_1.4-5
 [7] here_1.0.1           mapdeck_0.3.4        chorddiag_0.1.3      shinydashboard_0.7.2 shiny_1.7.1

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.8         lattice_0.20-45    class_7.3-19       gtools_3.9.2       assertthat_0.2.1   rprojroot_2.0.2
 [7] digest_0.6.29      utf8_1.2.2         V8_4.0.0           mime_0.12          R6_2.5.1           e1071_1.7-9
[13] geojson_0.3.4      pillar_1.6.4       rlang_0.4.12       lazyeval_0.2.2     curl_4.3.2         fontawesome_0.2.2
[19] jquerylib_0.1.4    jqr_1.2.2          foreign_0.8-81     htmlwidgets_1.5.4  proxy_0.4-26       compiler_4.1.2
[25] httpuv_1.6.5       pkgconfig_2.0.3    rgeos_0.5-9        htmltools_0.5.2    tidyselect_1.1.1   tibble_3.1.5
[31] httpcode_0.3.0     fansi_0.5.0        crayon_1.4.2       withr_2.4.3        later_1.3.0        sf_1.0-5
[37] crul_1.2.0         grid_4.1.2         jsonify_1.2.1      jsonlite_1.7.2     xtable_1.8-4       lifecycle_1.0.1
[43] DBI_1.1.1          magrittr_2.0.1     units_0.7-2        KernSmooth_2.23-20 cachem_1.0.6       promises_1.2.0.1
[49] bslib_0.3.1        ellipsis_0.3.2     chk_0.7.0          generics_0.1.1     vctrs_0.3.8        RColorBrewer_1.1-2
[55] geojsonsf_2.0.1    tools_4.1.2        glue_1.6.0         purrr_0.3.4        hms_1.1.1          rsconnect_0.8.25
[61] yaml_2.2.1         fastmap_1.1.0      maptools_1.1-2     classInt_0.4-3     sass_0.4.0
```
--->


## Usage
- To launch the applicaion, run server.R or ui.R in RStudio
- The web application can be found [here](https://rstudio-connect.nlp.linkeddata.es/content/7dda9ea4-48f8-49fe-9fc6-60946167281b)

## Author
- Júlia Sánchez Martínez ([@Julia-upc](https://github.com/Julia-upc))
