# TFM: Evaluating a search engine

## Description
This repository stores the code of a web application designed to carry out subject-centered usability experiments that will allow two search engines from the same domain to be compared. The application allows configuring and monitoring the tasks that each user performs with one of the search engines, in addition to allowing the administrator user to view and download all the data collected from the experiments. In addition, the admin user can configure many aspects of the application, such as the search engine URL or the text within the website. This repository can be used by anyone who wants to perform similar usability experiments with any search engine.

The fully configurable application has been posted online [here](https://rstudio-connect.nlp.linkeddata.es/content/7dda9ea4-48f8-49fe-9fc6-60946167281b).
 
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

- [RStudio](https://www.rstudio.com/products/rstudio/) as integrated development environment. 

## Usage
To launch the applicaion, donwload the repository and run server.R or ui.R files in RStudio Server or RStudio Desktop. You can also access the version available online mentioned above.

## Author
- Júlia Sánchez Martínez ([@Julia-upc](https://github.com/Julia-upc))
