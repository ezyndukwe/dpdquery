README
================
Ezinne Ndukwe

# R/dpdquery

## Overview

The DPD Query package (dpdquery) is a suite of functions designed to
query Health Canada’s Drug Product Database (DPD) through component
APIs. The DPD is managed by Health Canada and provides information on
drugs approved and authorized for sale in Canada.

This package is intended to assist with queries of the DPD. The results
are returned in a structured data frame that is integrated in the R
environment and can be exported to external files.

I did not make this package as an employee of Health Canada, nor did I
receive any form of payment for making this package. For more
information regarding the DPD and the information provided, please
consult Health Canada resources and contact their team.

- [Drug Product Database: Access the
  database](https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database.html)

- [Drug Product Database (DPD) API
  Guide](https://health-products.canada.ca/api/documentation/dpd-documentation-en.html)

  ------------------------------------------------------------------------

## Installation

``` r

# Install devtools package
# install.packages("devtools")

# Install development version of dpdquery from GitHub
devtools::install_github("ezyndukwe/dpdquery", dependencies = TRUE, build_vignettes = TRUE, upgrade = 'never')
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\ezynd\AppData\Local\Temp\RtmpE7q5wz\remotes68602e5f47be\ezyndukwe-dpdquery-b70ea91/DESCRIPTION' ...  ✔  checking for file 'C:\Users\ezynd\AppData\Local\Temp\RtmpE7q5wz\remotes68602e5f47be\ezyndukwe-dpdquery-b70ea91/DESCRIPTION' (400ms)
#>       ─  preparing 'dpdquery':
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  installing the package to build vignettes (462ms)
#>          creating vignettes ...     creating vignettes ...   ✔  creating vignettes (1m 0.5s)
#>       ─  checking for LF line-endings in source and make files and shell scripts (1.5s)
#>   ─  checking for empty or unneeded directories
#> ─  building 'dpdquery_1.0.0.0.tar.gz'
#>      
#> 
library(dpdquery)
```

## Background

As a pharmcoepidemiology researcher, I realized that I could not find a
way to export the results of the Health Canada Drug Product Database
online query. I made this package to integrate the results of the online
query into my R-based environment. The package returns results in data
frame which can then be written into tabular data files and used in many
programming software.

This package is designed to query the DPD in a replicable, verifiable
and efficient process. Using the `query_dpd_by_activeIngredient()`
function in this package, I was able to search the entire DPD for 35
diabetic active ingredients and retrieve information on 990 drug
products in a structured table in just 13 minutes. I was able to review
the output results and confirm that the active ingredient terms that did
not return any drug products were truly ingredients that are not
approved in Canada (and because of not typos).

For individuals who wish to retrieve all the same information found in
the DPD online query but using an R-based tool, this package is for you!
You can use the functions in the package that begin with
“`query_dpd_by_`” to access multiple component APIs of the DPD and run a
comprehensive query that replicates the results of the online DPD query.
You can query the DPD by active ingredient, brand name (product name),
drug identification number (DIN) or drug product code. You can also use
functions that start with “`search_`” to access the component APIs
individually.

------------------------------------------------------------------------

## Vignette

For an examples of workflows using the dpdquery, please see a list of
all the vignettes I prepared with `vignette(package = 'dpdquery')`. To
read a specific vignette, run `browseVignettes(package = 'dpdquery')` or
`vignette(topic, package = 'dpdquery'`).

### Topics

1.  ‘dpdquery’ - Getting started with dpdquery and exploring tools
2.  ‘Monitoring_drug_status’ - Case example of ways to stay up to date
    on the status of drug products
