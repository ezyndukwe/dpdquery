status_mapping <- data.frame(
  status_code = c(1, 2, 3, 4, 6, 9, 10, 11, 12, 13, 14, 15),
  status_name = c(
    "Approved",
    "Marketed",
    "Cancelled Pre Market",
    "Cancelled Post Market",
    "Dormant",
    "Cancelled (Unreturned Annual)",
    "Cancelled (Safety Issue)",
    "Authorized By Interim Order",
    "Authorization By Interim Order Revoked",
    "Restricted Access",
    "Authorization By Interim Order Expired",
    "Cancelled (Transitioned to Biocides)"
  )
)
usethis::use_data(status_mapping, overwrite = TRUE)

#' Mapping table for status codes and descriptions
#'
#' A mapping table for the status codes and descriptions in the Drug Product Database.
#'
#' @format `status_mapping`: a data frame with 15 rows and 2 columns:
#' \describe{
#'   \item{status_code}{Status code}
#'   \item{status_name}{Status name}
#' }
#' @source <https://health-products.canada.ca/api/documentation/dpd-documentation-en.html#a4.1>
"status_mapping"
