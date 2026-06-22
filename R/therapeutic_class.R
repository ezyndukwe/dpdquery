# Therapeutic Class API -----------------------------------------------------------

#' @title Query Therapeutic Class API of the Drug Product Database
#' @description A function that queries Therapeutic Class API using drug codes.
#' @details
#' Use [search_therapeuticclass_api()] if you wish to query the API using a vector of multiple drug codes.
#'
#' @param drug_code Code of the drug product.
#'
#' @return Results of the API query using the supplied parameter(s).
#' @author Ezinne MM Ndukwe
#' @examples
#' therapeuticclass_api(drug_code = 3847)
#' @export
#'

therapeuticclass_api <- function(drug_code=NULL) {

  url <- "https://health-products.canada.ca/api/drug/therapeuticclass/?lang=en&type=json"

  therapeuticclass_api_query <- GET(url, query = list(id = drug_code))
  returned_therapeuticclass <- validate_api_response(therapeuticclass_api_query)

  return(returned_therapeuticclass)
}


#' @title Return data frame of results from Therapeutic Class API of the Drug Product Database
#' @description A function that queries the Therapeutic Class API using drug codes and returns a data frame of the results. Unlike [therapeuticclass_api()], vectors can be supplied to drug codes parameter to query the API with multiple values.
#' @import purrr
#' @importFrom Hmisc label
#' @import dplyr
#' @import glue
#'
#' @param drug_code Required. Code of the drug product. Can be string, numeric, vector or list of values. Cannot be NULL.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if any of the supplied drug code(s) is not in the API. If TRUE, alerts show. Use FALSE to hide.
#'
#' @return Data frame of the results. Columns are formatted to match the output of the Drug Product Database online query
#' @author Ezinne MM Ndukwe
#' @examples
#' search_therapeuticclass_api(drug_code = 3847)
#' @seealso [therapeuticclass_api()], [query_dpd_by_atc()]
#' @source <https://health-products.canada.ca/api/documentation/dpd-documentation-en.html>
#' @export

search_therapeuticclass_api <- function(drug_code, progress_bar=FALSE, alert_val_not_found=TRUE) {

  if (is.null(drug_code)){
    found_results <- therapeuticclass_api()
  } else{
  param_name <- 'drug_code'
  param_value <- drug_code

  therapeuticclass_results <- map(drug_code, therapeuticclass_api, .progress = progress_bar)
  names(therapeuticclass_results) <- param_value

  found_results <- output_valid_results(
    named_list = therapeuticclass_results,
    second_element_to_check = 'tc_atc_number',
    api_name = 'Therapeutic Class',
    name_source = param_name,
    alert_val_not_found = alert_val_not_found
  )
  }

  if (length(found_results) == 0){
    bound_therapeuticclass_results <- data.frame(
      setNames(list(param_value), param_name),
      tc_atc = NA,
      tc_atc_number = NA,
      anatomical_therapeutic_chemical = NA
    )

    Hmisc::label(bound_therapeuticclass_results$anatomical_therapeutic_chemical) <- "Anatomical Therapeutic Chemical (ATC)"
    Hmisc::label(bound_therapeuticclass_results$tc_atc_number) <- "Anatomical Therapeutic Chemical (ATC) number"
    Hmisc::label(bound_therapeuticclass_results$tc_atc) <- "Anatomical Therapeutic Chemical (ATC) name"

    message(glue("No therapeutic class information was found for supplied {param_name} values"))
    return(bound_therapeuticclass_results)

  }

  bound_therapeuticclass_results <- bind_rows(found_results)

  bound_therapeuticclass_results <- bound_therapeuticclass_results %>%
    mutate(anatomical_therapeutic_chemical = na.omit(paste(tc_atc_number, tc_atc, sep = ' ')))

  Hmisc::label(bound_therapeuticclass_results$anatomical_therapeutic_chemical) <- "Anatomical Therapeutic Chemical (ATC)"
  Hmisc::label(bound_therapeuticclass_results$tc_atc_number) <- "Anatomical Therapeutic Chemical (ATC) number"
  Hmisc::label(bound_therapeuticclass_results$tc_atc) <- "Anatomical Therapeutic Chemical (ATC) name"


  return(bound_therapeuticclass_results)

}
