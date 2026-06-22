# Product Status API -----------------------------------------------------------

#' @title Query Product Status API of the Drug Product Database
#' @description A function that queries Product Statuses API using drug codes.
#' @details
#' Use [search_productstatus_api()] if you wish to query the API using a vector of multiple drug codes.
#'
#' @param drug_code Code of the drug product
#' @return Results of the API query as a list.
#' @author Ezinne MM Ndukwe
#' @examples
#' productstatus_api(drug_code = 3847)
#' @source <https://health-products.canada.ca/api/documentation/dpd-documentation-en.html>
#' @export
#'

productstatus_api <- function(drug_code=NULL) {

  url <- "https://health-products.canada.ca/api/drug/status/?lang=en&type=json"

  productstatus_api_query <- GET(url, query = list(id = drug_code))
  returned_productstatus <- validate_api_response(productstatus_api_query)

  return(returned_productstatus)
}



#' @title Return data frame of results from Product Status API of the Drug Product Database
#' @description A function that queries the Product Status API using drug codes and returns a data frame of the results. Unlike productstatus_api(), vectors can be supplied to drug codes parameter to query the API with multiple values.
#'
#' @param drug_code Code of the drug product. Can be string, numeric, vector or list of values. Cannot bu NULL
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default) with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if any of the supplied drug code(s) is not in the API. If TRUE, alerts show (default). Use FALSE to hide.
#'
#' @return Dataframe of the results. Columns are formatted to match the output of the Drug Product Database online query.
#' @author Ezinne MM Ndukwe
#' @examples
#' search_productstatus_api(drug_code = 3847)
#' @seealso [productstatus_api()]
#' @export

search_productstatus_api <- function(drug_code, progress_bar=FALSE, alert_val_not_found=TRUE) {

  param_name <- 'drug_code'
  param_value <- drug_code

  productstatus_results <- map(drug_code, productstatus_api, .progress = progress_bar)
  names(productstatus_results) <- param_value

  found_results <- output_valid_results(
    named_list = productstatus_results,
    second_element_to_check = 'status',
    name_source = param_name,
    alert_val_not_found = alert_val_not_found,
    api_name = 'Product Status'
  )

  if (length(found_results) == 0){
    bound_productstatus_results <- data.frame(
      setNames(list(param_value), param_name),
      status = NA,
      current_status_date = NA,
      original_market_date = NA
    )

    message(glue("No product status information was found for supplied {param_name} values"))
    return(bound_productstatus_results)
  }

  bound_productstatus_results <- bind_rows(found_results)

  bound_productstatus_results <- bound_productstatus_results %>%
    rename(
      status = status,
      current_status_date = history_date,
      original_market_date = original_market_date,
    )

  return(bound_productstatus_results)

}
