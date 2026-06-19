# Dosage Form API -------------------------------------------

#' @title Query Dosage Form API of the Drug Product Database
#' @description This function queries the Dosage Form API using drug codes
#' Use [search_dosageform_api()] if you wish to query the API using multiple drug codes.
#'
#' @param drug_code Code of the drug product
#' @param active_param Dosage form(s) that are active. Default is NULL. Use 'yes' to return the dosage form(s) that have a date that is greater than today or no date.
#'
#' @return Response from API call as a list.
#' @author Ezinne MM Ndukwe
#' @examples
#' dosageform_api(drug_code = 3847)
#' @export
#' @seealso [GET()]
dosageform_api <- function(drug_code, active_param=NULL) {

  url <- "https://health-products.canada.ca/api/drug/form/?lang=en&type=json"

  dosageform_api_query <- GET(url, query = list(id = drug_code, active=active_param))
  returned_dosageform <- validate_api_response(dosageform_api_query)

  return(returned_dosageform)
}

#' @title Return data frame of results from Dosage Form API of the Drug Product Database
#' @description A function that queries the Dosage Form API and outputs a data frame. Unlike [dosageform_api()], vectors can be supplied to drug codes parameter to query the API with multiple values.
#'
#' @param drug_code Code of the drug product. Can be string, numeric, vector or list of values
#' @param active_param Dosage form(s) that are active. Default is NULL. Use 'yes' to return the dosage form(s) that have a date that is greater than today or no date.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default) with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if a value of the supplied search parameter is not in the API. If TRUE, alerts show (default). Use FALSE to hide.
#'
#' @return Data frame of the results. Columns are formatted to match the output of the Drug Product Database online query
#' @examples
#' search_dosageform_api(drug_code = 96906)
#' search_dosageform_api(drug_code = 96906, active_param = 'yes')
#' @seealso [dosageform_api()]
#' @export

search_dosageform_api <- function(drug_code, active_param=NULL, progress_bar=FALSE, alert_val_not_found=TRUE) {

  if (is.null(drug_code)) {
    stop("drug_code is required and cannot be NULL, NA or NaN")
  } else{
    param_name <- 'drug_code'
    param_value <- drug_code
  }

  dosageform_results <- map(drug_code, ~dosageform_api(drug_code = .x, active_param = active_param), .progress = progress_bar)
  names(dosageform_results) <- param_value

  found_results <- output_valid_results(
    named_list = dosageform_results,
    second_element_to_check = "pharmaceutical_form_code",
    api_name = 'Dosage Form',
    name_source = param_name,
    alert_val_not_found = alert_val_not_found
  )

  if (length(found_results) == 0){
    bound_dosageform_results <- data.frame(
      setNames(list(param_value), param_name),
      dosage_forms = NA
    )

    Hmisc::label(bound_dosageform_results$dosage_forms) <- "Dosage form(s)"

    return(bound_dosageform_results)

    message(glue("No results were found in Dosage Form API using supplied {param_name} values"))

    break
  }

  bound_dosageform_results <- bind_rows(found_results) %>%
    group_by(drug_code) %>%
    summarise(
      dosage_forms = paste(pharmaceutical_form_name, collapse = ", "),
      .groups = "drop"
    ) %>%
    ungroup()

  Hmisc::label(bound_dosageform_results$dosage_forms) <- "Dosage form(s)"

  return(bound_dosageform_results)
}
