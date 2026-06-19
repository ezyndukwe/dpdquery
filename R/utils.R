#' Name and bind
#' @title Name and bind list
#' @description The name_and_bind function uses the names() and dplyr:bind_rows() functions to assigns names to lists (if names parameter is used) and bind list elements into a dataframe.
#'
#' @param list List (e.g., results of a mapped API query)
#' @param names Vector used to name the elements of a list or atomic vector
#' @param names_to Name of an optional identifier column. If value is supplied to names parameter, this parameter defaults to 'name'.
#'
#' @examples
#' #library(purrr)
#' #list_of_drugs <- c('semaglutide', 'insulin')
#' #api_result <- purrr::map(list_of_drugs, ~activeingredient_api(active_ingredient = .x))
#' #name_and_bind_list(api_result)
#' #name_and_bind_list(api_result, names = list_of_drugs, names_to = 'drug_class')
#' #name_and_bind_list(api_result, names = list_of_drugs) # Default name of identifier column is 'name'
#' #name_and_bind_list(api_result, names_to = 'drug_class')
#' @return Bound and named list
#' @export

name_and_bind_list <- function(list, names=NULL, names_to=NULL) {
  same_size <- length(unique(lengths(list))) == 1
  if (!same_size) {
    stop("List elements don't have the same size, so rows cannot be bound.")
  }

  if (is.null(names_to) & !is.null(names)) {names_to <- 'name'}

  names(list) <- names
  bound_list <- dplyr::bind_rows(list, .id = names_to)

  return(bound_list)
}

# Validate response of URL request from an API
#' @title Validate response of API call
#' @description
#' Prevents errors in API calls (i.e., querying for a value that doesn't exist, '404', etc....) from stopping the code from running.
#'
#' @param GET_response Response object from [GET()]
#' @seealso [GET()]

validate_api_response <- function(GET_response) {
  if(GET_response$status_code == 404){
    object <- list()
  } else{
    object <- jsonlite::fromJSON(content(GET_response, "text"))
  }
  return(object)
}

# Only output responses that are found. Use before name_and_bind_list()
#' @title Output (retain) valid responses of an API response
#' @description
#' This function evaluates the responses of an API call and retains the valid responses in the output.
#'
#' @param named_list The named list generated from an API call.
#' @param name_source Optional string to say what the names represent (e.g., "drug codes").
#' @param api_name Optional. Name of API. Used for alert if alert_val_not_found is TRUE.
#' @param alert_val_not_found Show alert if a value of the supplied search parameter is not in the list. If TRUE, alerts show (default). Use FALSE to hide.
#' @param second_element_to_check Second-level list element name (or nested list element name) as a string to check. If the value is null then element name is considered not found
#' @examples
#' #library(purrr)
#' #drug_codes_list <- c(0, 1, 8, 35413)
#' #api_results <- map(drug_codes_list, ~drugproduct_api(drug_code = .x))
#' #names(api_results) <- drug_codes_list
#' #output_valid_results(named_list = api_results, second_element_to_check = 'drug_identification_number', name_source = 'drug codes', api_name = 'Drug Products')


output_valid_results <- function(named_list, second_element_to_check=NULL, name_source=NULL, api_name=NULL, alert_val_not_found=TRUE) {

  if (is.null(name_source)){name_source <- ''}

  not_valid <- names(named_list)[lengths(named_list) == 0]

  if (!is.null(second_element_to_check)) {
    not_found <- names(named_list)[
      sapply(named_list, \(x) is.null(x[[second_element_to_check]]))
    ]
  } else {not_found <- character(0)}

  unavailable <- c(not_valid, not_found)
  unavailable <- unique(unavailable)

  if (alert_val_not_found==TRUE & length(unavailable) > 0){

    api_text <- if (!is.null(api_name) && !is.na(api_name)) {
      glue::glue(" in {api_name} API")
    } else {
      ""
    }

    message(glue("{name_source} values {toString(unavailable)} not found{api_text}"))

  }

  found_results <- named_list[!names(named_list) %in% unavailable]

  return(found_results)
}


# Query Product Status, Schedule, Route of Administration, Dosage Forms and Therapeutic Class APIs by drug
#' @title Query Product Status, Schedule, Route of Administration, Dosage Forms and Therapeutic Class APIs by drug code
#' @description
#' Query a group of APIs that can only be queried by drug code and return a data frame.
#'
#' @param drug_code Drug code (numeric, string or vector)
#' @param active_param Only return dosage forms, schedules and routes of administration that are active. Default is NULL. Use 'yes' to returns parameter values that have a date that is greater than today or no date.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default). Use FALSE to hide.
#' @param alert_val_not_found Show alert if a drug code is not in a component API. If TRUE, alerts show (default). Use FALSE to hide.
#'
#' @examples
#' drug_codes_list <- c(0, 1, 8, 35413)
#' use_drugcodes(drug_codes_list)
#' @export

use_drugCodes <- function(drug_code, active_param=NULL, progress_bar=TRUE, alert_val_not_found=TRUE){

  if (progress_bar == TRUE){
    message("Gathering product statuses")
  }
  status_search <- search_productstatus_api(
    drug_code = drug_code,
    progress_bar = progress_bar,
    alert_val_not_found = alert_val_not_found
  )

  if (progress_bar == TRUE){
    message("Gathering schedule")
  }
  schedule_search <- search_schedule_api(
    drug_code = drug_code,
    active_param = active_param,
    progress_bar = progress_bar,
    alert_val_not_found = alert_val_not_found
  )

  if (progress_bar == TRUE){
    message("Gathering route(s) of administration")
  }
  routeadminstration_search <- search_routeadminstration_api(
    drug_code = drug_code,
    active_param = active_param,
    progress_bar = progress_bar,
    alert_val_not_found = alert_val_not_found
  )

  if (progress_bar == TRUE){
    message("Gathering dosage form(s)")
  }
  dosage_search <- search_dosageform_api(
    drug_code = drug_code,
    active_param = active_param,
    progress_bar = progress_bar,
    alert_val_not_found = alert_val_not_found
  )

  if (progress_bar == TRUE){
    message("Gathering therapeutic class")
  }
  therapeuticclass_search <- search_therapeuticclass_api(
    drug_code = drug_code,
    progress_bar = progress_bar,
    alert_val_not_found = alert_val_not_found
  )

  query_using_drugcodes <- status_search %>%
    full_join(schedule_search, by = "drug_code", relationship = "many-to-many") %>%
    full_join(routeadminstration_search, by = "drug_code", relationship = "many-to-many")  %>%
    full_join(dosage_search, by = "drug_code", relationship = "many-to-many")%>%
    full_join(therapeuticclass_search, by = "drug_code", relationship = "many-to-many")%>%

    # Remove duplicates; keep first row
    distinct()

  return(query_using_drugcodes)
}
