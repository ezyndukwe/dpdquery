# Schedule API -----------------------------------------------------------

#' @title Query Schedule API of the Drug Product Database
#' @description A function that queries Schedule API using drug codes
#' @details
#' Use [search_schedule_api()] if you wish to query the API using a vector of multiple drug codes.
#' @import httr
#'
#' @param drug_code Code of the drug product. Can be string, numeric, vector or list of values
#' @param active_param Default is NULL. Use 'yes' to returns the schedule that has a date that is greater than today or no date.
#'
#' @return Results of the API query using the supplied parameter(s).
#' @author Ezinne MM Ndukwe
#' @examples
#' schedule_api(drug_code = 3847)
#' search_schedule_api(drug_code = list(8, 9))
#' schedule_api(drug_code = 3847, active_param='yes')
#' @export
#'
schedule_api <- function(drug_code=NULL, active_param=NULL) {

  url <- "https://health-products.canada.ca/api/drug/schedule/?lang=en&type=json"

  schedule_api_query <- GET(url, query = list(id = drug_code, active=active_param))
  returned_schedule <- validate_api_response(schedule_api_query)

  return(returned_schedule)
}


#' @title Return data frame of results from Schedule API query
#' @description A function that queries the Schedule API using drug codes and returns a data frame of the results. Unlike schedule_api(), vectors can be supplied to the drug codes parameter to query the API with multiple values.
#' @details
#' drug_code is a required parameter for this function. Use [schedule_api()] if you wish to return all possible drug codes in the API.
#'
#' @param drug_code Code of the drug product. Can be string, numeric, vector or list of values
#' @param active_param Default is NULL. Use 'yes' to returns the schedule that has a date that is greater than today or no date.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default) with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if any of the supplied drug code(s) is not in the API. If TRUE, alerts show (default). Use FALSE to hide.
#'
#' @return Data frame of the results. Columns are formatted to match the output of the Drug Product Database online query.
#' @author Ezinne MM Ndukwe
#' @examples
#' search_schedule_api(drug_code = 3847)
#' @seealso [schedule_api()]
#' @export

search_schedule_api <- function(drug_code, active_param=NULL, progress_bar=FALSE, alert_val_not_found=TRUE) {

  if (is.null(drug_code) || length(drug_code) == 0 ||  base::anyNA(drug_code)) {
    stop("drug_code is required and cannot be missing")
  }

  param_name <- 'drug_code'
  param_value <- drug_code

  schedule_results <- map(drug_code,
                          ~ schedule_api(drug_code = .x, active_param = active_param),
                          .progress = progress_bar)

  names(schedule_results) <- param_value

  found_results <- output_valid_results(
    named_list = schedule_results,
    second_element_to_check = "schedule_name",
    name_source = param_name,
    alert_val_not_found = alert_val_not_found,
    api_name = 'Schedule'
  )

  if (length(found_results) == 0){
    bound_schedule_results <- data.frame(
      setNames(list(param_value), param_name),
      schedule = NA
    )

    message(glue("No schedule information was found for supplied {param_name} values"))
    return(bound_schedule_results)

    break
  }

  bound_schedule_results <- bind_rows(schedule_results) %>%
    group_by(drug_code) %>%
    summarise(
      schedule = paste(schedule_name, collapse = ", "),
      .groups = "drop"
    ) %>%
    ungroup()

  return(bound_schedule_results)
}
