# Search Route of Administration API -------------------------------------------

#' Search Route of Administration API
#'
#' @title Query Route of Administration API of the Drug Product Database
#' @description routeadminstration_api() queries the Route of Administration API using drug codes
#'
#' @param drug_code Code of the drug product
#' @param active_param Routes of administration that are active. Default is NULL. Use 'yes' to Returns the routes of administration that have a date that is greater than today or no date.
#'
#' @return Results of the API query using the supplied parameter(s).
#' @author Ezinne MM Ndukwe
#' @examples
#' routeadminstration_api(drug_code = 3847)
#' @export

routeadminstration_api <- function(drug_code, active_param=NULL) {

  url <- "https://health-products.canada.ca/api/drug/route/?lang=en&type=json"

  routeadminstration_api_query <- GET(url, query = list(id = drug_code, active=active_param))
  returned_routeadminstration <- validate_api_response(routeadminstration_api_query)

  return(returned_routeadminstration)
}


#' @title Return data frame of results from TRoute of Administration API of the Drug Product Database
#'
#' @param drug_code Code of the drug product.
#' @param active_param Routes of administration that are active. Default is NULL. Use 'yes' to Returns the routes of administration that have a date that is greater than today or no date.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if any of the supplied drug code(s) is not in the API. If TRUE, alerts show. Use FALSE to hide.
#'
#'
#' @returns Data frame of the results. Columns are formatted to match the output of the Drug Product Database online query.
#' @export
#' @examples
#' # search_routeadminstration_api(8)
#'
#'
search_routeadminstration_api <- function(drug_code, active_param = NULL, progress_bar=FALSE, alert_val_not_found=TRUE) {

  if (is.null(drug_code)) {
    stop("drug_code is required and cannot be NULL, NA or NaN")
  } else{
    param_name <- 'drug_code'
    param_value <- drug_code
  }

  routeadminstration_results <- map(
    drug_code,
    ~ routeadminstration_api(drug_code = .x, active_param = active_param),
    .progress = progress_bar
  )

  names(routeadminstration_results) <- param_value

  found_results <- output_valid_results(
    named_list = routeadminstration_results,
    second_element_to_check = "route_of_administration_code",
    api_name = 'Route of Administration',
    name_source = param_name,
    alert_val_not_found = alert_val_not_found
  )

  if (length(found_results) == 0){
    bound_routeadminstration_results <- data.frame(
      setNames(list(param_value), param_name),
      route_of_administration = NA
    )

    Hmisc::label(bound_routeadminstration_results$route_of_administration) <- "Route(s) of administration"

    message(glue("No routes of administration information was found for supplied {param_name} values"))
    return(bound_routeadminstration_results)

    break
  }

  bound_routeadminstration_results <- bind_rows(found_results) %>%
    group_by(drug_code) %>%
    summarise(
      route_of_administration = paste(route_of_administration_name, collapse = ", "),
      .groups = "drop"
    ) %>%
    ungroup()

  Hmisc::label(bound_routeadminstration_results$route_of_administration) <- "Route(s) of administration"

  return(bound_routeadminstration_results)
}
