# Drug Products API ------------------------------------------------------

#' @title Query Drug Product API of the Drug Product Database
#' @description A function that queries Drug Product API using drug code, DIN, brand name, and  status.
#' Results can be unpredictable if drug code, DIN and brand name search criteria (arguments) are used in conjunction. Recommended to use only one.
#' The status argument can be used safely in combination with any of the other criteria
#'
#' @param drug_code Code of the drug product.
#' @param brand_name Brand name of the drug product.
#' @param drug_identification_number Drug Identification Number (DIN) of the drug product as a string.
#' @param status Drug product status code. See [status_mapping] dataset for the description of each status code.
#'
#' @seealso
#' \itemize{
##'  \item [search_dp_api()], [query_dpd_by_din()], [query_dpd_by_brandName()]
##'  \item [status_mapping]
##' }
#' @return Response from API call as a list. Information on DIN, brand name, company name, class name, therapeutic class name, Active Ingredient Group number and number of active ingredients, last date updated and additional details of drug product.
#' @author Ezinne MM Ndukwe
#' @examples
#' drugproduct_api() # all drug products in the DPD
#' drugproduct_api(drug_code = 3847)
#' drugproduct_api(drug_identification_number = '00446564')
#' drugproduct_api(brand_name = 'apidra')
#' drugproduct_api(brand_name = 'apidra', status = 3) # Find Apidra products cancelled pre-market
#' @export

drugproduct_api <- function(drug_code=NULL, drug_identification_number=NULL, brand_name=NULL, status=NULL) {

  if(is.numeric(drug_identification_number)){
    stop("DIN should be a string")
  }

  url <- "https://health-products.canada.ca/api/drug/drugproduct/?lang=en&type=json"

  drugproduct_api_query <- GET(url,
                               query = list(
                                 id = drug_code,
                                 din = drug_identification_number,
                                 brandname = brand_name,
                                 status = status
                               ))
  drugproduct_results <- validate_api_response(drugproduct_api_query)

  return(drugproduct_results)
}

# search_dp_api --------------------------------------------------------------

#' @title Return data frame of results from Drug Product API of the Drug Product Database
#' @description A function that queries Drug Product API using drug codes, DINs or brand names and returns a data frame of the results.
#' Unlike [drugproduct_api()], vectors can be supplied to search parameters to query the API with multiple values.
#' @details
#' Results can be unpredictable if more than one search criteria (drug code, DIN and brand name) are used. Only one use one argument; the rest should be NULL.
#'
#' @note The status parameter only accepts a single code (numeric or string) or NULL. To avoid errors, please call function without the status argument, then use [search_productstatus_api()] to retrieve status codes for identified drug codes, and filter results after merging.
#'
#'
#' @param drug_code Code of the drug product. Can be string, numeric, vector or list of values
#' @param drug_identification_number Drug Identification Number (DIN) of the drug product. Can be string or vector of strings.
#' @param brand_name Brand name (or product name) of the drug product. Can be string or vector of strings.
#' @param status Drug product status code. See [status_mapping] dataset for the description of each status code.

#' @param find_ais Search DPD to find all active ingredients of the identified drug product. Default is TRUE to find.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if a value of the supplied search parameter is not in the API. If TRUE, alerts show (default). Use FALSE to hide.
#'
#' @seealso [drugproduct_api()], [query_dpd_by_din()], [query_dpd_by_brandName()]
#' @return Dataframe of the API results. Columns are formatted to match the output of the Drug Product Database online query
#' @author Ezinne MM Ndukwe
#' @examples
#' search_dp_api(drug_code = 3847)
#' search_dp_api(drug_code = c("90140", "3847"))
#' search_dp_api(drug_identification_number = c("00446564"))
#' search_dp_api(brand_name = c("Ozempic"))
#' search_dp_api(brand_name = "tobradex") # not case-sensitive
#' search_dp_api(brand_name = "tobradex", find_ais = F) # no information returned about active ingredients
#' search_dp_api(drug_code = c(2,3), status = c(1)) # Drug product codes 2 and 3 are not currently approved
#' # Get drug product info for all drug products in DPD
#' # search_dp_api()
#' @export

search_dp_api <- function(drug_code=NULL, drug_identification_number=NULL, brand_name=NULL, find_ais=TRUE, status= NULL, progress_bar=TRUE, alert_val_not_found=TRUE) {

  if (length(status) > 1) {
    stop("Please enter 1 status code or NULL to the status argument.")
  }

  # Only 1 param should not be NULL
  dp_params <- list(
    drug_code = drug_code,
    drug_identification_number = drug_identification_number,
    brand_name = brand_name
  )
  non_null_count <- sum(!sapply(dp_params, is.null))

  if (non_null_count >= 2) {
    stop("Only supply 1 parameter (drug_code, drug_identification_number, brand_name). The rest should be NULL.")
  }

  if(is.numeric(drug_identification_number)){
    stop("DIN value should be a string")
  }

  if (non_null_count == 0) {

    param_name <- 'drug_code'
    if (progress_bar == TRUE){
      if (find_ais == FALSE) {message('Querying Drug Product API')}
      if (find_ais == TRUE) {message('1. Querying Drug Product API')}
    }
    found_results <- drugproduct_api(drug_code = NULL, status = status)

    } else {

      if (non_null_count == 1) {
        param_name <- names(dp_params)[!map_lgl(dp_params, is.null)][1]
        param_value <- dp_params[[param_name]]


        if (progress_bar == TRUE){
          if (find_ais == FALSE) {message('Querying Drug Product API')}
          if (find_ais == TRUE) {message('1. Querying Drug Product API')}
        }


        drugproduct_results <- purrr::map(
          param_value,
          ~ do.call(
            drugproduct_api,
            c(setNames(list(.x), param_name),
              list(status = status))
          ),
          .progress = progress_bar
        )

        names(drugproduct_results) <- param_value}

    found_results <- output_valid_results(
      named_list = drugproduct_results,
      second_element_to_check = "drug_identification_number",
      name_source = param_name,
      api_name = 'Drug Product',
      alert_val_not_found = alert_val_not_found
    )

  }



  # if (length(found_results) == 0){
  #   stop(glue("No results were found in Drug Product Database using supplied {param_name} values"))
  # }

  if (length(found_results) == 0){
    bound_drugproduct_results <- data.frame(
      setNames(list(param_value), param_name)
    )

    check_col <- c('drug_code',
                   'drug_identification_number',
                   'brand_name',
                   'descriptor',
                   'company_name',
                   'class_name',
                   'number_of_ais',
                   'ai_group_no')

    for (col in check_col) {
      if (!col %in% names(bound_drugproduct_results)) {
        bound_drugproduct_results[[col]] <- NA
      }
    }

    bound_drugproduct_results <- bound_drugproduct_results %>%
      rename(
        din = drug_identification_number,
        description= descriptor,
        product = brand_name,
        company = company_name,
        class = class_name
      )

    Hmisc::label(bound_drugproduct_results$number_of_ais) <- "#"
    Hmisc::label(bound_drugproduct_results$din) <- "Drug identification number"
    Hmisc::label(bound_drugproduct_results$ai_group_no) <- "Active ingredient group (AIG) number"
    Hmisc::label(bound_drugproduct_results$product) <- "Product name / Brand name"
    bound_drugproduct_results <- bound_drugproduct_results %>%
      relocate(drug_code, din, company, product, class)

    message(glue("No results were found in Drug Product API using supplied {param_name} values"))
    return(bound_drugproduct_results)
  }

  bound_drugproduct_results <- dplyr::bind_rows(found_results)
  bound_drugproduct_results <- bound_drugproduct_results %>%
    rename(
      din = drug_identification_number,
      description= descriptor,
      product = brand_name,
      company = company_name,
      class = class_name
    )

  Hmisc::label(bound_drugproduct_results$number_of_ais) <- "#"
  Hmisc::label(bound_drugproduct_results$din) <- "Drug identification number"
  Hmisc::label(bound_drugproduct_results$ai_group_no) <- "Active ingredient group (AIG) number"
  Hmisc::label(bound_drugproduct_results$product) <- "Product name / Brand name"
  bound_drugproduct_results <- bound_drugproduct_results %>%
    relocate(drug_code, din, company, product, class)

  if (find_ais == FALSE){


    final_results <- bound_drugproduct_results
  } else{

    if(progress_bar == TRUE){
      message('2. Querying Active Ingredients API for full list of active ingredients')
    }

    # Find all the drug products that are made with 2+ active ingredients
    prod_drugcodes <- unique(bound_drugproduct_results$drug_code)

    ais_query <-
      map(
        prod_drugcodes,
        ~ search_ai_api(
          active_ingredient = NULL,
          drug_code = .x,
          summarize_drugs = T,
          return_search_terms = F),
        .progress = progress_bar
      ) %>%
      bind_rows()

    list_ai_name_strength <- ais_query %>%
      rename(
        list_AIs = ai_name,
        list_AIs_strength = strength
      )

    ## Attach list of active ingredients to bound_drugproduct_results
    final_results <- bound_drugproduct_results %>%
      left_join(list_ai_name_strength, by = "drug_code")
  }

  return(final_results)
}
