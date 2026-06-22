# Active Ingredients API ------------------------------------------------------

#' @title Query Active Ingredient API of the Drug Product Database by
#' @description
#' A function that queries the Active Ingredient API of the DPD using drug codes or search terms of active ingredients.
#' Use [search_ai_api()] if you wish to query the API using a vector of multiple values.
#'
#' @param active_ingredient Active ingredient (string or vector). Enter term just as you would in the online DPD query (not case-sensitive). Only supply one term.
#' @param drug_code Code of the drug product.
#'
#' @seealso [search_ai_api()], [query_dpd_by_activeIngredient()]
#' @return Response from API call as a list. Information on the names and strengths of queried active ingredients or active ingredients in queried drug products.
#' @examples
#' # activeingredient_api() # returns all values in API
#' # activeingredient_api('semaglutide', drug_code=NULL)
#' # library(purrr)
#' # map(c('semaglutide', 'metformin'), activeingredient_api)
#' @author Ezinne MM Ndukwe
#' @export


activeingredient_api <- function(active_ingredient=NULL, drug_code=NULL) {

  ai_res <- httr::GET(
    url = "https://health-products.canada.ca/api/drug/activeingredient/?lang=en&type=json",
    query = list(ingredientname = active_ingredient, id = drug_code)
  )
  results <- validate_api_response(ai_res)

  return(results)
}

#' @title Return data frame of results from Active Ingredients API query of the Drug Product Database
#' @description A function that queries the Active Ingredients API and outputs a data frame. Unlike [activeingredient_api()], vectors can be supplied to drug codes or active ingredients parameters to query the API with multiple values.
#' One of drug codes or active ingredients need to be non-null. The two parameters should not be used in combination as the API behaves unpredictably.
#' Run [activeingredient_api()] if you wish to return all possible entries in the API.
#'
#' @param active_ingredient Active ingredient(s) to search in the API. Can be string or vector of strings.
#' @param drug_code Code of the drug product. Can be string, numeric, vector or list of values
#' @param return_search_terms Retain column of the supplied active ingredients terms in the output data frame.
#' @param summarize_drugs Summarize all found active ingredients for the drug product in one row. Search the Active Ingredients API by drug code to ensure all active ingredients in the drug product are found.
#' @param progress_bar Show progress bar. If TRUE (default), progress bar shows with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if a value of the supplied search parameter is not in the API. If TRUE (default), alerts show. Use FALSE to hide.
#'
#' @return Data frame of the results. Columns are formatted to match the output of the Drug Product Database online query.
#' @examples
#' search_ai_api(c('metformin', 'lixisenatide', 'sitagliptin'))
#' search_ai_api(drug_code = 96906)
#' search_ai_api(drug_code = 96906, summarize_drugs=FALSE)
#' @seealso [activeingredient_api()], , [query_dpd_by_activeIngredient()]
#' @export


search_ai_api <- function(active_ingredient=NULL, drug_code=NULL, return_search_terms = TRUE, summarize_drugs=TRUE, progress_bar=FALSE, alert_val_not_found=TRUE) {

  if(!is.null(active_ingredient) & !is.null(drug_code)) {
    stop("ai_tem and drug_code values cannot both be supplied. Only supply 1")
  }

  # Only 1 param should not be NULL
  ai_params <- list(
    active_ingredient = active_ingredient,
    drug_code = drug_code
  )
  non_null_count <- sum(!sapply(ai_params, is.null))

  if (non_null_count >= 2) {
    stop("Only supply 1 parameter: drug_code or active_ingredient")
  } else  if (non_null_count == 0) {
    stop("One of the following parameters is required to query the Active Ingredients API: drug_code, active_ingredient")
  }

  param_name <- names(ai_params)[!map_lgl(ai_params, is.null)][1]
  param_value <- ai_params[[param_name]]

  activeingredients_results <- map(
    param_value,
    ~ do.call(activeingredient_api, setNames(list(.x), param_name)),
    .progress = progress_bar
  )

  names(activeingredients_results) <- param_value

  valid_results <- output_valid_results(
    named_list = activeingredients_results,
    second_element_to_check = 'ingredient_name',
    name_source = param_name,
    alert_val_not_found = alert_val_not_found,
    api_name = 'Active Ingredients'
  )


  if (length(valid_results) == 0){
    bound_activeingredients_results <- data.frame(
      setNames(list(param_value), param_name)
    )

    check_col <- c('active_ingredient',
                   'drug_code',
                   'ingredient_name',
                   'ai_strength'
    )

    for (col in check_col) {
      if (!col %in% names(bound_activeingredients_results)) {
        bound_activeingredients_results[[col]] <- NA
      }
    }

    bound_AI_w_strength <- bound_activeingredients_results %>%
      select(searched_active_ingredient=active_ingredient, drug_code, ai_name = ingredient_name, strength = ai_strength)

    if (param_name != 'active_ingredient') {
      bound_activeingredients_results <- bound_activeingredients_results %>% select(-searched_active_ingredient)
    }

    Hmisc::label(bound_AI_w_strength$ai_name) <- "A.I. name"
    Hmisc::label(bound_AI_w_strength$strength) <- "Strength"

    message(glue::glue("No results were found in the Active Ingredients API using supplied {param_name} values"))
    return(bound_AI_w_strength)
  }


  if(!is.null(active_ingredient) & summarize_drugs == TRUE){
    warning("Summarized active ingredients and strengths may not include full list of active ingredients in the drug product. Search by drug codes instead to ensure all active ingredients are identified")
  }

  bound_activeingredients_results <- bind_rows(valid_results, .id = 'search_term')

  bound_AI_w_strength <- bound_activeingredients_results %>%
    rename(strength_num = strength) %>%
    mutate(strength_value = paste(strength_num, strength_unit)) %>%
    rowwise() %>%
    mutate(ai_strength ={
      if (dosage_unit == '') {
        strength_value
      } else { # Only show the '/' deliminator if dosage_unit is not blank
        paste(c(strength_value, dosage_unit), collapse = " / ")
      }
    }) %>%
    select(search_term, drug_code, ai_name = ingredient_name, strength = ai_strength)

  if (summarize_drugs == TRUE){
    bound_AI_w_strength <- bound_AI_w_strength %>%
      group_by(drug_code) %>%
      dplyr::summarise(
        search_term = paste(search_term , collapse = ", "),
        ai_name = paste(ai_name , collapse = " + "),
        strength = paste(strength, collapse = " + "),
        .groups = "drop"
      ) %>%
      relocate(search_term) %>%
      ungroup()
  }

  if (return_search_terms == FALSE){
    bound_AI_w_strength <- bound_AI_w_strength[, !colnames(bound_AI_w_strength) %in% "search_term"]
  }

  Hmisc::label(bound_AI_w_strength$ai_name) <- "A.I. name"
  Hmisc::label(bound_AI_w_strength$strength) <- "Strength"
  return(bound_AI_w_strength)
}
