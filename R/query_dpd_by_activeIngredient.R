#' @title Query DPD by active ingredient
#' @description A function that queries the entire DPD by active ingredient(s) and returns a data frame of the results.
#'
#' @param active_ingredient Search term for active ingredients. Can be string, or vector of terms as strings
#' @param active_param Only return dosage forms, schedules and routes of administration that are active. Default is NULL. Use 'yes' to returns parameter values that have a date that is greater than today or no date.
#' @param nest_additional_info If TRUE, some columns are hidden (nested) under column 'additional_info'. This is similar to how you need to click on the hyperlinked DIN to show more information for a drug product in the search results summary of the DPD online query.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default) with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if a drug code is not in a component API. If TRUE, alerts show (default). Use FALSE to hide.
#'
#' @author Ezinne MM Ndukwe
#' @examples
#' query_dpd_by_activeIngredient(active_ingredient = c('ozempic', 'SEMAGLUTIDE'), nest_additional_info = TRUE)
#' query_dpd_by_activeIngredient(active_ingredient = c('ozem', 'insulin'))
#' query_dpd_by_activeIngredient(active_ingredient = c('metamucil'), nest_additional_info = T)
#' @export
#'
#'
query_dpd_by_activeIngredient <- function(active_ingredient, active_param=NULL, nest_additional_info=FALSE, progress_bar = TRUE, alert_val_not_found = TRUE){

  dedup_ais <- unique(active_ingredient)

  if (progress_bar == TRUE){
    message("Querying Active Ingredients API for supplied active_ingredient values")
  }

  ai_query <- search_ai_api(
    active_ingredient = dedup_ais,
    return_search_terms = TRUE,
    summarize_drugs = FALSE,
    alert_val_not_found = alert_val_not_found,
    progress_bar = progress_bar
  )

  if (all(is.na(ai_query$drug_code))){
    return(ai_query)
    break
  }

  ai_query <- ai_query %>%
    select(search_term, drug_code)

  valid_drugcodes <- unique(ai_query$drug_code)

  query_w_dc <- query_dpd_by_drugCode(
    drug_code = valid_drugcodes,
    active_param = active_param,
    nest_additional_info = FALSE,
    progress_bar = progress_bar,
    alert_val_not_found = TRUE
  )

  ai_df <- data.frame(
    search_term = dedup_ais
  )

  query <- ai_df  %>%
    left_join(ai_query, by = "search_term", relationship = "many-to-many") %>%
    left_join(query_w_dc, by = "drug_code", relationship = "many-to-many") %>%

    # Remove duplicates; keep first row
    distinct() %>%

    arrange(!is.na(drug_code), drug_code)


  if (nest_additional_info == TRUE) {

    check_col <- c('current_status_date',
                   'original_market_date',
                   'description',
                   'dosage_forms',
                   'external_status_code',
                   'lot_number',
                   'expiration_date',
                   'ai_group_no',
                   'anatomical_therapeutic_chemical',
                   'tc_atc_number',
                   'tc_atc')

    for (col in check_col) {
      if (!col %in% names(query)) {
        query[[col]] <- NA
      }
    }

    query <- query %>%
      tidyr::nest(
        additional_info = c(
          current_status_date,
          original_market_date,
          description,
          dosage_forms,
          external_status_code,
          lot_number,
          expiration_date,
          ai_group_no,
          anatomical_therapeutic_chemical,
          tc_atc_number,
          tc_atc
        )
      )
  }


  return(query)
}
