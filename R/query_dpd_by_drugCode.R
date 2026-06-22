#' @title Query Drug Product Database by drug code
#' @description A function that queries the DPD by drug codes(s) and returns a data frame of the results.

#' @param drug_code Code of the drug product. Can be string, numeric, vector or list of values
#'
#' @param active_param Only return dosage forms, schedules and routes of administration that are active. Default is NULL. Use 'yes' to returns parameter values that have a date that is greater than today or no date.
#' @param nest_additional_info If TRUE, additional columns are hidden (nested) under column 'additional_info'.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default). Use FALSE to hide.
#' @param alert_val_not_found Show alert if a drug code is not in a component API. If TRUE, alerts show (default). Use FALSE to hide.
#' @param include_tc Include therapeutic class information (e.g., Anatomical Therapeutic Chemical Code).
#'
#' @examples
#' # query_dpd_by_drugCode(96058)
#' # query_dpd_by_drugCode(c(53756), active_param='yes') # Cancelled post-market
#' @export
#'

query_dpd_by_drugCode <- function(drug_code, active_param=NULL, nest_additional_info=FALSE, progress_bar=TRUE, alert_val_not_found=TRUE, include_tc = TRUE){

  dedup_drugcodes <- unique(drug_code)

  if (progress_bar == TRUE){
    message("Gathering drug products")
  }
  dp_search <- search_dp_api(
    drug_code = dedup_drugcodes,
    find_ais = TRUE,
    progress_bar = progress_bar,
    alert_val_not_found = alert_val_not_found
  )

  if (all(is.na(dp_search$company))){
    return(dp_search)
  }

  dp_search <- dp_search %>%
    rename(ai_name = list_AIs, strength = list_AIs_strength)

  valid_drugcodes <- unique(dp_search$drug_code)

  query_using_drugcodes <- use_drugCodes(
    drug_code = valid_drugcodes,
    active_param = active_param,
    progress_bar = progress_bar,
    alert_val_not_found = TRUE,
    include_tc = include_tc
  )

  dc_df <- data.frame(drug_code = as.numeric(dedup_drugcodes))

  query <- dc_df  %>%
    left_join(dp_search, by = "drug_code", relationship = "many-to-many") %>%
    left_join(query_using_drugcodes, by = "drug_code", relationship = "many-to-many") %>%
    # Remove duplicates; keep first row
    distinct()

  query <- query %>%
    relocate(drug_code, status)


  if (nest_additional_info == TRUE) {

    check_col <- c(
      "current_status_date",
      "original_market_date",
      "description",
      "dosage_forms",
      "external_status_code",
      "lot_number",
      "expiration_date",
      "ai_group_no"
    )

    if (include_tc) {
      check_col <- c(
        check_col,
        "anatomical_therapeutic_chemical",
        "tc_atc_number",
        "tc_atc"
      )
    }

    for (col in check_col) {
      if (!col %in% names(query)) {
        query[[col]] <- NA
      }
    }

    query <- query %>%
      nest(additional_info = all_of(check_col))

  }

  return(query)
}
