#' @title Query DPD by Drug Identification Number (DIN)
#' @description A function that queries the DPD by DIN(s) and returns a data frame of the results.
#'
#' @param drug_identification_number DIN of the drug product. Can be one DIN as string or vector of strings.
#' @param nest_additional_info If TRUE, some columns are hidden (nested) under column 'additional_info'. This is similar to how you need to click on the hyperlinked DIN to show more information for a drug product in the search results summary of the DPD online query.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default) with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if a DIN or drug code is not in a component API. If TRUE, alerts show (default). Use FALSE to hide.
#' @param active_param Only return dosage forms, schedules and routes of administration that are active. Default is NULL. Use 'yes' to returns parameter values that have a date that is greater than today or no date.
#'
#' @author Ezinne MM Ndukwe
#' @examples
#' query_dpd_by_din(drug_identification_number = '02416794')
#' query_dpd_by_din(drug_identification_number = c('0', '02416794'))
#' query_dpd_by_din(drug_identification_number = c('0')) # should come up blank
#' query_dpd_by_din(
#'   drug_identification_number = c('00446580', '53756', '90082', '89002'),
#'   active_param='yes'
#'   ) # Retrieve active dosage forms, schedules and routes of administration; these products are all canceled now
#' @export
#'
#'
#'
query_dpd_by_din <- function(drug_identification_number, active_param=NULL, nest_additional_info=FALSE, progress_bar=TRUE, alert_val_not_found=TRUE){

  dedup_dins <- unique(drug_identification_number)

  if (progress_bar == TRUE){
    message("Gathering drug products using supplied drug_identification_number values")
  }
  dp_search <- search_dp_api(drug_identification_number = dedup_dins, find_ais = TRUE, progress_bar = progress_bar, alert_val_not_found = alert_val_not_found)

  if (all(is.na(dp_search$company))){
    return(dp_search)
  }
  dp_search <- dp_search %>%  rename(ai_name = list_AIs, strength = list_AIs_strength)
  dp_search$drug_identification_number <- as.character(dp_search$din)

  valid_drugcodes <- unique(dp_search$drug_code)

  query_using_drugcodes <- use_drugCodes(
    drug_code = valid_drugcodes,
    active_param = active_param,
    progress_bar = progress_bar
  )

  dins_df <- data.frame(
    drug_identification_number = dedup_dins
  )

  dins_query <- dins_df  %>%
    left_join(dp_search, by = "drug_identification_number", relationship = "many-to-many") %>%
    left_join(query_using_drugcodes, by = "drug_code", relationship = "many-to-many") %>%
    rename(searched_drug_identification_number = drug_identification_number) %>%
    # Remove duplicates; keep first row
    distinct()

  dins_query <- dins_query %>%
    relocate(searched_drug_identification_number, drug_code, status)

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

    dins_query <- dins_query %>%
      nest(
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

  return(dins_query)
}
