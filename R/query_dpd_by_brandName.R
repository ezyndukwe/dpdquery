#' @title Query DPD by brand name
#' @description A function that queries the DPD by brand names(s) and returns a data frame of the results.
#'
#' @param brand_name Brand name (or product name) of the drug product. Can be string or vector of strings.
#' @param nest_additional_info If TRUE, some columns are hidden (nested) under column 'additional_info'. This is similar to how you need to click on the hyperlinked DIN to show more information for a drug product in the search results summary of the DPD online query.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default) with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if a drug code is not in a component API. If TRUE, alerts show (default). Use FALSE to hide.
#'
#' @author Ezinne MM Ndukwe
#' @examples
#' query_dpd_by_brandName(c('ozempic', 'metamucil'), nest_additional_info = TRUE)
#' @export
#'
#'
query_dpd_by_brandName <- function(brand_name, active_param=NULL, nest_additional_info=FALSE, progress_bar=TRUE, alert_val_not_found=TRUE){

  dedup_bns <- unique(brand_name)

  if (progress_bar == TRUE){
    message("Gathering drug products using supplied brand_name values")
  }
  dp_search <- search_dp_api(brand_name = dedup_bns, find_ais = TRUE, progress_bar = progress_bar, alert_val_not_found = alert_val_not_found)
  if (all(is.na(dp_search$company))){
    return(dp_search)
    break
  }
  dp_search <- dp_search %>%  rename(ai_name = list_AIs, strength = list_AIs_strength)

  valid_drugcodes <- unique(dp_search$drug_code)

  query_using_drugcodes <- use_drugCodes(
    drug_code = valid_drugcodes,
    active_param = active_param,
    progress_bar = progress_bar
  )


  bns_query <- dp_search %>%
    left_join(query_using_drugcodes, by = "drug_code", relationship = "many-to-many") %>%
    # Remove duplicates; keep first row
    distinct()

  bns_query <- bns_query %>%
    relocate(drug_code, status)

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
      if (!col %in% names(bns_query)) {
        bns_query[[col]] <- NA
      }
    }

    bns_query <- bns_query %>%
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

  return(bns_query)
}
