
#' @title Query DPD by Anatomical Therapeutic Chemical (ATC) code
#' @description A function that queries the entire DPD by ATC and returns a data frame of the results.
#'
#' @param atc Anatomical Therapeutic Chemical (ATC) code (as string or vector of strings)
#' @param active_param Only return dosage forms, schedules and routes of administration that are active. Default is NULL. Use 'yes' to returns parameter values that have a date that is greater than today or no date.
#' @param nest_additional_info If TRUE, some columns are hidden (nested) under column 'additional_info'. This is similar to how you need to click on the hyperlinked DIN to show more information for a drug product in the search results summary of the DPD online query.
#' @param progress_bar Show progress bar. If TRUE, progress bar shows (default) with progress messages. Use FALSE to hide.
#' @param alert_val_not_found Show alert if a drug code is not in a component API. If TRUE, alerts show (default). Use FALSE to hide.

#'
#' @examples
#' query_dpd_by_atc(atc = c('R03DC03', 'B01AC27', '0'), nest_additional_info = TRUE)
#' query_dpd_by_atc(atc = c('R03DC03 ', '0'), nest_additional_info = TRUE) # adding space to 'R03DC03' to cause invalid value
#' @export
#'
#'
#'
#'
query_dpd_by_atc <- function(atc,
                         active_param = NULL,
                         nest_additional_info = FALSE,
                         progress_bar = TRUE,
                         alert_val_not_found = TRUE) {

  all_atc <- search_therapeuticclass_api(drug_code = NULL)

  dedup_atc <- unique(atc)
  select_atc <- all_atc %>%
    filter(tc_atc_number %in% dedup_atc)

  if (alert_val_not_found){
    not_valid_atc <- setdiff(dedup_atc, select_atc$tc_atc_number)
    not_valid_atc <- paste0('"', not_valid_atc, '"', collapse = ", ")
    message(glue("The following values did not match any ATC codes in the DPD: {toString(not_valid_atc)}"))
    }

  valid_drugcodes <- select_atc %>% pull(drug_code)
  if (length(valid_drugcodes) == 0){

    message("No products in the DPD matched the ATC criteria of your search")
    return(select_atc)

  }

  query_w_dc <- query_dpd_by_drugCode(
    drug_code = valid_drugcodes,
    active_param = active_param,
    nest_additional_info = FALSE,
    progress_bar = progress_bar,
    alert_val_not_found = alert_val_not_found,
    include_tc = FALSE
  )

  atc_df <- data.frame(
    tc_atc_number = dedup_atc,
    search_atc = dedup_atc
  )
  Hmisc::label(atc_df$tc_atc_number) <- "Anatomical Therapeutic Chemical (ATC) number"


  query <- query_w_dc  %>%
    full_join(select_atc, by = "drug_code", relationship = "many-to-many") %>%
    full_join(atc_df, by = "tc_atc_number", relationship = "many-to-many") %>%

    relocate(search_atc) %>%
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
