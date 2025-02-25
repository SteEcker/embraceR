#' Save EMBRACE Data for EviGuide Analysis
#'
#' Extracts and processes a subset of EMBRACE data variables needed for EviGuide analysis.
#' This function loads combined EMBRACE-I and EMBRACE-II data, selects relevant clinical
#' and dosimetric variables, applies factor conversions, and saves the result as a CSV file.
#'
#' @param output_file Character, path where to save the CSV file (default: "clean_eviguide_em_data.csv")
#' @param quote Logical, whether to quote strings in the CSV output (default: TRUE)
#'
#' @return Invisibly returns the processed data frame
#' @keywords internal
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'   # Save with default filename
#'   save_eviguide_data()
#'   
#'   # Save with custom filename
#'   save_eviguide_data("my_eviguide_data.csv")
#' }
save_eviguide_data <- function(output_file = "clean_eviguide_em_data.csv", quote = TRUE) {

  em <- load_combined_embrace(return_common_columns = F) %>%
    replace_neg_one_with_NA()

  out <- em %>% select(
    centre_id = centre_id_sta_d,
    event = localfailureevent,
    time = timetoevent,

    t_imaging,
    histopathological_type_sta_d,

    fraction01hrctv_volume_tdvh,
    fraction01hrctv_volume_bins,
    fraction01residual_gtv_volume_tdvh,
    fraction01irctv_volume_tdvh,

    mri_cervix2_sta_d,
    mri_corpus_uteri_sta_d,
    mri_vagina_sta_d,
    ott,

    starts_with('total_'),

    icis,
    imp1image_max_parametrium_sta_b,
    bt_fractions_tdvh,

    gu_ntcp_filter,
    fistula_bleeding_cystits_g2_time,
    fistula_bleeding_cystits_g2_event,

    gi_ntcp_filter,
    bleeding_proctitis_g2_time,
    bleeding_proctitis_g2_event,

    urinary_incontinence_g2_time,
    urinary_incontinence_g2_event,

    vag_ntcp_filter,
    vagina_stenosis_g2_time,
    vagina_stenosis_g2_event,

    flatulence_g2_time,
    flatulence_g2_event,

    fraction01dose_rate_tdvh,
    trak_tandem_applicator_sum,
    trak_vaginal_applicator_sum,
    trak_needles_sum,
    trak_total_sum,
    study

    ) %>%
    embraceR::apply_guess_factor() %>%
    mutate(across(histopathological_type_sta_d, ~ recode_factor_levels(.x, c('1', '2', '3'), c('0', '1', '1'))))

  out %>% write.csv(file = output_file, quote = quote)
  
  invisible(out)
}


