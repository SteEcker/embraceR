
# TODO: Make function



# library(dplyr)
# library(tidyr)
#
# emii <- load_embrace_ii() %>% add_disease_events()
#
# df <- emii %>%
#   filter(event_systemicfailure == 1) %>% select(latest_followup_id,
#                       contains("metastases"),
#                       contains("mediastinal_nodes"),
#                       contains("paraaortic_nodes_above_l2"),
#                       contains("supraclavicular_nodes"),
#                       contains("abdominal_carcinomatosis")
#                       ) %>%
#   select(-contains("text")) %>%
#   filter(!is.na(latest_followup_id))
#
#
#
# # Identify the unique metastasis types (assuming the naming convention is consistent)
# metastasis_types <- unique(gsub("_\\d+m", "", names(df)[-1]))
#
# # Create new columns for each metastasis type with the status at the latest followup
# for(metastasis_type in metastasis_types) {
#   # Dynamically create the column names to check based on the latest_followup_id
#   df <- df %>%
#     rowwise() %>%
#     mutate(!!paste0(metastasis_type, "_latest_status") := get(paste0(metastasis_type, "_", latest_followup_id, "m"))) %>%
#     ungroup()
# }
#
#
# df <- df %>% select(ends_with("_latest_status") ) %>% replace_neg_one_with_NA()
#
# names(df) <- gsub("_latest_status$", "", names(df))
#
# name_mapping <- c(
#   liver_metastases = "Liver Metastases",
#   bone_metastases = "Bone Metastases",
#   brain_metastases = "Brain Metastases",
#   lung_metastases = "Lung Metastases",
#   other_metastases = "Other Metastases",
#   mediastinal_nodes = "Mediastinal Nodes",
#   paraaortic_nodes_above_l2 = "Paraaortic Nodes Above L2",
#   supraclavicular_nodes = "Supraclavicular Nodes",
#   abdominal_carcinomatosis = "Abdominal Carcinomatosis"
# )
#
# names(df) <- name_mapping[names(df)]
#
# df %>% gtsummary::tbl_summary()

