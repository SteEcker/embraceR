# library(dplyr)

# filter_patients <- function(df, basename, criteria) {
#   # Create a pattern for the relevant columns
#   pattern <- paste0("^", basename, ".*m$")

#   filtered_df <- df %>%
#     filter(if_any(matches(pattern), ~ . %in% criteria)) %>%
#     select(embrace_id, matches(pattern))

#   return(filtered_df)
# }
