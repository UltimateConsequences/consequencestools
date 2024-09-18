# This function produces a single name string
combine_names <- function(dataframe, unknown_string = "?"){
  dataframe <- dataframe %>%
    mutate(name = str_c(str_replace_na(dec_firstname, replacement = unknown_string),
                        str_replace_na(dec_surnames, replacement = unknown_string),
                        sep=" "))
  dataframe
}
