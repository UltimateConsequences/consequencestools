#' Combine First and Last Names
#'
#' @param dataframe A dataframe containing 'dec_firstname' and 'dec_surnames' columns
#' @param unknown_string String to replace NA values (default: "?")
#'
#' @return A dataframe with an additional 'name' column
#' @export
#'
#' @examples
#' df <- data.frame(dec_firstname = c("John", NA), dec_surnames = c("Doe", "Smith"))
#' combine_names(df)
combine_names <- function(dataframe, unknown_string = "?"){
  dataframe <- dataframe %>%
    mutate(name = str_c(str_replace_na(dec_firstname, replacement = unknown_string),
                        str_replace_na(dec_surnames, replacement = unknown_string),
                        sep=" "))
  dataframe
}
