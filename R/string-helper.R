#' Convert String to List Case
#'
#' List case here is defined as "Sentence case between commas,
#' With new capital letters only after, Each comma."
#'
#' @param string A character string to be converted
#'
#' @return A character string in list case format
#' @export
#'
#' @importFrom stringr str_replace str_to_sentence
#'
#' @examples
#' string_to_listcase("hello, world")
string_to_listcase <- function(string) {
  string %>%
    stringr::str_replace_all(",", ".") %>%
    str_to_sentence() %>%       # despite name, this lowercases everything except the first character
    capitalize_sentences() %>%
    stringr::str_replace_all("\\.", ",")
}


capitalize_sentences <- function(text) {
  # Split the text into sentences
  sentences <- strsplit(text, "(?<=\\.|\\?|\\!)\\s*", perl = TRUE)

  # Capitalize the first letter of the first word in each sentence
  capitalized_sentences <- sapply(sentences, function(sentence) {
    first_char <- substr(sentence, 1, 1)
    rest_of_sentence <- substr(sentence, 2, nchar(sentence))
    paste0(toupper(first_char), rest_of_sentence)
  })

  # Join the sentences back together
  paste(capitalized_sentences, collapse = " ")
}
