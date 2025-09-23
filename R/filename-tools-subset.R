#' Generate protest domain filename
#'
#' @param protest_domain The protest domain name
#' @return The corresponding filename for the domain page,
#'         relative to the root of the website.
#' @export
#'
#' @examples
#' domain_filename("Rural land")
domain_filename <- function(protest_domain){
  # No files for overlapping domains like "Rural land, Partisan politics"
  name <- stringr::str_replace_all(protest_domain, "\\s+", "-") %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  filename <-  str_glue("/domain/{name}.html")
  return(if_else(stringr::str_detect(protest_domain, ","),
                 "", # No files for overlapping domains
                 filename
  ))
}

#' Generate Spanish domain filename
#'
#' @param protest_domain The protest domain name in Spanish
#' @return The corresponding filename for the Spanish domain page,
#'   relative to the root of the website.
#'
#' @examples
#' domain_filename_es("Campesino")
domain_filename_es <- function(protest_domain){
  name <- stringr::str_replace_all(protest_domain, "\\s+", "-") %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  filename <-  str_glue("/dominio/{name}.html")

  return(if_else(stringr::str_detect(protest_domain, ","),
                 "", # No files for overlapping domains
                 filename
  ))
}
