#' Calculate number of rows matching a filter
#'
#' @param dataset A dataframe usable by dplyr
#' @param ... Other conditions to be passed to dplyr::filter
#'
#' @returns An integer count of the number of matching rows/observations
#' @export
#'
#' @examples
#' n_filter(dplyr::starwars, (species=="Droid") & height<120)
n_filter <- function(dataset, ...) {
  dataset %>%
    filter(...) %>%
    nrow()
}

#' Count observations in a specific municipality
#'
#' @param dataset A dataframe containing a "municipality" column
#' @param stated_municipality The municipality to filter by
#'
#' @return An integer count of rows matching the specified municipality
#' @export
#'
#' @examples
#' n_municipality(deaths_aug24, "Cochabamba")
n_municipality <- function(dataset, stated_municipality){
  n_filter(dataset, municipality == stated_municipality)
}
