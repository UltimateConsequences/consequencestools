## Make a generic function to summarize deaths according to the columns
## in our Categorized Deaths table
utils::globalVariables(c("n", "n_coca", "n_armedactor", "n_state_perp",
                         "n_state_perp_coca", "n_state_perp_armedactor", "n_state_victim",
                         "n_state_victim_coca", "n_state_victim_armedactor", "n_state_separate",
                         "n_state_perp_ordinary", "n_state_victim_ordinary", "n_remaining",
                         "n_remaining_coca", "n_remaining_armedactor"))



#' Produce a Summary Table for Deaths by Responsibility and Major Domains
#'
#' Given a table of deaths `def`, these functions produce a summary table according
#' to a given variable `by` calculating the number of deaths in a variety
#' of situations.
#'
#' The simpler function `n_responsibility_by` considers only the overall total number,
#' and deaths that were perpetrated by the state, had state forces as victims, or
#' were separate from the state. It returns a five-column data table.
#'
#' The more complex function `n_categorized_by` subdivides deaths according to these categories,
#' as well as their involvement in the "Coca" protest domain, in the two domains
#' involving Armed Actors ("Guerrilla" and "Paramilitary"). This results in
#' a sixteen-column data table, where deaths outside those domains are
#' called Ordinary deaths.
#'
#' @param def A data frame detailing deaths
#' @param by A variable to summarize by
#' @param complete A logical value stating whether to fill in zero values
#' @param sp_binary A logical value. If TRUE, partial values for state_perpetrator
#'   (e.g., "Likely Yes") are counted.
#'
#' @return A data table.
#' @export
#'
#' @examples
#' n_categorized_by(deaths_aug24, pres_admin, complete=TRUE)
#' n_categorized_by(deaths_aug24, pres_admin, complete=TRUE)
#' n_responsibility_by(deaths_aug24, protest_domain, complete=TRUE)
n_categorized_by <- function(def, by, complete = FALSE, sp_binary = FALSE){
  if (sp_binary){
    def$state_perpetrator <- consequencestools::quasilogical_as_binary(def$state_perpetrator)
  }

  output <- def %>%
    dplyr::filter(!is.na({{by}})) %>%
    dplyr::filter({{by}} != "") %>%
    dplyr::group_by( {{ by }} ) %>%
    dplyr::summarize(
      n = dplyr::n(),
      n_coca = sum(str_detect(protest_domain, "Coca"), na.rm = TRUE),
      n_armedactor = sum(protest_domain %in% c("Guerrilla", "Paramilitary"), na.rm = TRUE),
      n_state_perp = sum(state_perpetrator == "Yes", na.rm = TRUE),
      n_state_perp_coca = sum(state_perpetrator == "Yes" & str_detect(protest_domain, "Coca"), na.rm = TRUE),
      n_state_perp_armedactor = sum(state_perpetrator == "Yes" & protest_domain %in% c("Guerrilla", "Paramilitary"), na.rm = TRUE),
      n_state_victim = sum(state_responsibility == "Victim", na.rm = TRUE),
      n_state_victim_coca = sum(state_responsibility == "Victim" & str_detect(protest_domain, "Coca"), na.rm = TRUE),
      n_state_victim_armedactor = sum(state_responsibility == "Victim" & protest_domain %in% c("Guerrilla", "Paramilitary"), na.rm = TRUE),
      n_state_separate = sum(str_detect(state_responsibility, "Separate"), na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      n_state_perp_ordinary = n_state_perp - n_state_perp_armedactor - n_state_perp_coca,
      n_state_victim_ordinary = n_state_victim - n_state_victim_armedactor - n_state_victim_coca,
      n_remaining = n - n_state_perp - n_state_victim,
      n_remaining_coca = n_coca - n_state_perp_coca - n_state_victim_coca,
      n_remaining_armedactor = n_armedactor - n_state_perp_armedactor - n_state_victim_armedactor
    )

  if (complete) {
    fill_values <- stats::setNames(rep(0, 15), c(
      "n", "n_coca", "n_armedactor", "n_state_perp", "n_state_perp_coca",
      "n_state_perp_armedactor", "n_state_perp_ordinary", "n_state_victim",
      "n_state_victim_coca", "n_state_victim_armedactor", "n_state_separate",
      "n_remaining", "n_remaining_coca", "n_remaining_armedactor"
    ))

    output <- output %>%
      tidyr::complete({{by}}, fill = as.list(fill_values))
  }

  return(output)
}

n_responsibility_by <- function(def, by, complete = FALSE){
  def <- assign_state_responsibility_levels(def, simplify = TRUE)

  output <- def %>%
    dplyr::filter(!is.na({{by}})) %>%
    dplyr::filter({{by}} != "") %>%
    dplyr::group_by( {{ by }} ) %>%
    dplyr::summarize(
      n = dplyr::n(),
      n_state_perp = sum(state_responsibility=="Perpetrator", na.rm = TRUE),
      n_state_victim = sum(state_responsibility=="Victim", na.rm = TRUE),
      n_state_separate = sum(stringr::str_detect(state_responsibility, "Separate"), na.rm = TRUE),
      .groups='drop'
    )

  if (complete) {
    fill_values <- stats::setNames(rep(0, 4), c(
      "n", "n_state_perp", "n_state_victim", "n_state_separate"
    ))

    output <- output %>%
      tidyr::complete({{by}}, fill = as.list(fill_values))
  }

  return(output)
}
