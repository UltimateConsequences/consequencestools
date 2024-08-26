## Make a generic function to summarize deaths according to the columns
## in our Categorized Deaths table
utils::globalVariables(c("n", "n_coca", "n_armedactor", "n_state_perp",
                         "n_state_perp_coca", "n_state_perp_armedactor", "n_state_victim",
                         "n_state_victim_coca", "n_state_victim_armedactor", "n_state_separate",
                         "n_state_perp_ordinary", "n_state_victim_ordinary", "n_remaining",
                         "n_remaining_coca", "n_remaining_armedactor"))

n_categorized_by <- function(def, by, complete = FALSE, sp_binary = FALSE){
  if (sp_binary){
    def$state_perpetrator <- quasilogical_as_binary(def$state_perpetrator)
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

