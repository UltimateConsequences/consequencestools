## Make a generic function to summarize deaths according to the columns
## in our Categorized Deaths table
quasilogical_as_binary <- function(datacolumn){
  datacolumn %>% forcats::fct_collapse(
    "Yes" = c("Yes", "Likely Yes", "Indirect", "Presumed Yes"),
    "No" = c("No", "Likely No", "Presumed No", "Likely Disarmed")) %>%
    suppressWarnings()
}

n_categorized_by <- function(def, by, complete = FALSE, sp_binary = FALSE){
  if (sp_binary){
    def$state_perpetrator <- quasilogical_as_binary(def$state_perpetrator)
  }

  output <- def %>%
    dplyr::filter(!is.na({{by}})) %>%
    dplyr::filter({{by}} != "") %>%
    dplyr::group_by( {{ by }} ) %>%
    dplyr::summarize(
      n = dplyr::n(), # all deaths
      n_coca = sum(str_detect(protest_domain, "Coca"), na.rm = TRUE), #coca deaths
      n_armedactor =sum((protest_domain=="Guerrilla") |
                          ( protest_domain == "Paramilitary"),
                        na.rm = TRUE), # armed actor death
      n_state_perp = sum(state_perpetrator=="Yes", na.rm = TRUE), #state perp
      n_state_perp_coca = sum(((state_perpetrator=="Yes") & (str_detect(protest_domain, "Coca"))), na.rm = TRUE), #state perp coca death
      n_state_perp_armedactor = sum((state_perpetrator=="Yes" & ((protest_domain=="Guerrilla") | ( protest_domain == "Paramilitary" ))), na.rm = TRUE), #state perp armed actor death,
      n_state_perp_ordinary = n_state_perp - n_state_perp_armedactor - n_state_perp_coca, # all other state perp deaths
      n_state_victim = sum(state_responsibility=="Victim", na.rm = TRUE), # state victim
      n_state_victim_coca = sum(((state_responsibility=="Victim") &
                                   (str_detect(protest_domain, "Coca"))), na.rm = TRUE), #state victim coca death
      n_state_victim_armedactor = sum(((state_responsibility=="Victim") &
                                         ((protest_domain=="Guerrilla") |
                                            (protest_domain == "Paramilitary"))), na.rm = TRUE), #state victim armed actor death
      n_state_victim_ordinary = n_state_victim - n_state_victim_armedactor - n_state_victim_coca, # all other state perp deaths
      n_state_separate = sum(str_detect(state_responsibility, "Separate"), na.rm = TRUE),
      n_remaining = n - n_state_perp - n_state_victim,
      n_remaining_coca = n_coca - n_state_perp_coca - n_state_victim_coca,
      n_remaining_armedactor = n_armedactor - n_state_perp_armedactor - n_state_victim_armedactor
    )

  if(complete){
    output <- output %>%
      tidyr::complete({{by}}, fill =list(
        n = 0,
        n_coca = 0,
        n_armedactor = 0,
        n_state_perp = 0,
        n_state_perp_coca = 0,
        n_state_perp_armedactor = 0,
        n_state_perp_ordinary = 0,
        n_state_victim = 0,
        n_state_victim_coca = 0,
        n_state_victim_armedactor = 0,
        n_state_perp_ordinary = 0,
        n_state_separate = 0,
        n_remaining = 0,
        n_remaining_coca = 0,
        n_remaining_armedactor = 0
      ))
  }

  return(output)
}

