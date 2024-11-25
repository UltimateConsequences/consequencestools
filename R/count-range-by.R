utils::globalVariables(c("department", "n", "n_unfiltered", "n_state_perp",
                         "n_state_perp_hi", "n_state_victim", "n_state_victim_hi",
                         "n_state_separate", "n_state_separate_hi",
                         "n_unconfirmed", "n_collateral", "n_nonconflict"))


#' Produce a Range Summary Table for Deaths by Responsibility
#'
#' Given a table of deaths `deaths` and its unfiltered version `deaths_unfiltered`,
#' this function produces a summary table according to a given variable `by`,
#' calculating the range (low and high estimates) of the number of deaths where
#' the state was a perpetrator, victim, or separate from the incident.
#'
#' The low estimates are based on the filtered `deaths` table, while the high
#' estimates are derived from the unfiltered `deaths_unfiltered` table, which
#' includes additional categories such as unconfirmed deaths, collateral deaths,
#' and non-conflict deaths.
#'
#' @param deaths A data frame detailing confirmed and filtered deaths
#' @param deaths_unfiltered A data frame detailing all reported deaths,
#'   including unconfirmed ones
#' @param by A variable to summarize by
#' @param drop_separate A logical value indicating whether to exclude the
#'   "separate from state" category
#' @param drop_extra A logical value indicating whether to exclude extra
#'   columns (e.g., unconfirmed, collateral, non-conflict deaths)
#' @param blank_hi A logical value indicating whether to replace high estimates
#'   with NA if they are the same as the low estimates
#'
#' @return A data table with low and high estimates for each category of
#' state responsibility. In the default state, these are the index variable
#' supplied by "by" followed by "n", "n_state_perp", "n_state_perp_hi",
#' "n_state_victim", "n_state_victim_hi". When drop_separate is FALSE,
#' "n_state_separate" and "n_state_separate_hi" are also returned. When
#' drop_extra is false, "n_unfiltered", "n_unconfirmed", "n_collateral",
#' and "n_nonconflict" are also returned.
#' @export
#'
#' @examples
#' deaths_aug24_filtered <- standard_filter(deaths_aug24)
#' deaths_aug24_unfiltered <- deaths_aug24
#' count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, protest_domain)
#' count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department)
count_range_by <- function(deaths, deaths_unfiltered, by,
                           drop_separate=FALSE, drop_extra=FALSE,
                           blank_hi = TRUE){
  counts <- deaths %>% dplyr::filter(!is.na({{by}})) %>%
    dplyr::filter({{by}} != "") %>%
    group_by( {{ by }} ) %>%
    summarize(
      n = dplyr::n(),
      n_state_perp = sum(state_perpetrator=="Yes", na.rm = TRUE),
      n_state_victim = sum(str_detect(state_responsibility, "^State victim") &      # detect State victim only at beginning of string
                             !str_detect(intentionality, "cident"), na.rm = TRUE),
      n_state_separate = sum(str_detect(state_responsibility, "Separate from state"), na.rm = TRUE),
      .groups = "drop"
    )

  counts_unfiltered <- deaths_unfiltered %>%
    dplyr::filter(!is.na({{by}})) %>%
    dplyr::filter({{by}} != "") %>%
    group_by( {{ by }} ) %>%
    summarize(
      n_unconfirmed = sum(unconfirmed == TRUE, na.rm = TRUE),
      n_collateral = sum(intentionality == "Collateral", na.rm = TRUE),
      n_nonconflict = sum(str_detect(intentionality, "Nonconflict"), na.rm = TRUE),
      n_state_perp_hi = sum(state_perpetrator %in% c("Yes", "Disputed", "Likely Yes", "Presumed Yes", "Indirect"), na.rm = TRUE),
      n_state_victim_hi = sum(str_detect(state_responsibility, "^State victim"), na.rm = TRUE),
      n_state_separate_hi = sum(str_detect(state_responsibility, "Separate from state"), na.rm = TRUE),
      n_unfiltered = dplyr::n(),
      .groups = "drop"
    )

  # merge the event/campaign table with the corresponding data from the unfiltered counts
  counts_joined <- counts %>%
    left_join(counts_unfiltered) %>%
    relocate(n_state_perp_hi, .after=n_state_perp) %>%
    relocate(n_state_victim_hi, .after=n_state_victim) %>%
    relocate(n_state_separate_hi, .after=n_state_separate) %>%
    relocate(n_unfiltered, .after = n) %>% # n_unfiltered in the "high" value of n
    # but our standard option is to drop it below
    suppressMessages()

  # blank the high values if they are the same as the low values…
  if(blank_hi){
    counts_joined <- counts_joined %>%
      mutate(n_state_perp_hi = ifelse(n_state_perp_hi==n_state_perp, NA, n_state_perp_hi)) %>%
      mutate(n_state_victim_hi = ifelse(n_state_victim_hi==n_state_victim, NA, n_state_victim_hi)) %>%
      mutate(n_state_separate_hi = ifelse(n_state_separate_hi==n_state_separate, NA, n_state_separate_hi))
  }

  if(drop_separate){
    counts_joined <- counts_joined %>% select(-n_state_separate, -n_state_separate_hi)
  }

  if(drop_extra){
    counts_joined <- counts_joined %>% select(-n_unfiltered, -n_unconfirmed,
                                              -n_collateral, -n_nonconflict)
  }

  counts_joined
}
