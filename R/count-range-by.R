utils::globalVariables(c("department", "n", "n_unfiltered", "n_state_perp",
                         "n_state_perp_hi", "n_state_victim", "n_state_victim_hi",
                         "n_state_separate", "n_state_separate_hi",
                         "n_unconfirmed", "n_collateral", "n_nonconflict",
                         "sp_text"))


#' Add a new column to a dataframe if it doesn't exist
#'
#' @param df A dataframe
#' @param new_col String name of the new column to add
#' @param source_col String name of the existing column to copy from
#'
#' @return The modified dataframe
#'
#' @noRd
add_column_if_missing <- function(df, new_col, source_col) {
  if (!(new_col %in% names(df))) {
    df <- dplyr::mutate(df, !!new_col := !!rlang::sym(source_col))
  }
  return(df)
}

#' Calculate counts for deaths data
#'
#' @param data A dataframe containing death records
#' @param by A column name to group by
#'
#' @return A dataframe with counts grouped by the specified column
#'
#' @noRd
calculate_counts <- function(data, by) {
  data <- add_column_if_missing(data, "sr_text", "state_responsibility")
  data <- add_column_if_missing(data, "sp_text", "state_perpetrator")

  counts <- data %>%
    dplyr::filter(!is.na({{by}}), {{by}} != "") %>%
    group_by({{by}}) %>%
    summarize(
      n = dplyr::n(),
      n_state_perp = sum(sp_text %in% c("Yes", "Disputed", "Likely Yes", "Presumed Yes", "Indirect"),
                         na.rm = TRUE),
      n_state_victim = sum(str_detect(sr_text, "^State victim") &
                             !str_detect(intentionality, "cident"), na.rm = TRUE),
      n_state_separate = sum(str_detect(sr_text, "Separate from state"), na.rm = TRUE),
      .groups = "drop"
    )
  return(counts)
}

calculate_counts_from_factored_data <- function(data, by) {
  counts <- data %>%
    dplyr::filter(!is.na({{by}}), {{by}} != "") %>%
    group_by({{by}}) %>%
    summarize(
      n = dplyr::n(),
      n_state_perp = sum(state_perpetrator == "Yes", na.rm = TRUE),
      n_state_victim = sum((state_responsibility=="Victim") &
                             !str_detect(intentionality, "cident"), na.rm = TRUE),
      n_state_separate = sum((state_responsibility=="Separate"), na.rm = TRUE),
      .groups = "drop"
    )
  return(counts)
}

#' Calculate unfiltered counts for deaths data
#'
#' @param data A dataframe containing unfiltered death records
#' @param by A column name to group by
#'
#' @return A dataframe with unfiltered counts grouped by the specified column
#'
#' @noRd
calculate_counts_unfiltered <- function(data, by) {
  data <- add_column_if_missing(data, "sr_text", "state_responsibility")
  data <- add_column_if_missing(data, "sp_text", "state_perpetrator")

  counts_unfiltered <- data %>%
    dplyr::filter(!is.na({{by}}), {{by}} != "") %>%
    group_by({{by}}) %>%
    summarize(
      n_unconfirmed = sum(unconfirmed == TRUE, na.rm = TRUE),
      n_collateral = sum(intentionality == "Collateral", na.rm = TRUE),
      n_nonconflict = sum(str_detect(intentionality, "Nonconflict"), na.rm = TRUE),
      n_state_perp_hi = sum(sp_text %in% c("Yes", "Disputed", "Likely Yes", "Presumed Yes", "Indirect"), na.rm = TRUE),
      n_state_victim_hi = sum(str_detect(sr_text, "^State victim"), na.rm = TRUE),
      n_state_separate_hi = sum(str_detect(sr_text, "Separate from state"), na.rm = TRUE),
      n_unfiltered = dplyr::n(),
      .groups = "drop"
    )
  return(counts_unfiltered)
}

calculate_counts_unfiltered_from_factored_data <- function(data, by) {
  counts_unfiltered <- data %>%
    dplyr::filter(!is.na({{by}}), {{by}} != "") %>%
    group_by({{by}}) %>%
    summarize(
      n_unconfirmed = sum(unconfirmed == TRUE, na.rm = TRUE),
      n_collateral = sum(intentionality == "Collateral", na.rm = TRUE),
      n_nonconflict = sum(str_detect(intentionality, "Nonconflict"), na.rm = TRUE),
      n_state_perp_hi = sum(state_perpetrator == "Yes", na.rm = TRUE),
      n_state_victim_hi = sum((state_responsibility=="Victim"), na.rm = TRUE),
      n_state_separate_hi = sum((state_responsibility=="Separate"), na.rm = TRUE),
      n_unfiltered = dplyr::n(),
      .groups = "drop"
    )
  return(counts_unfiltered)
}


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
#' @param complete When true, fill in unknown counts with zeroes. This
#'   does not override the blank_hi parameter.
#' @param .disqualified When TRUE, return the list of events that would only
#'   be included if the unfiltered list of deaths is used.
#' @param .verbose When TRUE, message with the number of events in the
#'   filtered and unfiltered datasets.
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
                           complete = TRUE, blank_hi = TRUE,
                           .disqualified=FALSE,
                           .verbose = FALSE){

  # We want to operate on the unfactored text, if possible


  # First case: unfactored state_responsibility, state_perpetrator
  if(("sp_text" %in% names(deaths)) & ("sr_text" %in% names(deaths))){
    # First Case: SP, SR text available
    counts <- calculate_counts(deaths, {{by}})
    counts_unfiltered <- calculate_counts_unfiltered(deaths_unfiltered, {{by}})

  }else if(rlang::is_character(deaths$state_responsibility) &
           rlang::is_character(deaths$state_perpetrator)) {
    # Second Case: Unfactored sr, sp, use as text
    # — This is done internal to these functions, so same code as first case
    counts <- calculate_counts(deaths, {{by}})
    counts_unfiltered <- calculate_counts_unfiltered(deaths_unfiltered, {{by}})
    # Third case, calculate based on factored data
  }else{
    counts <- calculate_counts_from_factored_data(deaths, {{by}})
    counts_unfiltered <- calculate_counts_unfiltered_from_factored_data(deaths_unfiltered, {{by}})
  }

  if (.verbose) cat("Counts: ", nrow(counts), "\n")
  if (.verbose) cat("Counts unfiltered: ", nrow(counts_unfiltered), "\n")

  # merge the event/campaign table with the corresponding data from the unfiltered counts
  counts_joined <- counts %>%
    dplyr::full_join(counts_unfiltered) %>%
    suppressMessages()

  if(.disqualified){
    disqualified_table <- dplyr::anti_join(counts_unfiltered, counts)
    return(disqualified_table)
  }

  counts_joined <- counts_joined %>%
    relocate(n_state_perp_hi, .after=n_state_perp) %>%
    relocate(n_state_victim_hi, .after=n_state_victim) %>%
    relocate(n_state_separate_hi, .after=n_state_separate) %>%
    relocate(n_unfiltered, .after = n) # n_unfiltered in the "high" value of n
    # but our standard option is to drop it below

  if(complete){
    counts_joined <- counts_joined %>%
      tidyr::complete({{by}}, fill =list(
        n = 0,
        n_unfiltered = 0,
        n_state_perp = 0,
        n_state_perp_hi = 0,
        n_state_victim = 0,
        n_state_victim_hi = 0,
        n_state_separate = 0,
        n_state_separate_hi = 0,
        n_collateral = 0,
        n_nonconflict = 0
      ))
  }

   # blank the high values if they are the same as the low values…
  if(blank_hi){
    counts_joined <- counts_joined %>%
      mutate(n_unfiltered = ifelse(n==n_unfiltered, NA, n_unfiltered)) %>%
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
