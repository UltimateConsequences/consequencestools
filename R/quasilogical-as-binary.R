#' Turn quasilogical variable into a binary one
#'
#' We use "quasilogical" variables to document uncertainty about facts
#' ("likely" means we lack certainty) and about overt claims ("Presumed"
#' denotes a logical inference like that civilians are unarmed). This
#' function flattens quasilogical variables into three options:
#' "Yes", "No" and "Disputed". The project
#'
#' @param datacolumn The relevant variable in a dataframe
#'
#' @return Same data column with simplified values
#' @export
#'
#' @examples
#' quasilogical_as_binary(deaths_aug24$state_perpetrator)
quasilogical_as_binary <- function(datacolumn){
  datacolumn %>% forcats::fct_collapse(
    "Yes" = c("Yes", "Likely Yes", "Indirect", "Presumed Yes", "Likely"),
    "No" = c("No", "Likely No", "Presumed No", "Likely Disarmed", "Disarmed"),
    "Disputed" = c("Disputed")) %>%
    suppressWarnings()
}
