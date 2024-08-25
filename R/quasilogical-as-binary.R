quasilogical_as_binary <- function(datacolumn){
  datacolumn %>% forcats::fct_collapse(
    "Yes" = c("Yes", "Likely Yes", "Indirect", "Presumed Yes"),
    "No" = c("No", "Likely No", "Presumed No", "Likely Disarmed")) %>%
    suppressWarnings()
}
