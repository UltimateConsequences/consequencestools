#' Ultimate Consequences Deaths Table
#'
#' An internal copy of the Ultimate Consequences deaths table
#' (not for public release)
#'
#' @format
#' `deaths_aug24` is a large data table. A tibble [670 × 59]
#' (S3: tbl_df/tbl/data.frame)
#'
#' @details
#' c("event_title", "unconfirmed", "year", "month", "day", "later_year",
#' "later_month", "later_day", "dec_firstname", "dec_surnames",
#' "id_indiv", "dec_age", "dec_alt_age", "dec_gender", "dec_ethnicity",
#' "dec_residence", "dec_nationality", "dec_affiliation", "dec_spec_affiliation",
#' "dec_title", "cause_death", "cause_medical", "munition", "weapon",
#' "victim_armed", "va_notes", "perp_category", "perp_group", "perp_firstname",
#' "perp_surname", "perp_pol_stalemate", "dec_pol_stalemate", "perp_gender",
#' "perp_affiliation", "intentionality", "place", "location_precision",
#' "address", "community", "municipality", "province", "department",
#' "certainty_level", "pres_admin", "protest_campaign", "protest_domain",
#' "pol_assassination", "state_perpetrator", "sp_notes", "state_responsibility",
#' "sr_notes", "navarro_2006_number", "narrative", "day_notes",
#' "month_notes", "year_notes", "later_day_notes", "later_month_notes",
#' "later_year_notes")
"deaths_aug24"

# This list produced by colnames(deaths_aug24) %>% dput()
# Needed for dplyr references to variables
# See: https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
utils::globalVariables(
  c(
    "event_title",
    "unconfirmed",
    "year",
    "month",
    "day",
    "later_year",
    "later_month",
    "later_day",
    "dec_firstname",
    "dec_surnames",
    "id_indiv",
    "dec_age",
    "dec_alt_age",
    "dec_gender",
    "dec_ethnicity",
    "dec_residence",
    "dec_nationality",
    "dec_affiliation",
    "dec_spec_affiliation",
    "dec_title",
    "cause_death",
    "cause_medical",
    "munition",
    "weapon",
    "victim_armed",
    "va_notes",
    "perp_category",
    "perp_group",
    "perp_firstname",
    "perp_surname",
    "perp_pol_stalemate",
    "dec_pol_stalemate",
    "perp_gender",
    "perp_affiliation",
    "intentionality",
    "place",
    "location_precision",
    "address",
    "community",
    "municipality",
    "province",
    "department",
    "certainty_level",
    "pres_admin",
    "protest_campaign",
    "protest_domain",
    "pol_assassination",
    "state_perpetrator",
    "sp_notes",
    "state_responsibility",
    "sr_notes",
    "navarro_2006_number",
    "narrative",
    "day_notes",
    "month_notes",
    "year_notes",
    "later_day_notes",
    "later_month_notes",
    "later_year_notes"
  )
)
