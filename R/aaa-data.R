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

#' Ultimate Consequences Deaths Table
#'
#' An internal copy of the Ultimate Consequences deaths table
#' (not for public release)
#'
#' @format
#' `event_status_aug24` is a large data table. A tibble [211 × 21]
#' (S3: tbl_df/tbl/data.frame)
#'
#' @details
#' c("event_title", "year", "id_event", "n", "n_state_perp", "n_state_perp_excl",
#' "n_state_perp_direct_excl", "n_state_victim", "n_name_problems",
#' "name_problem_expl", "n_blank_notes", "n_blank_highestsource",
#' "n_blank_certainty", "outcome", "outcome_summary", "has_narrative",
#' "quality_narrative", "has_uncertain_deaths", "expand_narrative",
#' "newspaper", "sourcing")
"event_status_aug24"

#' An internal copy of the Ultimate Consequences deaths table raw data
#' (not for public release)
#'
#' @format
#' `deaths_aug24` is a large data table. A tibble [675 × 50]
#' (S3: tbl_df/tbl/data.frame)
#'
#' @details
#' c("event_title", "unconfirmed", "year", "month", "day", "later_year",
#' "later_month", "later_day", "dec_firstname", "dec_surnames",
#' "id_indiv", "dec_age", "dec_alt_age", "dec_gender", "dec_ethnicity",
#' "dec_residence", "dec_nationality", "dec_affiliation", "dec_spec_affiliation",
#' "dec_title", "cause_death", "cause_medical", "munition", "weapon",
#' "victim_armed", "perp_category", "perp_group", "perp_firstname",
#' "perp_surname", "perp_pol_stalemate", "dec_pol_stalemate", "perp_gender",
#' "perp_affiliation", "intentionality", "place", "location_precision",
#' "address", "community", "municipality", "province", "department",
#' "certainty_level", "pres_admin", "protest_campaign", "protest_domain",
#' "pol_assassination", "state_perpetrator", "state_responsibility",
#' "navarro_2006_number", "narrative")
"deaths_raw_sep24"

#' A list of helper variables
#'
#' @format A list of helper variables for the package. The list contains:
#' `lev$pres_admin` (a list of levels of presidential administration),
#' `lev$location_precision` (a list of levels of location precision),
#' `lev$state_resp` (a list of levels of state responsibility),
#' `lev$protest_domain` (a list of levels of protest domain),
#' `lev$department` (a list of levels of department). These names
#' correspond to columns in the `deaths_aug24` data frame.
"lev"

#' Helper variable for levels of presidential administration
#'
#' @format A list with members `president$levels` (a chronological list of
#' names for each presidency in English), `president$levels_es` (a chronological
#' list of names for each presidency in Spanish), `president$id_presidency`
#' (a sequence of unique ID's beginning with "p101" in October 1982), and
#' `president$initials` (two-
#' or three-letter initials for display in shortened contexts)
"lev$president"

#' Helper variable for names for presidential administrations
#'
#' A look-up table for presidential administrations in Bolivia from
#' 1964 to the present. Columns are as follows:
#' - presidency: Name of the presidential administration in English
#' - id_presidency: Unique identifier for the presidential administration
#' - presidency_year: Common name for the president, plus years in office
#'     in parentheses
#' - presidency_commonname: Common name for the president
#' - presidency_fullname: Full name of the president
#' - presidency_surnames: Surnames of the president
#' - presidency_year_es, presidency_commonname_es, presidency_fullname_es:
#'     As above but with text (besides the name) in Spanish
#' - presidency_initials: Initials of the president
#' - presidency_initials_num: Initials of the president with multiple
#'     terms distinguished by numbers and years of Junta labeled
#' - first_day: Date of the first day of the presidential term
#' - last_day: Date of the last day of the presidential term
#'
#' @format A tibble with multiple columns including unqiue identifiers in
#'   `presidency` (the levels of pres_admin in English) and `presidency_id`
#'   (a unique identifier in the format "p104"). `first_day` and `last_day`
#'   are dates indicating the term of offfice of each president.
#'
"presidency_name_table"


#' Helper variable for levels of location precision
#'
#' @format A list with members `location_precision$levels`, a small-to-large
#' list of levels in English: c("address", "poi_small", "intersection",
#' "block", "poi_large", "road", "community", "town", "rural_zone",
#' "municipality", "province", "region", "department")
"location_precision"

#' Helper variable for levels of state responsibility
#'
#' @format A list with members `state_resp$levels` (an ordered list of
#' state responsibility levels in English), `state_resp$colors` (name
#' and color pairs for these levels in English) and `state_resp$colors_es`,
#' the same pairs with Spanish names
"state_resp"

#' Helper Variable for Levels of Protest Domain
#'
#' @format A list with members `protest_domain$title` (the string "Protest
#' Domain"), `protest_domain$levels` (an ordered list of
#' state responsibility levels in English), `protest_domain$colors` (name
#' and color pairs for these levels in English).
"protest_domain"

#' Helper Variable for Levels of Department
#'
#' @format A list with members `departments$title` (the string
#' "Department"), `departments$levels` (an ordered list of
#' department levels, with unknown in English), `departments$levels`
#' (an ordered list of department levels, with unknown in Spanish), and
#' `departments$colors` (name and color pairs for these levels).
"departments"

#' Table of Month Names in Spanish
#'
#' @format A list with twelve lower-case names as members from "enero"
#'   to "diciembre"
"month.name.es"

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
    "later_year_notes",
    "date_text",
    "sr_text"
  )
)


