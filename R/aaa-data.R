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
#' `lev$department` (a list of levels of department), `lev$dec_affiliation`
#' and `lev$perp_affiliation` (a list of levels of affiliation).
#' These names correspond to columns in the `deaths_aug24` data frame.
"lev"

#' Helper variable for levels of presidential administration
#'
#' @format A list with members `president$levels` (a chronological list of
#' names for each presidency in English), `president$levels_es` (a chronological
#' list of names for each presidency in Spanish), `president$id_presidency`
#' (a sequence of unique ID's beginning with "p101" in October 1982), and
#' `president$initials` (two-
#' or three-letter initials for display in shortened contexts)
"president"

#' Helper variable for categories of affiliation
#'
#' @format A list with members `affiliations$levels` (a list of affiliation
#' categories in English), `affiliations$levels_es` (a list of affiliation
#' categories in Spanish), and `affiliations$colors` (a list of
#' affiliation categories with colors in English).
"affiliations"

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
"protest_domains"

#' Helper Variable for Levels of Department
#'
#' @format A list with members `departments$title` (the string
#' "Department"), `departments$levels` (an ordered list of
#' department levels, with unknown in English), `departments$levels`
#' (an ordered list of department levels, with unknown in Spanish), and
#' `departments$colors` (name and color pairs for these levels).
"departments"

#' List of variables to be factored
#'
#' @format A list of variables to be factored in the data set. The list
#' contains:
#'   "pres_admin", "state_responsibility", "state_perpetrator",
#'   "protest_domain", "dec_affiliation", "perp_affiliation",
#'   "location_precision."
"standard_factoring_variables"

#' Table of Month Names in Spanish
#'
#' @format A list with twelve lower-case names as members from "enero"
#'   to "diciembre"
"month.name.es"

#' Municipal Reference Table for Bolivia
#'
#' A reference table containing municipality information for Bolivia,
#' read from the official anexo of municipalities.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{codigo/cod.mun}{Municipal code (INE code)}
#'   \item{municipio/municipality}{Municipality name}
#'   \item{provincia/province}{Province name}
#'   \item{departamento/department}{Department name}
#' }
#'
#' @details
#' This table contains the official list of municipalities in Bolivia
#' with their corresponding INE (Instituto Nacional de Estadística) codes,
#' provinces, and departments. The table is used for municipality lookup
#' and ID assignment functions.
"anexo_municipios"

#' Municipality ID Lookup Table
#'
#' A comprehensive lookup table for municipality IDs and names in Bolivia,
#' including alternative municipality names and mappings.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{id_muni}{Municipal ID (INE code)}
#'   \item{muni_list}{List of alternative municipality names}
#'   \item{muni_anexo}{Standard municipality name from anexo}
#'   \item{muni_gb2014}{Municipality name in GB2014 format}
#'   \item{department}{Department name}
#' }
#'
#' @details
#' This table provides comprehensive municipality lookup functionality,
#' including alternative spellings and name variations. It supports
#' the id_for_municipality_2() function and other advanced municipality
#' matching operations.
"muni_id_lookup_table"

#' Municipality Name Conversion Table for GB2014 Format
#'
#' A lookup table for converting between standard municipality names
#' and GB2014 format municipality names in Bolivia.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{id_muni}{Municipal ID (INE code)}
#'   \item{muni_gb2014}{Municipality name in GB2014 format}
#'   \item{muni_anexo}{Standard municipality name from anexo}
#'   \item{recode}{R code string for recoding municipality names}
#' }
#'
#' @details
#' This table contains only municipalities where the GB2014 format name
#' differs from the standard anexo name. It includes generated R code
#' strings that can be used to perform the name conversions automatically.
"muni_gb2014_conversion"

#' Municipality Lookup Table in GB2014 Format
#'
#' A simplified lookup table using GB2014 format municipality names
#' for municipality ID and department lookup.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{id_muni}{Municipal ID (INE code)}
#'   \item{municipality}{Municipality name in GB2014 format}
#'   \item{department}{Department name}
#' }
#'
#' @details
#' This table provides a streamlined lookup mechanism using GB2014
#' format municipality names. It's derived from muni_id_lookup_table
#' and can be used when working with data that uses GB2014 naming
#' conventions.
"muni_gb2014_lookup"

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
    "protest_domains",
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
    "sr_text",
    "lev",
    "standard_factoring_variables",
    "muni_list",
    "muni_anexo",
    "muni_gb2014"
  )
)


