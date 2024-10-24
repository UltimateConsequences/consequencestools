#' Clean variable names in presidents table
#'
#' Puts all names into lower snake_case, removes "frequency of" at start
#' and any parenthetical expression at the end. Changes spaces, hyphens,
#' slashes to underscore. Removes question marks.
#'
#' @param pt A dataframe with named variables
#'
#' @return A modified version of the dataframe
#' @export
#'
clean_pt_variables <- function(pt){
  stopifnot(is.tbl(pt))
  pt <- rename_with(pt, ~ sub(" \\(.*", "", .x))
  pt <- rename_with(pt, ~ tolower(gsub(" ", "_", .x, fixed = TRUE)))
  pt <- rename_with(pt, ~ gsub("frequency_of_", "", .x, fixed = TRUE))
  pt <- rename_with(pt, ~ gsub("-", "_", .x, fixed = TRUE))
  pt <- rename_with(pt, ~ gsub("/", "_", .x, fixed = TRUE))
  pt <- rename_with(pt, ~ gsub("?", "", .x, fixed = TRUE))

  return(pt)
}

#' Assign Presidency Levels
#'
#' @param dataframe A dataframe that may contain a 'pres_admin' column
#'
#' @return A dataframe with 'pres_admin' column as a factor with levels from president$levels
#' @export
#'
#' @examples
#' df <- data.frame(pres_admin = c("Obama", "Trump", "Biden"))
#' assign_presidency_levels(df)
assign_presidency_levels <- function(dataframe){
  # This complicated construction ensures that no errors will be generated if there is no pres_admin column
  if (!("pres_admin" %in% colnames(dataframe))) return(dataframe)

  dataframe <- dataframe %>% mutate(pres_admin = factor(pres_admin, levels=president$levels))
  return(dataframe)
}

#' Assign State Perpetrator Levels
#'
#' @param dataframe A dataframe containing a 'state_perpetrator' column
#' @param simplify Logical, whether to simplify the levels (default: FALSE)
#'
#' @return A dataframe with modified 'state_perpetrator' column
#' @export
#'
#' @examples
#' df <- data.frame(state_perpetrator = c("yes", "no", NA, "LIKELY YES"))
#' assign_state_perpetrator_levels(df)
#' assign_state_perpetrator_levels(df, simplify = TRUE)
assign_state_perpetrator_levels <- function(dataframe, simplify=FALSE){
  de <- dataframe
  de$state_perpetrator <- str_to_title(de$state_perpetrator)
  de$state_perpetrator <- fct_na_value_to_level(de$state_perpetrator, level = "Unknown")
  if (simplify){
    de$state_perpetrator <- fct_collapse(de$state_perpetrator,
                                         Yes = c("Yes", "Likely Yes", "Presumed Yes"),
                                         Indirect = c("Indirect"),
                                         No = c("No", "Likely No"),
                                         Mutiny = c("In Mutiny"),
                                         Unknown  = c("Unknown", "Disputed", "Suspected") ) %>% suppressWarnings()
    de$state_perpetrator <- fct_relevel(de$state_perpetrator,
                                        c("Unknown", "No", "Indirect", "Mutiny", "Yes")) # default ordering
  }
  return(de)
}
#' Assign State Responsibility Levels
#'
#' This function simplifies the variable `state_responsibility`,
#' in part by overriding its values if the death was a conflict accident
#' (state_responsibility set to "Accidental") or an incidental death,
#' according to `intentionality`. NA's are resolved to "Unknown".
#'
#' If simplify is FALSE, this function takes the existing values of
#' state_responsibility and turns them each into factors.
#'
#' If simplify is TRUE, this function collapse to one-word values:
#' Perpetrator, Involved, Victim, Separate, Unintentional, Unknown.
#'
#' @param dataframe A dataframe containing a 'state_responsibility' column
#' @param simplify Logical, whether to simplify the levels (default: FALSE)
#'
#' @return A dataframe with modified 'state_responsibility' column
#' @export
#'
#' @examples
#' df1 <- data.frame(state_responsibility = c("State perpetrator", "Unknown", "State involved"),
#'   intentionality = c("Intentional", "Direct", "Conflict Accident"))
#' assign_state_responsibility_levels(df1)
#' assign_state_responsibility_levels(df1, simplify = TRUE)
#' assign_state_responsibility_levels(deaths_aug24)
assign_state_responsibility_levels <- function(dataframe, simplify=FALSE){
  if (is.factor(dataframe$state_responsibility)){
    message("assign_state_responsibility_levels: leaving factored variable unchanged.")
    return(dataframe)
  }

  de <- dataframe
  de <- mutate(de, sr_text = state_responsibility) %>% # add a new column with original text in "state_responsibility"
    relocate(sr_text, .after=state_responsibility)

  de <- de %>% mutate(state_responsibility = case_when(    # overwrite the state responsibility for unintentional cases
    intentionality == "Incidental" ~ "Incidental",
    intentionality == "Conflict Accident" ~ "Accidental",
    TRUE ~ state_responsibility))

  de$state_responsibility <- fct_na_value_to_level(de$state_responsibility, level = "Unknown")
  if (simplify){
    de$state_responsibility <- fct_collapse(de$state_responsibility,
                                            Perpetrator = c("State perpetrator", "State likely perpetrator",
                                                            "State perpetrator, State victim refusing orders",
                                                            "State perpetrator, State victim in mutiny",
                                                            "State indirect perpetrator"),
                                            Involved = c("State involved", "Political victim",
                                                         "Political victim / political perpetrator",
                                                         "Political victim / unknown perpetrator",
                                                         "Possibly state involved"),
                                            Victim = c("State victim",
                                                       "State victim, State perpetrator in mutiny"),
                                            Separate = c("Separate from state"),
                                            Unintentional = c("Incidental", "Accidental"),
                                            Unknown  = c("Unknown", "Unclear", "Disputed") ) %>%
                                 suppressWarnings()

    de$state_responsibility <- fct_relevel(de$state_responsibility, sr_levels) %>%
      suppressWarnings()
  }
  return(de)
}

#' Produce an Estimated Date String for (Sometimes Incomplete) Dates
#'
#' This function produces dates using partial information to enable
#' sequential sorting by date even when information is incomplete.
#' It treats unknown dates within a year as June 30 and unknown dates
#' within a month as the 15th day of the month.
#'
#' @param year
#' @param month
#' @param day
#'
#' @return A numerical date string in "YYYY-MM-DD" format
#' @export
#' @importFrom incase in_case
#'
#' @examples
#' estimated_date_string(2015, NA, NA), "2015-06-30")
#' estimated_date_string(2015, 8, NA), "2015-08-15")
#' estimated_date_string(2015, 1, 11)
estimated_date_string <- function(year, month, day){
  date_string <- incase::in_case(
    (is.na(year)) ~ NA,
    (is.na(day) & is.na(month) & !is.na(year)) ~ str_glue("{year}-06-30"),
    (is.na(day) & !is.na(month) & !is.na(year)) ~ str_glue("{year}-{month}-15"),
    TRUE ~ paste(year, month, day, sep = "-")
  )

  return(date_string)
}

displayed_date_string <- function(year, month, day){
  # Unfortunate work around to the simultaneous evaluation done by incase::in_case()
  # when using the vectors month.abb, month.name
  month_name <- ""
  month_abb <- ""
  if (!is.na(month)) {
    month_name <- month.name[month]
    month_abb <- month.abb[month]
  }

  date_string <- incase::in_case(
    (is.na(year)) ~ "Date Unknown",
    ((is.na(day) & is.na(month) & !is.na(year))) ~ str_glue("{year}"),
    (is.na(day) & !is.na(month) & !is.na(year)) ~ str_glue("{month_name} {year}"),
    TRUE ~ str_glue("{day} {month_abb} {year}")
  )

  return(date_string)
}

combine_dates <- function(dataframe, incl_laterdate=FALSE, date_at_front=FALSE,
                          unknown_date_string = NA){
  dataframe <- dataframe %>% rowwise() %>%
                 dplyr::mutate(date_text = estimated_date_string(year, month, day)) %>%
                 dplyr::mutate(date = as.Date(lubridate::ymd(date_text))) %>%
                 dplyr::mutate(date_text = displayed_date_string(year, month, day))

  if(incl_laterdate & ("later_day" %in% colnames(dataframe))){
    dataframe <- dplyr::mutate(dataframe,
                        laterdate = (paste(later_year, later_month, later_day, sep="-") %>%
                                       lubridate::ymd() %>% as.Date()))
  }
  if(date_at_front){
    dataframe <- dataframe %>% relocate(event_title, date) %>%
                               relocate(year, month, day, .after = last_col())
  }

  dataframe
}

assign_location_precision_levels <- function(dataframe){
  # This assignment is done via data.R
  #
  # location_precision$levels <- c(
  #   "address", "poi_small", "intersection",
  #   "block", "poi_large", "road", "community",
  #   "town", "rural_zone",
  #   "municipality", "province",
  #   "region", "department")

  # This complicated construction ensures that no errors will be generated if there is no location_precision column
  if (!("location_precision" %in% colnames(dataframe))) return(dataframe)

  dataframe <- dataframe %>% mutate(location_precision = factor(location_precision, levels=location_precision$levels))
  return(dataframe)
}

string_to_listcase <- function(string) {
  string %>% str_replace(",", ".") %>%
    str_to_sentence() %>%
    str_replace("\\.", ",")
  }

assign_protest_domain_levels <- function(dataframe, na.level = "Unknown"){
  # This complicated construction ensures that no errors will be generated if there is no location_precision column
  if (!("protest_domain" %in% colnames(dataframe))) return(dataframe)

  # factor protest_domain
  protest_domain.grouped <<- c(
    "Gas wars",                         # Economic
    "Economic policies",
    "Labor",
    "Education",
    "Mining",
    "Coca",                             # Rural
    "Peasant",
    "Rural land",
    "Rural land, Partisan politics",
    "Ethno-ecological",
    "Drug trade",                       # Criminalized
    "Contraband",
    "Local development",                # Local
    "Municipal governance",
    "Partisan politics",                # (solo)
    "Disabled",                         # (solo)
    "Guerrilla",                        # Armed actors
    "Paramilitary",
    "Urban land",
    "Unknown")                       # (solo)

  if(is.character(dataframe$protest_domain))
  {
    dataframe <- dataframe %>%
                mutate(protest_domain = string_to_listcase(protest_domain))
  }
  dataframe$protest_domain <- fct_na_value_to_level(dataframe$protest_domain, level = na.level)
  dataframe$protest_domain <- fct_relevel(dataframe$protest_domain, protest_domain.grouped)
  return(dataframe)
}


