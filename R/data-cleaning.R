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

  dataframe <- dataframe %>% mutate(pres_admin = factor(pres_admin, levels= consequencestools::president$levels))
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
  # This complicated construction ensures that no errors will be generated if column is missing
  if (!("state_responsibility" %in% colnames(dataframe))) return(dataframe)

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

    de$state_responsibility <- fct_relevel(de$state_responsibility, consequencestools::state_resp$levels) %>%
      suppressWarnings()
  }
  return(de)
}


#' Assign Location Precision Levels
#'
#' This function factors the variable `location_precision`.
#'
#' @param dataframe A dataframe containing a 'state_responsibility' column
#'
#' @return A dataframe with the 'location_precision' column turned into an
#'   ordered factor
#' @export
#'
#' @examples
#' assign_location_precision_levels(deaths_aug24)
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

  lp_levels <- location_precision$levels

  dataframe <- dataframe %>% mutate(location_precision = factor(location_precision, levels=lp_levels))
  return(dataframe)
}


#' Assign Protest Domain Levels
#'
#' This function factors the variable `protest_domain`.
#'
#' @param dataframe A dataframe containing a 'state_responsibility' column
#' @param na.level The name of the level to which NA is assigned
#'
#' @return A dataframe with the 'protest_domain' column turned into an
#'   ordered factor
#' @export
#'
#' @examples
#' assign_protest_domain_levels(deaths_aug24)
assign_protest_domain_levels <- function(dataframe, na.level = "Unknown"){
  # This complicated construction ensures that no errors will be generated if column is missing
  if (!("protest_domain" %in% colnames(dataframe))) return(dataframe)

  # factor protest_domain
  if(is.character(dataframe$protest_domain))
  {
    dataframe <- dataframe %>%
                mutate(protest_domain = string_to_listcase(protest_domain))
  }
  dataframe$protest_domain <- fct_na_value_to_level(dataframe$protest_domain, level = na.level)
  dataframe$protest_domain <- fct_relevel(dataframe$protest_domain, protest_domain$levels)
  return(dataframe)
}


