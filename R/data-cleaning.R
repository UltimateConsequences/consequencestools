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

  # check if already converted
  if (("first_day" %in% colnames(pt)) & ("days_in_office" %in% colnames(pt))) return(pt)

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

  dataframe <- dataframe %>%
    mutate(pres_admin = factor(pres_admin, levels= lev$pres_admin$levels))
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
  de <- mutate(de, sp_text = state_perpetrator) %>% # add a new column with original text in "state_perpetrator"
    relocate(sp_text, .after=state_perpetrator)

  de$state_perpetrator <- str_to_title(de$state_perpetrator)
  de$state_perpetrator <- fct_na_value_to_level(de$state_perpetrator, level = "Unknown")
  if (simplify){
    sp_levels_in_order <- c("Unknown", "No", "Indirect", "Mutiny", "Yes")

    de$state_perpetrator <- fct_collapse(de$state_perpetrator,
                                         Yes = c("Yes", "Likely Yes", "Presumed Yes"),
                                         Indirect = c("Indirect"),
                                         No = c("No", "Likely No"),
                                         Mutiny = c("In Mutiny"),
                                         Unknown  = c("Unknown", "Disputed", "Suspected") ) %>% suppressWarnings()

    if(length(levels(de$state_responsibility)) > length(lev$state_responsibility$levels)) {
      warning(paste("Unknown state_perpetrator level:",
                    setdiff(levels(de$state_perpetrator), sp_levels_in_order), "\n"))
    }

    de$state_perpetrator <- fct_relevel(de$state_perpetrator,
                                        sp_levels_in_order) # default ordering
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


    if(length(levels(de$state_responsibility)) > length(lev$state_responsibility$levels)) {
      warning(paste("Unknown state_responsibility level:",
                    setdiff(levels(de$state_responsibility), lev$state_responsibility$levels), "\n"))
    }

    de$state_responsibility <- fct_relevel(de$state_responsibility,
                                           lev$state_responsibility$levels) %>%
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

  lp_levels <- lev$location_precision$levels

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

  if(is.character(dataframe$protest_domain)) {
    dataframe <- dataframe %>%
                # rowwise() %>%
                mutate(protest_domain = string_to_listcase(protest_domain))
  }

  existing_levels <- lev$protest_domain$levels[lev$protest_domain$levels %in% dataframe$protest_domain]
  dataframe$protest_domain <- fct_relevel(dataframe$protest_domain, existing_levels)
  dataframe$protest_domain <- fct_na_value_to_level(dataframe$protest_domain, level = na.level)
  return(dataframe)
}

#' Assign Levels for Affiliation
#'
#' These functions factor the variable `dec_affiliation` and `perp_affiliation`.
#'
#' @param dataframe A dataframe containing a the relevant affiliation column
#' @param na.level The name of the level to which NA is assigned
#'
#' @return A dataframe with the affiliation column turned into an
#'   ordered factor
#' @export
#'
#' @examples
#' assign_dec_affiliation_levels(deaths_aug24)
#' assign_perp_affiliation_levels(deaths_aug24)
assign_dec_affiliation_levels <- function(dataframe, na.level = "Unknown"){

  if ("dec_affiliation" %in% colnames(dataframe)) {
    existing_levels <- lev$dec_affiliation$levels[lev$dec_affiliation$levels %in% dataframe$dec_affiliation]
    dataframe$dec_affiliation <- fct_relevel(dataframe$dec_affiliation, existing_levels)
    dataframe$dec_affiliation <- fct_na_value_to_level(dataframe$dec_affiliation, level = na.level)
  }

  return(dataframe)
}

#' @rdname assign_dec_affiliation_levels
#' @export
assign_perp_affiliation_levels <- function(dataframe, na.level = "Unknown"){

  if ("perp_affiliation" %in% colnames(dataframe)) {
    existing_levels <- lev$perp_affiliation$levels[lev$perp_affiliation$levels %in% dataframe$perp_affiliation]
    dataframe$perp_affiliation <- fct_relevel(dataframe$perp_affiliation, existing_levels)
    dataframe$perp_affiliation <- fct_na_value_to_level(dataframe$perp_affiliation, level = na.level)
  }

  return(dataframe)
}

#' Assign Levels to Multiple Variables
#'
#' This function assigns levels to multiple variables in a dataframe,
#' deploying the relevant functions. You can use "standard" (or)
#' to use the default list `standard_factoring_variables`.
#'
#' @param dataframe A dataframe to which levels will be assigned
#' @param ... Variable names to which levels should be assigned
#' @param .simplify Logical, whether to simplify the levels, where this
#'   applies (default: TRUE)
#'
#' @return A dataframe with assigned levels
#'
#' @details This function applies level assignment to specified variables in the dataframe.
#' It only processes variables that exist in both the input list and the dataframe.
#'
#' @examples
#' de <- assign_levels(deaths_aug24, "pres_admin", "state_responsibility", "state_perpetrator")
#'
#' @export
assign_levels <- function(dataframe, ..., .simplify = TRUE) {
  var_list <- rlang::ensyms(...)

  if (length(var_list) == 1){
    if (var_list[[1]] == "standard") {
    var_list <- standard_factoring_variables
    }
  }

  if (length(var_list) == 0) {
    stop("No variables provided for level assignment. Use assign_levels(dataframe, \"standard\") to assign levels to the standard factoring set.")
  }

  functions <- list(
    pres_admin = assign_presidency_levels,
    state_responsibility = assign_state_responsibility_levels,
    state_perpetrator = assign_state_perpetrator_levels,
    protest_domain = assign_protest_domain_levels,
    location_precision = assign_location_precision_levels,
    dec_affiliation = assign_dec_affiliation_levels,
    perp_affiliation = assign_perp_affiliation_levels
  )

  for (var in var_list) {
    var_name <- rlang::as_string(var)
    if (!(var_name %in% names(functions))) {
      warning(paste("No corresponding function for variable:", var))
    } else if (!(var_name %in% names(dataframe))) {
      warning(paste("Variable not found in dataframe:", var))
    } else {
      if (var_name == "state_responsibility" || var_name == "state_perpetrator") {
        dataframe <- functions[[var]](dataframe, simplify = .simplify )
      } else {
        dataframe <- functions[[var]](dataframe)
      }
    }
  }

  return(dataframe)
}




