#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_collapse
#' @importFrom forcats fct_explicit_na
#' @importFrom forcats fct_na_value_to_level
#' @importFrom forcats fct_relevel
#' @importFrom rlang .data
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_glue
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_to_title
#' @importFrom zoo as.Date.yearmon
## usethis namespace: end
NULL

# Operators from the tidyverse
# "You may wish to import any syntactic sugar functionality here"
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
globalVariables(c(":=", "!!"))
