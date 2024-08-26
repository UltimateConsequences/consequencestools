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
#' @importFrom rlang .data
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_glue
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_sub
## usethis namespace: end
NULL

# Operators from the tidyverse
# "You may wish to import any syntactic sugar functionality here"
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
globalVariables(c(":=", "!!"))
