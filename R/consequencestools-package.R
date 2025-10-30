#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr if_any
#' @importFrom dplyr is.tbl
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr rename_with
#' @importFrom dplyr rowwise
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_collapse
#' @importFrom forcats fct_explicit_na
#' @importFrom forcats fct_na_value_to_level
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 coord_equal
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom grDevices col2rgb
#' @importFrom grDevices colorRamp
#' @importFrom grDevices rgb
#' @importFrom incase in_case
#' @importFrom lubridate days
#' @importFrom reactable colDef
#' @importFrom reactablefmtr nytimes
#' @importFrom rlang !!!
#' @importFrom rlang .data
#' @importFrom rlang enquo
#' @importFrom rlang list2
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_glue
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace_na
#' @importFrom stringr str_sub
#' @importFrom stringr str_to_sentence
#' @importFrom stringr str_to_title
#' @importFrom tidyselect any_of
#' @importFrom tidyselect last_col
#' @importFrom transcats translated_join_vars
#' @importFrom utils head
#' @importFrom waffle geom_waffle
#' @importFrom zoo as.Date.yearmon
## usethis namespace: end
NULL

# Operators from the tidyverse
# "You may wish to import any syntactic sugar functionality here"
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
globalVariables(c(":=", "!!"))
