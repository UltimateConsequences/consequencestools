#' Render a numerical age as text
#'
#' Returns a partial numerical and partially spelled out age for
#' a person. `render_age` is in English and `render_age_es` is
#' in Spanish
#'
#' @param age A real number or fraction experssing age in years
#'
#' @return A text string stating the age using digits and words,
#'   providing only years for over 2 years old; years and months fro
#'   under two years old; and "less than 30 days" where appropriate.
#' @export
#'
#' @examples
#' render_age(7)
#' render_age(2+5/12)
#' render_age_es(1+5/12)
#' render_age(8/12)
#' render_age_es(33/365)
#' render_age(28/365)
render_age <- function(age){
  assertthat::assert_that(is.numeric(age))

  age_number <- floor(age)
  months_number <- round(12*(age-age_number), digits = 0)

  age_string <- dplyr::case_when(
    age_number >= 2 ~ str_c(age_number, " years"),
    (age_number==1) & (months_number>1) ~ str_c("1 year, ", months_number, " months"),
    (age_number==1) & (months_number==1) ~ str_c("1 year, ", months_number, " month"),
    (age_number==1) & (months_number==0) ~ str_c("1 year"),
    (age_number==0) & (months_number>1) ~ str_c(months_number, " months"),
    (age_number==0) & (months_number==1) ~ str_c(months_number, " month"),
    (age_number==0) & (months_number==0) ~ str_c("less than 30 days")
  )
  return(age_string)
}

#' @rdname render_age
#'
#' @export
render_age_es <- function(age){
  age_number <- floor(age)
  months_number <- round(12*(age-age_number), digits = 0)

  age_string <- dplyr::case_when(
    age_number >= 2 ~ str_c(age_number, " años"),
    (age_number==1) & (months_number>1) ~ str_c("1 año y ", months_number, " meses"),
    (age_number==1) & (months_number==1) ~ str_c("1 año y ", months_number, " mes"),
    (age_number==1) & (months_number==0) ~ str_c("1 año"),
    (age_number==0) & (months_number>1) ~ str_c(months_number, " meses"),
    (age_number==0) & (months_number==1) ~ str_c(months_number, " mes"),
    (age_number==0) & (months_number==0) ~ str_c("menos que 30 días")
  )
  return(age_string)
}
