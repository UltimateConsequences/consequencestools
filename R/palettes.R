#' Color Palettes for a Numerical Scale
#'
#' A series of functions with different colors:
#' red_pal(), brown_pal(), bluegray_pal(), gray_pal().
#'
#' @param x A number from 0 to 1
#'
#' @return A color along the palette range
#' @export
#'
#' @examples
#' max_deaths <- 100
#' red_pal(80/max_deaths)
#' brown_pal(80/max_deaths)
#' bluegray_pal(80/max_deaths)
#' gray_pal(80/max_deaths)
red_pal <- function(x) {
  grDevices::rgb(grDevices::colorRamp(c("#ffe0e0", "#ff3030", "#bb2020"))(x), maxColorValue = 255)
}

#' @rdname red_pal
#' @export
brown_pal <- function(x) {
  grDevices::rgb(grDevices::colorRamp(c("#EFEBE9", "#795548", "#3E2723"))(x), maxColorValue = 255)
}

#' @rdname red_pal
#' @export
bluegray_pal <- function(x) {
  grDevices::rgb(grDevices::colorRamp(c("#ECEFF1", "#607D8B", "#37474F"))(x), maxColorValue = 255)
}

#' @rdname red_pal
#' @export
gray_pal <- function(x) {
  grDevices::rgb(grDevices::colorRamp(c("#EFEFEF", "#8B8B8B", "#4F4F4F"))(x), maxColorValue = 255)
}

#' Color Palettes by Category for a Numerical Scale
#'
#' A series of functions blending from a light grey to
#' category-specific colors: perp_pal() for state-perpetrated
#' deaths; sv_pal() for state-victim deaths, sep_pal() for
#' deaths separate from the state.
#'
#' @param x A number from 0 to 1
#'
#' @return A color along the palette range
#'
#' @export
#'
#' @examples
#' max_deaths <- 100
#' red_pal(80/max_deaths)
#' brown_pal(80/max_deaths)
#' bluegray_pal(80/max_deaths)
#' gray_pal(80/max_deaths)
perp_pal <- function(...) {
  if (exists("state_resp") & ("colors" %in% names(state_resp))){
    grDevices::rgb(grDevices::colorRamp(c("#eeeeee", state_resp$colors[["Perpetrator"]]))(...), maxColorValue = 255)
  } else {
    red_pal(...)
  }
}

#' @rdname perp_pal
#' @export
sv_pal <- function(...) {
  if (exists("state_resp") & ("colors" %in% names(state_resp))){
    grDevices::rgb(grDevices::colorRamp(c("#eeeeee", state_resp$colors[["Victim"]]))(...), maxColorValue = 255)
  } else {
    red_pal(...)
  }
}

#' @rdname perp_pal
#' @export
sep_pal <- function(...) {
  if (exists("state_resp") & ("colors" %in% names(state_resp))){
    grDevices::rgb(grDevices::colorRamp(c("#eeeeee", state_resp$colors[["Separate"]]))(...), maxColorValue = 255)
  } else {
    red_pal(...)
  }
}

#' Set Maximum Death Quantities
#'
#' @param df.responsibility A dataframe with n, n_state_victim, n_state_separate
#'
#' @return maxm: A list with three values $deaths, $sv_deaths, $sep_deaths
#' @export
#'
#' @examples
#' n_responsibility_by(deaths_aug24, department) %>% set_maximums()
set_maximums <- function(df.responsibility){
  assertthat::assert_that(is.data.frame(df.responsibility))
  assertthat::assert_that(all(c("n", "n_state_victim", "n_state_separate") %in% names(df.responsibility)))
  maxm <- list()
  maxm$deaths <- max(dplyr::select(df.responsibility, n:n_state_victim),  na.rm=TRUE)
  maxm$sv_deaths <- max(dplyr::select(df.responsibility, n_state_victim), na.rm=TRUE)
  maxm$sep_deaths <- max(dplyr::select(df.responsibility, n_state_separate), na.rm=TRUE)

  maxm
}


#' Calculate Luminance of a Hex-Coded Color
#'
#' @param hex Color expressed in hex code
#'
#' @return Luminance between 0 and 1
#'
#' @examples
#' luminance("#ffe0e0")
#' luminance("#ff3030")
#' luminance("#bb2020")
luminance <- function(hex="#ffffff") {
  rgb <- grDevices::col2rgb(hex)
  (0.299*rgb[1] + 0.587*rgb[2] + 0.114*rgb[3])/255
}



#' Convert color names to hex RGB strings
#'
#' @param cname Color name(s)
#'
#' @return Character vector giving the hex color code translation of
#' the provided color names.
#' @author Gregory R. Warnes
#'
#' @export
#'
#' @examples
#' col2hex("forestgreen")
col2hex <- function(cname)
{
  colMat <- grDevices::col2rgb(cname)
  grDevices::rgb(
    red=colMat[1,]/255,
    green=colMat[2,]/255,
    blue=colMat[3,]/255
  )
}
