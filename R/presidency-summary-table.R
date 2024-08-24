# This script produces…
# de.pl: an extension of the overall data table with two new variables…
#.  pol_lean  — "Left", "Right", "" for Eduardo Rodríguez
#.  pol_lean2 — "UDP", "Neoliberal", "MAS-IPSP", "Right Interim"
#
# pt.summary: a table with subheadings consisting of subtables stacked vertically
#   1. Top table summarizes by presidency
#.  2. Middle table summarizes by pol_lean
#.  3. Bottom table summarizes by pol_lean2
#
# pt.summary.administration: consists of the top table,
#   only describing the data for each presidential administration
#
# pt.summary.partisanship: consists of the bottom two tables, 
#.  with first_day and last_day excluded
#
# pres.categorized provides a totals chart for deaths under each presidency
# columns: pres_admin, n, n_coca, n_armedactor, n_state_perp, n_state_perp_coca

# invoke summary table generation function
source(here::here("src", "n-categorized-by.R"), local = knitr::knit_global())

## This creates a summary table corresponding to the table "categorized deaths"
presidency_summary_table <- function(de) {
  
  # future exception should make sure that state perpetrator, state responsibility variables 
  # have been simplified, and if not, simplify them before passing to n_categorized_by()
  
  pres.categorized <- de %>%
    mutate(pres_admin = factor(pres_admin, levels = president_levels)) %>%
    n_categorized_by(pres_admin, complete=TRUE) %>%
    arrange(factor(pres_admin, president_levels)) # sort presidents chronologically
  return(pres.categorized)
}

pres.categorized <- presidency_summary_table(de)

# TESTING
# This independently produced variable should have the same values as pres.categorized$n
# pres.totals <- de %>%
#   mutate(pres_admin = factor(pres_admin, levels = president_levels)) %>%
#   group_by(pres_admin) %>%
#   dplyr::summarize(total = n()) %>%    # This variable counts deaths per presidency
#   complete(pres_admin, fill = list(total = 0))
# 

## Second data table: Presidencies by political lean

## We can create a similar table for an arbitrary subset of de
# enable analysis by political leaning

presidents.left=str_c("Hernán Siles Zuazo", "Evo Morales", "Luis Arce", sep = "|")
presidents.right=str_c("Víctor Paz Estenssoro", "Jaime Paz Zamora",
                       "Gonzalo Sanchez de Lozada \\(1st\\)", "Hugo Banzer \\(2nd\\)", "Jorge Quiroga",
                       "Gonzalo Sanchez de Lozada \\(2nd\\)", "Carlos Diego Mesa Gisbert",
                       "Interim military government", "Jeanine Áñez", sep = "|")

presidency_lean_summary_table <- function(de){
  de.pl <- de %>%
    mutate(pol_lean = case_when(
      str_detect(string = pres_admin, pattern=presidents.right) ~ "Right",
      str_detect(string = pres_admin, pattern=presidents.left) ~ "Left",
      TRUE ~ ""))
  
  pres.categorized.lean <- de.pl %>%
    filter(pol_lean != "") %>%
    n_categorized_by(pol_lean)  %>%
    dplyr::rename(pres_admin = pol_lean) # Put the leaning into the leftmost column
  
  return(pres.categorized.lean)
}

pres.categorized.lean <- presidency_lean_summary_table(de)

## Third datatable: Presidencies by historical period

# enable analysis by historical period
presidents.udp=str_c("Hernán Siles Zuazo")
presidents.neolib=str_c("Víctor Paz Estenssoro", "Jaime Paz Zamora",
                        "Gonzalo Sanchez de Lozada \\(1st\\)", "Hugo Banzer \\(2nd\\)", "Jorge Quiroga",
                        "Gonzalo Sanchez de Lozada \\(2nd\\)", "Carlos Diego Mesa Gisbert", sep = "|")
presidents.mas=str_c("Evo Morales", "Luis Arce", sep = "|")
presidents.righti=str_c("Interim military government", "Jeanine Áñez", sep = "|")

pol_lean2_levels <- c("UDP", "Neoliberal", "MAS-IPSP", "Right Interim") # in chronological order

presidency_period_summary_table <- function(de){
  
  de.pl <- de %>%
    mutate(pol_lean2 = case_when(    # assign a new flag variable pol_lean2 to for this factor
      str_detect(string = pres_admin, pattern=presidents.udp) ~ "UDP",
      str_detect(string = pres_admin, pattern=presidents.neolib) ~ "Neoliberal",
      str_detect(string = pres_admin, pattern=presidents.mas) ~ "MAS-IPSP",
      str_detect(string = pres_admin, pattern=presidents.righti) ~ "Right Interim",
      TRUE ~ ""))
  
  pol_lean2_levels <- c("UDP", "Neoliberal", "MAS-IPSP", "Right Interim") # in chronological order
  
  pres.categorized.lean2 <- de.pl %>%
    filter(pol_lean2 != "") %>%
    n_categorized_by(pol_lean2) %>%
    arrange(factor(pol_lean2, levels=pol_lean2_levels)) %>%
    dplyr::rename(pres_admin = pol_lean2)
  
  return(pres.categorized.lean2)
}

pres.categorized.lean2 <- presidency_period_summary_table(de)

# subsets of the presidency table for each category
pt.left <- filter(pt.study, str_detect(string = presidency, pattern=presidents.left))
pt.right <- filter(pt.study, str_detect(string = presidency, pattern=presidents.right))

pt.udp    <- filter(pt.study, str_detect(string = presidency, pattern=presidents.udp))
pt.neolib <- filter(pt.study, str_detect(string = presidency, pattern=presidents.neolib))
pt.mas    <- filter(pt.study, str_detect(string = presidency, pattern=presidents.mas))
pt.righti <- filter(pt.study, str_detect(string = presidency, pattern=presidents.righti))

# Now create the presidential outcomes table
#
# First functions to create the left side
add_pres_row <- function(existing_table, pres_group_title, pt) {
  add_row(existing_table,
          presidency = pres_group_title,
          first_day = min(pt$first_day),
          last_day = max(pt$last_day),
          days_in_office = sum(pt$days_in_office),
          years_in_office = sum(pt$days_in_office) / 365
  )
}

add_pres_title_row <- function(existing_table, pres_group_title) {
  add_row(existing_table,
          presidency = pres_group_title,
          first_day = NA,
          last_day = NA,
          days_in_office = NA,
          years_in_office = NA
  )
}

#create the core of the left edge of the table
pt.leftbuild <- pt.study %>%
  select(presidency, first_day, last_day, days_in_office) %>%
  mutate(years_in_office = days_in_office / 365)

# add political lean (left/right)
pt.leftbuild <- pt.leftbuild %>%
  add_pres_title_row("Political lean") %>%
  add_pres_row(pres_group_title = "Right", pt.right) %>%
  add_pres_row(pres_group_title = "Left", pt.left)

# add political lean (period)
pt.leftbuild <- pt.leftbuild %>%
  add_pres_title_row("Political period") %>%
  add_pres_row(pres_group_title = "UDP", pt.udp) %>%
  add_pres_row(pres_group_title = "Neoliberal", pt.neolib) %>%
  add_pres_row(pres_group_title = "MAS-IPSP", pt.mas) %>%
  add_pres_row(pres_group_title = "Right Interim", pt.righti)

# stack the outcome tables together vertically
pt.rightbuild <- bind_rows(pres.categorized, pres.categorized.lean) %>%
                 bind_rows(pres.categorized.lean2)

pt.complete <- left_join(pt.leftbuild, pt.rightbuild, c("presidency"="pres_admin"))

pt.complete <- pt.complete %>%
  mutate(
    n_peryear = n / years_in_office,
    sp_peryear = n_state_perp / years_in_office,
    sv_peryear = n_state_victim / years_in_office,
    sep_peryear = n_state_separate / years_in_office
  )

pt.summary <- pt.complete %>%
  select(
    presidency, first_day, last_day, days_in_office, n, n_peryear, n_state_perp, sp_peryear, n_state_victim, sv_peryear, n_state_separate, sep_peryear) %>%
  mutate(across(c(days_in_office, n, n_state_perp, n_state_victim, n_state_separate), as.integer)) 
# %>%
#     mutate(across(c(first_day, last_day), as.Date))

pt.summary.partisanship <- slice_tail(pt.summary, n= 8) %>% select(-first_day, -last_day)

pt.summary.administration <- slice_head(pt.summary, n= nrow(pt.study))