president <- list()

president$levels <- c(
  "Hernán Siles Zuazo", "Víctor Paz Estenssoro", "Jaime Paz Zamora",
  "Gonzalo Sanchez de Lozada (1st)", "Hugo Banzer (2nd)", "Jorge Quiroga",
  "Gonzalo Sanchez de Lozada (2nd)", "Carlos Diego Mesa Gisbert",
  "Eduardo Rodríguez", "Evo Morales", "Interim military government",
  "Jeanine Áñez", "Luis Arce")
president$initials <- c(
  "HSZ", "VPE", "JPZ",
  "GSL", "HB", "JQ",
  "GSL", "CM",
  "ER", "EM", "Mil",
  "JA", "LA")

usethis::use_data(president)
