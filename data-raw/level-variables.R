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

location_precision <- list()
location_precision$levels <- c(
  "address", "poi_small", "intersection",
  "block", "poi_large", "road", "community",
  "town", "rural_zone",
  "municipality", "province",
  "region", "department")

usethis::use_data(location_precision)

state_resp <- list()
state_resp$levels <- c("Perpetrator", "Victim", "Involved", "Separate", "Unintentional", "Unknown")
state_resp$colors <-  c(
  Perpetrator = "forestgreen",
  Victim = "#cd6600",                  # "darkorange3",
  Involved = "#90ee90",                # "lightgreen",
  Separate = "#eeb422",                # "goldenrod2",
  Unintentional = "darkgray",
  Unknown = "lightgray")

state_resp$colors_es <-  c(
  Perpetrador = "forestgreen",
  Víctima = "#cd6600",                  # "darkorange3",
  Involucrado = "#90ee90",                # "lightgreen",
  Separado = "#eeb422",                # "goldenrod2",
  "No Intencional" = "darkgray",
  Desconocido = "lightgray")

usethis::use_data(state_resp)
