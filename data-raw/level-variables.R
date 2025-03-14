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
president$levels_es <- c(
  "Hernán Siles Zuazo", "Víctor Paz Estenssoro", "Jaime Paz Zamora",
  "Gonzalo Sanchez de Lozada (1ro)", "Hugo Banzer (2do)", "Jorge Quiroga",
  "Gonzalo Sanchez de Lozada (2do)", "Carlos Diego Mesa Gisbert",
  "Eduardo Rodríguez", "Evo Morales", "Interim military government",
  "Jeanine Áñez", "Luis Arce")

usethis::use_data(president, overwrite=TRUE)

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
  Victima = "#cd6600",                  # "darkorange3",
  Involucrado = "#90ee90",                # "lightgreen",
  Separado = "#eeb422",                # "goldenrod2",
  "No Intencional" = "darkgray",
  Desconocido = "lightgray")

usethis::use_data(state_resp, overwrite=TRUE)

mat_color <- readr::read_csv("data-raw/MaterialColour.csv")

mat_color_hex <- mat_color$hexvalue
names(mat_color_hex) <- mat_color$name

protest_domain.grouped <- c(
  "Gas wars",                         # Economic
  "Economic policies",
  "Labor",
  "Education",
  "Mining",
  "Coca",                             # Rural
  "Peasant",
  "Rural land",
  "Rural land, Partisan politics",
  "Ethno-ecological",
  "Urban land",
  "Drug trade",                       # Criminalized
  "Contraband",
  "Municipal governance",
  "Local development",                # Local
  "National governance",
  "Partisan politics",                # (solo)
  "Disabled",                         # (solo)
  "Guerrilla",                        # Armed actors
  "Paramilitary",
  "Unknown")                       # (solo)

assign_protest_domain.colors <- function() {
  c(
    "Gas wars" = mat_color_hex[['blue-900']],
    "Economic policies" = mat_color_hex[['blue-700']],
    "Labor" = mat_color_hex[['blue-500']],
    "Education" = mat_color_hex[['blue-200']],
    "Mining" = mat_color_hex[['red-700']],
    "Coca" = mat_color_hex[['green-900']],
    "Peasant" = mat_color_hex[['green-700']],
    "Rural land" = mat_color_hex[['green-500']],
    "Rural land, Partisan politics" = mat_color_hex[['green-400']],
    "Ethno-ecological" = mat_color_hex[['teal-400']],
    "Urban land" = mat_color_hex[['yellow-700']],
    "Drug trade" = mat_color_hex[['lime-700']],
    "Contraband" = mat_color_hex[['lime-400']],
    "Municipal governance" = mat_color_hex[['deep purple-900']],
    "Local development" = mat_color_hex[['deep purple-600']],
    "National governance" = mat_color_hex[['deep purple-300']],
    "Partisan politics" = mat_color_hex[['orange-500']],
    "Disabled" = mat_color_hex[['blue grey-600']],
    "Guerrilla" = mat_color_hex[['brown-800']],
    "Paramilitary" = mat_color_hex[['brown-400']],
    "Unknown" = mat_color_hex[['grey-300']]
  )
}

protest_domain <- list()
protest_domain$title <- "Protest Domain"
protest_domain$levels <- protest_domain.grouped
protest_domain$colors <- assign_protest_domain.colors()

usethis::use_data(protest_domain)

departments <- list()

departments$title <- "Department"

departments$levels <- c("Beni", "Chuquisaca", "Cochabamba", "La Paz", "Oruro", "Pando",
                        "Potosí", "Santa Cruz", "Tarija", "Unknown")
departments$levels_es <- c("Beni", "Chuquisaca", "Cochabamba", "La Paz", "Oruro", "Pando",
                        "Potosí", "Santa Cruz", "Tarija", "Desconocido")

assign_department_colors <- function() {
  c(
    "Beni" = mat_color_hex[['green-800']],
    "Chuquisaca" = mat_color_hex[['blue-800']],
    "Cochabamba" = mat_color_hex[['blue-400']],
    "La Paz" = mat_color_hex[['orange-500']],
    "Oruro" = mat_color_hex[['red-700']],
    "Pando" = mat_color_hex[['green-400']],
    "Potosí" = mat_color_hex[['red-400']],
    "Santa Cruz" = mat_color_hex[['lime-700']],
    "Tarija" = mat_color_hex[['pink-500']],
    "Unknown" = mat_color_hex[['grey-300']])
}
departments$colors <- assign_department_colors()

usethis::use_data(departments, overwrite = TRUE)
