lev <- list()

president <- list()

president$title <- "Presidential Administration"
president$r_variable <- "pres_admin"
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
president$id_presidency <- c(
  "p101", "p102", "p103",
  "p104", "p105", "p106",
  "p107", "p108", "p109",
  "p110", "p111", "p112",
  "p113")

lev$pres_admin <- president

usethis::use_data(president, overwrite=TRUE)

location_precision <- list()
location_precision$title <- "Location Precision"
location_precision$r_variable <- "location_precision"
location_precision$levels <- c(
  "address", "poi_small", "intersection",
  "block", "poi_large", "road", "community",
  "town", "rural_zone",
  "municipality", "province",
  "region", "department")

lev$location_precision <- location_precision

state_resp <- list()
state_resp$title <- "State Responsibility"
state_resp$r_variable <- "state_responsibility"
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

lev$state_responsibility <- state_resp
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
    "Mining" = mat_color_hex[['brown-500']],
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
    "Guerrilla" = mat_color_hex[['pink-400']],
    "Paramilitary" = mat_color_hex[['pink-800']],
    "Unknown" = mat_color_hex[['grey-300']]
  )
}

protest_domains <- list()
protest_domains$title <- "Protest Domain"
protest_domains$r_variable <- "protest_domain"
protest_domains$levels <- protest_domain.grouped
protest_domains$colors <- assign_protest_domain.colors()

lev$protest_domain <- protest_domain
usethis::use_data(protest_domains, overwrite=TRUE)

# Affiliations

affiliation.grouped <- c(
  "Factory Worker",               # labor
  "Transport Worker",
  "Student",
  "Teacher",
  "Miner",                        # mining
  "Cocalero",                     # rural/indigenous
  "Campesino",
  "Highland Indigenous",
  "Lowland Indigenous",
  "Landowner",
  "Urban Movement",
  "Government Employee",          # governance
  "Government Officeholder",
  "Partisan",
  "Narcotrafficker",              # armed
  "Armed Actor",
  "Security Force",
  "Protester",
  "Journalist",                   # civilians
  "Civilian",
  "Unknown"
)

affiliation.grouped_es <- c(
  "Trabajador de fábrica",               # labor
  "Trabajador del transporte",
  "Estudiante",
  "Docente",
  "Minero",                        # mining
  "Cocalero",                     # rural/indigenous
  "Campesino",
  "Indígena de tierras altas",
  "Indígena de tierras bajas",
  "Terrateniente",
  "Movimiento urbano",
  "Empleado público",          # governance
  "Funcionario público",
  "Partidista",
  "Narcotraficante",              # armed
  "Actor armado",
  "Fuerza de seguridad",
  "Manifestante",
  "Periodista",                   # civilians
  "Civil",
  "Desconocido"
)

assign_affiliation.colors <- function() {
  c(
    #"Gas wars" = mat_color_hex[['blue-900']],
    #"Economic policies" = mat_color_hex[['blue-700']],
    #"Labor" = mat_color_hex[['blue-500']],
    "Factory Worker" = mat_color_hex[['blue-300']],
    "Transport Worker" = mat_color_hex[['blue-100']],

    # "Education" = mat_color_hex[['light blue-900']],
    "Student" = mat_color_hex[['light blue-700']],
    "Teacher" = mat_color_hex[['light blue-500']],

    "Miner" = mat_color_hex[['brown-500']],
    # "Mining" = mat_color_hex[['red-700']],

    "Cocalero" = mat_color_hex[['green-700']],
    #"Coca" = mat_color_hex[['green-900']],

    "Campesino" = mat_color_hex[['green-300']],
    # "Peasant" = mat_color_hex[['green-500']],

    "Highland Indigenous" = mat_color_hex[['light green-700']],
    #"Rural land" = mat_color_hex[['light green-500']],
    #"Rural land, Partisan politics" = mat_color_hex[['light green-300']],

    "Lowland Indigenous" = mat_color_hex[['teal-500']],
    #"Ethno-ecological" = mat_color_hex[['teal-300']],
    "Landowner" = mat_color_hex[['cyan-500']],

    "Government Employee" = mat_color_hex[['deep purple-300']],

    #"Municipal governance" = mat_color_hex[['deep purple-900']],
    "Government Officeholder" = mat_color_hex[['deep purple-700']],
    #"Local development" = mat_color_hex[['deep purple-500']],

    "Partisan" = mat_color_hex[['pink-500']],
    #"Partisan politics" = mat_color_hex[['pink-300']],

    #"Disabled" = mat_color_hex[['blue grey-600']],

    "Narcotrafficker" = mat_color_hex[['lime-500']],
    #"Drug trade" = mat_color_hex[['lime-700']],
    #"Contraband" = mat_color_hex[['lime-300']],

    "Armed Actor" = mat_color_hex[['pink-600']],
    #"Guerrilla" = mat_color_hex[['brown-800']],
    #"Paramilitary" = mat_color_hex[['brown-400']],

    "Security Force" = mat_color_hex[['red-800']],

    #"Urban land" = mat_color_hex[['yellow-700']],
    "Urban Movement" = mat_color_hex[['yellow-500']],

    "Protester" = mat_color_hex[['amber-500']],

    "Journalist" = mat_color_hex[['indigo-500']],
    "Civilian" = mat_color_hex[['blue grey-600']],
    "Unknown" = mat_color_hex[['grey-300']]
  )
}

affiliations <- list()
affiliations$title <- "Affiliation"
affiliations$r_variable <- "affiliation"
affiliations$levels <- affiliation.grouped
affiliations$levels_es <- affiliation.grouped_es
affiliations$colors <- assign_affiliation.colors()

usethis::use_data(affiliations, overwrite = TRUE)

lev$dec_affiliation <- affiliations
lev$dec_affiliation$title <- "Deceased Affiliation"
lev$dec_affiliation$r_variable <- "dec_affiliation"
lev$perp_affiliation <- affiliations
lev$perp_affiliation$title <- "Perpetrator Affiliation"
lev$perp_affiliation$r_variable <- "perp_affiliation"

departments <- list()

departments$title <- "Department"
departments$r_variable <- "department"
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

lev$department <- departments

usethis::use_data(departments, overwrite = TRUE)

standard_factoring_variables <- c(
  "pres_admin",
  "state_responsibility",
  "state_perpetrator",
  "protest_domain",
  "dec_affiliation",
  "perp_affiliation",
  "location_precision"
)

usethis::use_data(standard_factoring_variables)

usethis::use_data(lev, overwrite = TRUE)
