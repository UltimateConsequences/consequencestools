test_that("domain_filename works", {
  domain_list <- c("Gas wars", "Economic policies", "Labor", "Education", "Mining",
                   "Coca", "Peasant", "Rural land",
                   "Ethno-ecological", "Urban land", "Drug trade", "Contraband",
                   "Municipal governance", "Local development", "National governance",
                   "Partisan politics", "Disabled", "Guerrilla", "Paramilitary",
                   "Unknown")
  desired_links <- c("/domain/gas-wars.html", "/domain/economic-policies.html",
                     "/domain/labor.html", "/domain/education.html", "/domain/mining.html",
                     "/domain/coca.html", "/domain/peasant.html", "/domain/rural-land.html",
                     "/domain/ethno-ecological.html", "/domain/urban-land.html",
                     "/domain/drug-trade.html", "/domain/contraband.html",
                     "/domain/municipal-governance.html",
                     "/domain/local-development.html", "/domain/national-governance.html",
                     "/domain/partisan-politics.html", "/domain/disabled.html",
                     "/domain/guerrilla.html",
                     "/domain/paramilitary.html", "/domain/unknown.html")
  expect_equal(domain_filename(domain_list), desired_links)
})
test_that("domain_filename handles commas", {
  expect_equal(domain_filename("Rural land, Partisan politics"), "")
  expect_equal(domain_filename_es("Campesino, Politica partidaria"), "")
})



