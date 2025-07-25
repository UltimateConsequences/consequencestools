anexo_municipios <- readr::read_csv(here::here("data-raw", "anexo_municipios_de_bolivia.csv"))
usethis::use_data(anexo_municipios, overwrite = TRUE)

muni_id_lookup_table <- readr::read_rds(here::here("data-raw", "muni_id_lookup_table.rds"))
usethis::use_data(muni_id_lookup_table, overwrite = TRUE)

# This code produces a substantial lookup table for changes needed between
# our standard municipalities and gb2014.
muni_gb2014_conversion <- muni_id_lookup_table %>%
  filter(muni_gb2014 != muni_anexo) %>%
  select(1, 3, 2) %>%
  mutate(recode = str_glue("mutate(municipality = recode(municipality, \"{muni_anexo}\"",
                           " = ",
                           "\"{muni_gb2014}\")) %>% "))
# muni_gb2014_conversion$recode
usethis::use_data(muni_gb2014_conversion, overwrite = TRUE)

muni_gb2014_lookup <- muni_id_lookup_table %>%
  select(id_muni, muni_gb2014, department) %>%
  rename(municipality = muni_gb2014)
usethis::use_data(muni_gb2014_lookup, overwrite = TRUE)
