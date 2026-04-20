test_that("muni_list_with_counts_by", {
  expect_equal(muni_list_with_counts_by(head(deaths_aug24, 17)),
        structure(list(muni_text = c("San Julián, Santa Cruz", "Sacaba, Cochabamba",
                     "San Ignacio de Velasco, Santa Cruz", "La Paz, La Paz",
                    "Arbieto, Cochabamba",  "Collana, La Paz", "Riberalta, Beni",
                    "Santa Cruz de la Sierra, Santa Cruz", "Sipe Sipe, Cochabamba"),
                    count = c(4L, 3L, 3L, 2L, 1L, 1L, 1L,  1L, 1L)),
                    class = c("tbl_df", "tbl", "data.frame"),
                  row.names = c(NA,   -9L))
     )
  expect_snapshot(muni_list_with_counts_by(head(deaths_aug24, 100)))
  expect_equal(muni_list_with_counts_by(tail(deaths_aug24, 10), intentionality),
               structure(list(intentionality =
                     c("Collateral", "Collateral", "Collateral", "Direct",
                                  "Direct", "Direct", "Direct", "Direct", "Direct"),
                  muni_text = c("Villa Tunari, Cochabamba", "La Paz, La Paz",
                               "Yapacaní, Santa Cruz", "Apolo, La Paz",
                               "Ascensión de Guarayos, Santa Cruz",
                               "Cochabamba, Cochabamba", "Cotoca, Santa Cruz",
                               "Guanay, La Paz", "Sorata, La Paz"),
                  count = c(2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)),
                  class = c("tbl_df", "tbl", "data.frame"),
                  row.names = c(NA, -9L)))
  expect_snapshot(muni_list_with_counts_by(head(deaths_aug24, 200),
                                           weapon, intentionality))
})
