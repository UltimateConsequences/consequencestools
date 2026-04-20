# muni_list_with_counts_by

    Code
      muni_list_with_counts_by(head(deaths_aug24, 100))
    Output
      # A tibble: 34 x 2
         muni_text                  count
         <chr>                      <int>
       1 Villa Tunari, Cochabamba      22
       2 La Paz, La Paz                15
       3 Sipe Sipe, Cochabamba          5
       4 Potosí, Potosí                 4
       5 San Julián, Santa Cruz         4
       6 Santa Ana del Yacuma, Beni     4
       7 Entre Ríos, Cochabamba         3
       8 Huatajata, La Paz              3
       9 NA, Cochabamba                 3
      10 NA, Oruro                      3
      # i 24 more rows

---

    Code
      muni_list_with_counts_by(head(deaths_aug24, 200), weapon, intentionality)
    Output
      # A tibble: 83 x 4
         weapon intentionality muni_text                     count
         <chr>  <chr>          <chr>                         <int>
       1 Bullet Direct         Villa Tunari, Cochabamba         26
       2 <NA>   Direct         Challapata, Oruro                26
       3 Bullet Direct         La Paz, La Paz                   10
       4 Bullet Direct         Chayanta, Potosí                  6
       5 <NA>   Direct         NA, Potosí                        5
       6 Bullet Direct         Puerto Villarroel, Cochabamba     4
       7 Bullet Direct         Santa Ana del Yacuma, Beni        4
       8 None   Direct         Villa Tunari, Cochabamba          4
       9 <NA>   Direct         Chayanta, Potosí                  4
      10 Bullet Direct         Caripuyo, Potosí                  3
      # i 73 more rows

