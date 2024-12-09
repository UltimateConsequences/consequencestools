# Results table is consistent with past runs

    Code
      event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered)
    Output
      # A tibble: 214 x 12
         event_title        n n_unfiltered n_state_perp n_state_perp_hi n_state_victim
         <chr>          <int>        <int>        <int>           <int>          <int>
       1 Achocalla lan~     1           NA            0              NA              0
       2 Achumani land~     1           NA            0              NA              0
       3 Altiplano blo~     2           NA            2              NA              0
       4 Ancoraimes Co~     1           NA            0              NA              0
       5 Anti-privatiz~     1           NA            0               1              0
       6 Apolo coca er~     4           NA            0              NA              4
       7 Arani school ~     1           NA            1              NA              0
       8 Arcopongo Min~     3           NA            0              NA              0
       9 Arcopongo Min~     1           NA            0              NA              0
      10 Asunta coca c~     3           NA            2              NA              1
      # i 204 more rows
      # i 6 more variables: n_state_victim_hi <int>, n_state_separate <int>,
      #   n_state_separate_hi <int>, n_unconfirmed <int>, n_collateral <int>,
      #   n_nonconflict <int>

---

    Code
      event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
        drop_separate = TRUE)
    Output
      # A tibble: 214 x 10
         event_title        n n_unfiltered n_state_perp n_state_perp_hi n_state_victim
         <chr>          <int>        <int>        <int>           <int>          <int>
       1 Achocalla lan~     1           NA            0              NA              0
       2 Achumani land~     1           NA            0              NA              0
       3 Altiplano blo~     2           NA            2              NA              0
       4 Ancoraimes Co~     1           NA            0              NA              0
       5 Anti-privatiz~     1           NA            0               1              0
       6 Apolo coca er~     4           NA            0              NA              4
       7 Arani school ~     1           NA            1              NA              0
       8 Arcopongo Min~     3           NA            0              NA              0
       9 Arcopongo Min~     1           NA            0              NA              0
      10 Asunta coca c~     3           NA            2              NA              1
      # i 204 more rows
      # i 4 more variables: n_state_victim_hi <int>, n_unconfirmed <int>,
      #   n_collateral <int>, n_nonconflict <int>

---

    Code
      event_counts_table(deaths_aug24_filtered, deaths_aug24_unfiltered,
        drop_separate = TRUE, drop_extra = TRUE)
    Output
      # A tibble: 214 x 6
         event_title                     n n_state_perp n_state_perp_hi n_state_victim
         <chr>                       <int>        <int>           <int>          <int>
       1 Achocalla land dispute          1            0              NA              0
       2 Achumani land clash             1            0              NA              0
       3 Altiplano blockades June 2~     2            2              NA              0
       4 Ancoraimes Councilwoman As~     1            0              NA              0
       5 Anti-privatization miner d~     1            0               1              0
       6 Apolo coca eradication          4            0              NA              4
       7 Arani school transfer road~     1            1              NA              0
       8 Arcopongo Mine clash            3            0              NA              0
       9 Arcopongo Mine clash 2018       1            0              NA              0
      10 Asunta coca conflict            3            2              NA              1
      # i 204 more rows
      # i 1 more variable: n_state_victim_hi <int>

# Event description table is consistent with past runs

    Code
      event_description_table(deaths_aug24)
    Output
      # A tibble: 214 x 4
         event_title                            year protest_domain pres_admin        
         <chr>                                 <int> <chr>          <chr>             
       1 Shooting at San Julián blockade        1984 Peasant        Hernán Siles Zuazo
       2 Killing of hacienda owner              1984 Peasant        Hernán Siles Zuazo
       3 Huayllani roadblock                    1985 Coca           Hernán Siles Zuazo
       4 COB education protest                  1986 Education      Víctor Paz Estens~
       5 Huanchaca narco-killings               1986 Drug trade     Víctor Paz Estens~
       6 FRI Deputy assassinated                1986 Drug trade     Víctor Paz Estens~
       7 Beni rural labor leader assassinated   1987 Labor          Víctor Paz Estens~
       8 Parotani Triennial Plan protest        1987 Coca           Víctor Paz Estens~
       9 Education strike 1987                  1987 Education      Víctor Paz Estens~
      10 Infant deaths in mining hunger strike  1987 Labor          Víctor Paz Estens~
      # i 204 more rows

