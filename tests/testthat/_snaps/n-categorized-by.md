# Results table is consistent with past runs

    Code
      n_categorized_by(deaths_aug24, pres_admin, complete = TRUE)
    Output
      # A tibble: 12 x 16
         pres_admin               n n_coca n_armedactor n_state_perp n_state_perp_coca
         <chr>                <int>  <int>        <int>        <int>             <int>
       1 Carlos Diego Mesa G~    19      7            1            3                 2
       2 Evo Morales            149     12            3           34                 4
       3 Gonzalo Sanchez de ~    52     24            0           32                18
       4 Gonzalo Sanchez de ~   146     17            0          102                 9
       5 Hernán Siles Zuazo       8      3            0            3                 3
       6 Hugo Banzer (2nd)      122     37            0           37                18
       7 Interim military go~     9      0            0            4                 0
       8 Jaime Paz Zamora        22     10            7           17                10
       9 Jeanine Áñez            26      0            0           24                 0
      10 Jorge Quiroga           32     18            0           16                13
      11 Luis Arce               42      1            0            1                 0
      12 Víctor Paz Estensso~    43     18            3           30                18
      # i 10 more variables: n_state_perp_armedactor <int>,
      #   n_state_perp_ordinary <int>, n_state_victim <int>,
      #   n_state_victim_coca <int>, n_state_victim_armedactor <int>,
      #   n_state_victim_ordinary <int>, n_state_separate <int>, n_remaining <int>,
      #   n_remaining_coca <int>, n_remaining_armedactor <int>

