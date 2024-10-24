# Results table is consistent with past runs

    Code
      n_categorized_by(deaths_aug24, pres_admin, complete = FALSE)
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
      # i 10 more variables: n_state_perp_armedactor <int>, n_state_victim <int>,
      #   n_state_victim_coca <int>, n_state_victim_armedactor <int>,
      #   n_state_separate <int>, n_state_perp_ordinary <int>,
      #   n_state_victim_ordinary <int>, n_remaining <int>, n_remaining_coca <int>,
      #   n_remaining_armedactor <int>

---

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
      # i 10 more variables: n_state_perp_armedactor <int>, n_state_victim <int>,
      #   n_state_victim_coca <int>, n_state_victim_armedactor <int>,
      #   n_state_separate <int>, n_state_perp_ordinary <int>,
      #   n_state_victim_ordinary <int>, n_remaining <int>, n_remaining_coca <int>,
      #   n_remaining_armedactor <int>

---

    Code
      n_categorized_by(deaths_aug24, pres_admin, complete = TRUE, sp_binary = TRUE)
    Output
      # A tibble: 12 x 16
         pres_admin               n n_coca n_armedactor n_state_perp n_state_perp_coca
         <chr>                <int>  <int>        <int>        <int>             <int>
       1 Carlos Diego Mesa G~    19      7            1            4                 3
       2 Evo Morales            149     12            3           36                 4
       3 Gonzalo Sanchez de ~    52     24            0           33                19
       4 Gonzalo Sanchez de ~   146     17            0          114                 9
       5 Hernán Siles Zuazo       8      3            0            3                 3
       6 Hugo Banzer (2nd)      122     37            0           39                20
       7 Interim military go~     9      0            0            6                 0
       8 Jaime Paz Zamora        22     10            7           18                10
       9 Jeanine Áñez            26      0            0           25                 0
      10 Jorge Quiroga           32     18            0           16                13
      11 Luis Arce               42      1            0            2                 0
      12 Víctor Paz Estensso~    43     18            3           33                18
      # i 10 more variables: n_state_perp_armedactor <int>, n_state_victim <int>,
      #   n_state_victim_coca <int>, n_state_victim_armedactor <int>,
      #   n_state_separate <int>, n_state_perp_ordinary <int>,
      #   n_state_victim_ordinary <int>, n_remaining <int>, n_remaining_coca <int>,
      #   n_remaining_armedactor <int>

---

    Code
      n_responsibility_by(deaths_aug24, cause_death, complete = TRUE)
    Output
      # A tibble: 60 x 5
         cause_death                 n n_state_perp n_state_victim n_state_separate
         <chr>                   <int>        <int>          <int>            <int>
       1 Assault                     1            0              0                1
       2 Assault, whipping, rape     1            0              0                1
       3 Beating                    38           11              8               17
       4 Beating / Impact            2            0              0                2
       5 Beating and Gunshot         1            0              0                1
       6 Beating, burning            1            0              1                0
       7 Beating, torture            1            1              0                0
       8 Beating/tear gas            1            0              0                1
       9 COVID / Beating             1            0              0                1
      10 Cable cut through neck      1            0              0                0
      # i 50 more rows

