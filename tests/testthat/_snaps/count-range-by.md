# Results table is consistent with past runs

    Code
      count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, protest_domain)
    Output
      # A tibble: 20 x 12
         protest_domain     n n_unfiltered n_state_perp n_state_perp_hi n_state_victim
         <chr>          <int>        <int>        <int>           <int>          <int>
       1 Coca             138          147           90             103             36
       2 Contraband         3           NA            3              NA              0
       3 Disabled           3            6            0              NA              0
       4 Drug trade        10           11            6               7              1
       5 Economic poli~    41           NA           34              35              6
       6 Education         28           NA            7              10              0
       7 Ethno-ecologi~     2            8            0              NA              0
       8 Gas Wars          71           72           60              61              2
       9 Guerrilla         10           NA            5               6              1
      10 Labor             29           30            9              21              1
      11 Local develop~     6           NA            4              NA              0
      12 Mining            69           70           21              22              9
      13 Municipal gov~     9           10            2               3              1
      14 National gove~     1           NA            0               1              0
      15 Paramilitary       4           NA            3              NA              1
      16 Partisan Poli~    73           77           33              36              3
      17 Peasant           31           32           16              18              2
      18 Rural Land        82           97            2              NA              0
      19 Rural Land, P~     2           NA            0              NA              0
      20 Urban land        13           NA            2              NA              0
      # i 6 more variables: n_state_victim_hi <int>, n_state_separate <int>,
      #   n_state_separate_hi <int>, n_unconfirmed <int>, n_collateral <int>,
      #   n_nonconflict <int>

---

    Code
      count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department)
    Output
      # A tibble: 9 x 12
        department     n n_unfiltered n_state_perp n_state_perp_hi n_state_victim
        <chr>      <int>        <int>        <int>           <int>          <int>
      1 Beni          18           21            5               8              1
      2 Chuquisaca     9           10            8              NA              1
      3 Cochabamba   168          185          112             126             30
      4 La Paz       242          249          128             148             25
      5 Oruro         59           69           12              NA              1
      6 Pando         14           NA            2              NA              1
      7 Potosí        51           53           13              14              1
      8 Santa Cruz    56           60           15              17              3
      9 Tarija         9           NA            2              NA              0
      # i 6 more variables: n_state_victim_hi <int>, n_state_separate <int>,
      #   n_state_separate_hi <int>, n_unconfirmed <int>, n_collateral <int>,
      #   n_nonconflict <int>

---

    Code
      count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department,
        drop_separate = TRUE)
    Output
      # A tibble: 9 x 10
        department     n n_unfiltered n_state_perp n_state_perp_hi n_state_victim
        <chr>      <int>        <int>        <int>           <int>          <int>
      1 Beni          18           21            5               8              1
      2 Chuquisaca     9           10            8              NA              1
      3 Cochabamba   168          185          112             126             30
      4 La Paz       242          249          128             148             25
      5 Oruro         59           69           12              NA              1
      6 Pando         14           NA            2              NA              1
      7 Potosí        51           53           13              14              1
      8 Santa Cruz    56           60           15              17              3
      9 Tarija         9           NA            2              NA              0
      # i 4 more variables: n_state_victim_hi <int>, n_unconfirmed <int>,
      #   n_collateral <int>, n_nonconflict <int>

---

    Code
      count_range_by(deaths_aug24_filtered, deaths_aug24_unfiltered, department,
        drop_separate = TRUE, drop_extra = TRUE)
    Output
      # A tibble: 9 x 6
        department     n n_state_perp n_state_perp_hi n_state_victim n_state_victim_hi
        <chr>      <int>        <int>           <int>          <int>             <int>
      1 Beni          18            5               8              1                NA
      2 Chuquisaca     9            8              NA              1                NA
      3 Cochabamba   168          112             126             30                31
      4 La Paz       242          128             148             25                NA
      5 Oruro         59           12              NA              1                 3
      6 Pando         14            2              NA              1                NA
      7 Potosí        51           13              14              1                NA
      8 Santa Cruz    56           15              17              3                NA
      9 Tarija         9            2              NA              0                NA

