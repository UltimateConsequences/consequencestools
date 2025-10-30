# relabel_pres_admin works

    Code
      def_labeled
    Output
      # A tibble: 670 x 52
         event_title    unconfirmed  year month   day later_year later_month later_day
         <chr>          <lgl>       <int> <int> <int>      <int>       <int>     <int>
       1 Shooting at S~ NA           1984    10    26         NA          NA        NA
       2 Shooting at S~ NA           1984    10    26         NA          NA        NA
       3 Shooting at S~ NA           1984    10    26         NA          NA        NA
       4 Shooting at S~ NA           1984    10    26         NA          NA        NA
       5 Killing of ha~ NA           1984    10    28         NA          NA        NA
       6 Huayllani roa~ NA           1985     6     3         NA          NA        NA
       7 Huayllani roa~ NA           1985     6     3         NA          NA        NA
       8 Huayllani roa~ TRUE         1985     6     3         NA          NA        NA
       9 COB education~ NA           1986     4     8         NA          NA        NA
      10 COB education~ NA           1986     4     8       1986           4        10
      # i 660 more rows
      # i 44 more variables: dec_firstname <chr>, dec_surnames <chr>, id_indiv <chr>,
      #   dec_age <dbl>, dec_alt_age <chr>, dec_gender <chr>, dec_residence <chr>,
      #   dec_nationality <chr>, dec_affiliation <chr>, dec_spec_affiliation <chr>,
      #   dec_title <chr>, cause_death <chr>, munition <chr>, weapon <chr>,
      #   va_notes <chr>, perp_category <chr>, perp_group <chr>,
      #   perp_pol_stalemate <chr>, dec_pol_stalemate <chr>, ...

---

    Code
      def_labeled2
    Output
      # A tibble: 670 x 52
         event_title    unconfirmed  year month   day later_year later_month later_day
         <chr>          <lgl>       <int> <int> <int>      <int>       <int>     <int>
       1 Shooting at S~ NA           1984    10    26         NA          NA        NA
       2 Shooting at S~ NA           1984    10    26         NA          NA        NA
       3 Shooting at S~ NA           1984    10    26         NA          NA        NA
       4 Shooting at S~ NA           1984    10    26         NA          NA        NA
       5 Killing of ha~ NA           1984    10    28         NA          NA        NA
       6 Huayllani roa~ NA           1985     6     3         NA          NA        NA
       7 Huayllani roa~ NA           1985     6     3         NA          NA        NA
       8 Huayllani roa~ TRUE         1985     6     3         NA          NA        NA
       9 COB education~ NA           1986     4     8         NA          NA        NA
      10 COB education~ NA           1986     4     8       1986           4        10
      # i 660 more rows
      # i 44 more variables: dec_firstname <chr>, dec_surnames <chr>, id_indiv <chr>,
      #   dec_age <dbl>, dec_alt_age <chr>, dec_gender <chr>, dec_residence <chr>,
      #   dec_nationality <chr>, dec_affiliation <chr>, dec_spec_affiliation <chr>,
      #   dec_title <chr>, cause_death <chr>, munition <chr>, weapon <chr>,
      #   va_notes <chr>, perp_category <chr>, perp_group <chr>,
      #   perp_pol_stalemate <chr>, dec_pol_stalemate <chr>, ...

