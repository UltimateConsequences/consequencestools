# clean_dates(): Notes are functional

    Code
      date_notes
    Output
      # A tibble: 18 x 4
         day_notes      month_notes     year_notes         n
         <chr>          <chr>           <chr>          <int>
       1 Unknown day    "Unknown month" "Unknown year"    36
       2 Unknown day    "Unknown month" ""                24
       3 Unknown day    "Unknown month" ""                24
       4 Unknown day    "Unknown month" ""                24
       5 19 or 20 or 21 ""              ""                14
       6 30 or unknown  ""              ""                13
       7 Unknown day    ""              ""                11
       8 Unknown day    ""              ""                11
       9 Unknown day    ""              ""                11
      10 Unknown day    ""              ""                11
      11 Unknown day    ""              ""                11
      12 Unknown day    ""              ""                11
      13 Unknown day    ""              ""                11
      14 Unknown day    ""              ""                11
      15 Unknown day    ""              ""                11
      16 Unknown day    ""              ""                11
      17 5 or 6         ""              ""                 6
      18 5 or 6         ""              ""                 6

# clean_dates(): Correct by snapshot test

    Code
      clean_dates(de.raw_datenotes)
    Output
      # A tibble: 31 x 14
         event_title       id_indiv  year month   day later_year later_month later_day
         <chr>             <chr>    <int> <int> <int>      <int>       <int>     <int>
       1 TIPNIS landowner~ i10012    1990     5    NA         NA          NA        NA
       2 Sucre assassinat~ i11002    1991     4    30       1992          11         9
       3 UMOPAR 1992       i12002    1992     6    NA         NA          NA        NA
       4 Laymi-Qaqachaka ~ i15002    1995     6    NA         NA          NA        NA
       5 Laymi-Qaqachaka ~ i15003    1995     6    NA         NA          NA        NA
       6 Laymi-Qaqachaka ~ i15004    1995     6    NA         NA          NA        NA
       7 Chayanta mining ~ i16021    1996    12    NA       1996          12        24
       8 Cocalero Death D~ i16022    1996    NA    NA         NA          NA        NA
       9 Laymi-Qaqachaka ~ i17001    1997     3    NA         NA          NA        NA
      10 Laymi-Qaqachaka ~ i17002    1997     3    NA         NA          NA        NA
      # i 21 more rows
      # i 6 more variables: day_notes <chr>, month_notes <chr>, year_notes <chr>,
      #   later_day_notes <chr>, later_month_notes <chr>, later_year_notes <chr>

