# event_list_with_counts_by(): Results table is consistent with past runs

    Code
      event_list_with_counts_by(deaths_aug24, department)
    Output
      # A tibble: 231 x 3
         department event_title                             count
         <chr>      <chr>                                   <int>
       1 La Paz     Gas War 2003                               70
       2 La Paz     Tarifazo tax protest                       33
       3 Oruro      Laymi-Qaqachaka 2000                       30
       4 Cochabamba Plan Dignity 1998                          23
       5 Oruro      Huanuni clashes                            17
       6 Potosí     Laymi-Qaqachaka 1998                       14
       7 Cochabamba Villa Tunari Massacre                      12
       8 La Paz     Bus crash during miners pension protest    11
       9 La Paz     Senkata Massacre                           11
      10 Pando      Porvenir Massacre                          11
      # i 221 more rows

---

    Code
      event_list_with_counts_by(deaths_aug24, protest_domain, protest_campaign)
    Output
      # A tibble: 214 x 4
         protest_domain    protest_campaign           event_title                count
         <chr>             <chr>                      <chr>                      <int>
       1 Gas Wars          2003 Gas War               Gas War 2003                  71
       2 Rural Land        Laymi-Qaqachaka            Laymi-Qaqachaka 2000          37
       3 Economic policies Tarifazo                   Tarifazo tax protest          35
       4 Coca              Plan Dignity               Plan Dignity 1998             23
       5 Mining            Huanuni Mine               Huanuni clashes               17
       6 Rural Land        Laymi-Qaqachaka 1998       Laymi-Qaqachaka 1998          14
       7 Coca              Law 1008                   Villa Tunari Massacre         12
       8 Coca              Demilitarize Chapare       Cochabamba dept coca clas~    11
       9 Labor             Miners Pension Strike 2003 Bus crash during miners p~    11
      10 Mining            Bustillos mine occupation  Chayanta mining strike        11
      # i 204 more rows

# event_counts_by(): Results table is consistent with past runs

    Code
      event_counts_by(deaths_aug24, department)
    Output
      # A tibble: 9 x 3
      # Rowwise:  department
        department total events                                                       
        <chr>      <int> <chr>                                                        
      1 Beni          21 Santa Ana de Yacuma drug trafficking raid (4), <br>Santa Ros~
      2 Chuquisaca    10 Pensioner strike (3), <br>Sucre constitution protest (3), <b~
      3 Cochabamba   185 Plan Dignity 1998 (23), <br>Villa Tunari Massacre (12), <br>~
      4 La Paz       249 Gas War 2003 (70), <br>Tarifazo tax protest (33), <br>Bus cr~
      5 Oruro         69 Laymi-Qaqachaka 2000 (30), <br>Huanuni clashes (17), <br>Lay~
      6 Pando         14 Porvenir Massacre (11), <br>Militarization of Cobija (2), <b~
      7 Potosí        53 Laymi-Qaqachaka 1998 (14), <br>Chayanta mining strike (11), ~
      8 Santa Cruz    60 Shooting at San Julián blockade (4), <br>Yapacaní land clash~
      9 Tarija         9 Panantí Massacre (7), <br>Tarija provincial border clash (1)~

---

    Code
      event_counts_by(deaths_aug24, protest_domain, protest_campaign)
    Output
      # A tibble: 171 x 4
      # Rowwise:  protest_domain, protest_campaign
         protest_domain protest_campaign         total events                         
         <chr>          <chr>                    <int> <chr>                          
       1 Coca           2001 Coca protest            8 Chapare cocalero protests 2001~
       2 Coca           Adepcoca conflict            6 Asunta coca conflict (3), <br>~
       3 Coca           Apolo eradication            4 Apolo coca eradication (4)     
       4 Coca           Chapare coca 1991            2 Isiboro Sécure cocalero killed~
       5 Coca           Chapare coca 1992            3 UMOPAR 1992 (3)                
       6 Coca           Chapare coca 1994            4 Carrasco coca (1), <br>Chapare~
       7 Coca           Chapare coca 1995            8 Coca eradication 1995 (5), <br~
       8 Coca           Chapare coca 1997           10 Eterazama eradication (8), <br~
       9 Coca           Chapare eradication 2003     2 Chapare soldiers killed Jun 20~
      10 Coca           Coca conflict 2000           1 Isinuta cocalero ambush (1)    
      # i 161 more rows

---

    Code
      event_counts_by(deaths_aug24, protest_domain, protest_campaign, count_events = TRUE)
    Output
      # A tibble: 171 x 5
      # Rowwise:  protest_domain, protest_campaign
         protest_domain protest_campaign         total n_events events                
         <chr>          <chr>                    <int>    <int> <chr>                 
       1 Coca           2001 Coca protest            8        1 Chapare cocalero prot~
       2 Coca           Adepcoca conflict            6        4 Asunta coca conflict ~
       3 Coca           Apolo eradication            4        1 Apolo coca eradicatio~
       4 Coca           Chapare coca 1991            2        2 Isiboro Sécure cocale~
       5 Coca           Chapare coca 1992            3        1 UMOPAR 1992 (3)       
       6 Coca           Chapare coca 1994            4        4 Carrasco coca (1), <b~
       7 Coca           Chapare coca 1995            8        3 Coca eradication 1995~
       8 Coca           Chapare coca 1997           10        2 Eterazama eradication~
       9 Coca           Chapare eradication 2003     2        1 Chapare soldiers kill~
      10 Coca           Coca conflict 2000           1        1 Isinuta cocalero ambu~
      # i 161 more rows

# truncate_event_list(): Results table is consistent with past runs

    Code
      truncate_event_list(event_counts_by(deaths_aug24, protest_domain,
        protest_campaign))
    Output
      # A tibble: 171 x 4
      # Rowwise:  protest_domain, protest_campaign
         protest_domain protest_campaign         total events                         
         <chr>          <chr>                    <int> <chr>                          
       1 Coca           2001 Coca protest            8 Chapare cocalero protests 2001~
       2 Coca           Adepcoca conflict            6 Asunta coca conflict (3), <br>~
       3 Coca           Apolo eradication            4 Apolo coca eradication (4)     
       4 Coca           Chapare coca 1991            2 Isiboro Sécure cocalero killed~
       5 Coca           Chapare coca 1992            3 UMOPAR 1992 (3)                
       6 Coca           Chapare coca 1994            4 Carrasco coca (1), <br>Chapare~
       7 Coca           Chapare coca 1995            8 Coca eradication 1995 (5), <br~
       8 Coca           Chapare coca 1997           10 Eterazama eradication (8), <br~
       9 Coca           Chapare eradication 2003     2 Chapare soldiers killed Jun 20~
      10 Coca           Coca conflict 2000           1 Isinuta cocalero ambush (1)    
      # i 161 more rows

---

    Code
      truncate_event_list(event_counts_by(deaths_aug24, pres_admin, protest_domain))
    Output
      # A tibble: 77 x 4
      # Rowwise:  pres_admin, protest_domain
         pres_admin                protest_domain       total events                  
         <chr>                     <chr>                <int> <chr>                   
       1 Carlos Diego Mesa Gisbert Coca                     7 Chapare soldiers killed~
       2 Carlos Diego Mesa Gisbert Gas Wars                 1 Gas War 2005 (1)        
       3 Carlos Diego Mesa Gisbert Mining                   3 Miner suicide bombing o~
       4 Carlos Diego Mesa Gisbert Municipal governance     1 Ayo Ayo mayor killed ov~
       5 Carlos Diego Mesa Gisbert Paramilitary             1 Car bomb explosion (1)  
       6 Carlos Diego Mesa Gisbert Partisan Politics        1 San Ignacio de Moxos Ma~
       7 Carlos Diego Mesa Gisbert Peasant                  3 Beni highway blockade (~
       8 Carlos Diego Mesa Gisbert Urban land               2 Pampa San Miguel urban ~
       9 Evo Morales               Coca                    12 Apolo coca eradication ~
      10 Evo Morales               Contraband               1 Chipaya customs raid (1)
      # i 67 more rows

# top_values_string works correctly

    Code
      top_values_string(event_counts_by(deaths_aug24, department), 8)
    Output
      [1] "La Paz, Cochabamba, Oruro, Santa Cruz, Potosí, Beni, Pando, Chuquisaca"

---

    Code
      top_values_string(evco, 5, incl_counts = TRUE, by = "n_events")
    Output
      [1] "Coca (46), Mining (29), Partisan Politics (27), Rural Land (24), Labor (14)"

