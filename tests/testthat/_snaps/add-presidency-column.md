# Testing add_presidency_column

    Code
      add_presidency_column(pres_freq_table, "pres_admin", dest_var = "presidency_surnames")
    Output
      # A tibble: 12 x 3
         pres_admin                          n presidency_surnames        
         <chr>                           <int> <fct>                      
       1 Hernán Siles Zuazo                  8 Siles Zuazo                
       2 Víctor Paz Estenssoro              43 Paz Estenssoro             
       3 Jaime Paz Zamora                   22 Paz Zamora                 
       4 Gonzalo Sanchez de Lozada (1st)    52 Sánchez de Lozada          
       5 Hugo Banzer (2nd)                 122 Banzer Suárez              
       6 Jorge Quiroga                      32 Quiroga Ramírez            
       7 Gonzalo Sanchez de Lozada (2nd)   146 Sánchez de Lozada          
       8 Carlos Diego Mesa Gisbert          19 Mesa Gisbert               
       9 Evo Morales                       149 Morales Ayma               
      10 Interim military government         9 Interim military government
      11 Jeanine Áñez                       26 Áñez Chávez                
      12 Luis Arce                          42 Arce Catacora              

---

    Code
      add_presidency_column(pres_freq_table, "pres_admin", dest_var = "presidency_surnames",
        .location = "replace")
    Output
      # A tibble: 12 x 2
         presidency_surnames             n
         <fct>                       <int>
       1 Siles Zuazo                     8
       2 Paz Estenssoro                 43
       3 Paz Zamora                     22
       4 Sánchez de Lozada              52
       5 Banzer Suárez                 122
       6 Quiroga Ramírez                32
       7 Sánchez de Lozada             146
       8 Mesa Gisbert                   19
       9 Morales Ayma                  149
      10 Interim military government     9
      11 Áñez Chávez                    26
      12 Arce Catacora                  42

---

    Code
      add_presidency_column(pres_freq_table, "pres_admin", dest_var = "id_presidency",
        .location = "beside")
    Output
      # A tibble: 12 x 3
         pres_admin                      id_presidency     n
         <chr>                           <fct>         <int>
       1 Hernán Siles Zuazo              p101              8
       2 Víctor Paz Estenssoro           p102             43
       3 Jaime Paz Zamora                p103             22
       4 Gonzalo Sanchez de Lozada (1st) p104             52
       5 Hugo Banzer (2nd)               p105            122
       6 Jorge Quiroga                   p106             32
       7 Gonzalo Sanchez de Lozada (2nd) p107            146
       8 Carlos Diego Mesa Gisbert       p108             19
       9 Evo Morales                     p110            149
      10 Interim military government     p111              9
      11 Jeanine Áñez                    p112             26
      12 Luis Arce                       p113             42

---

    Code
      add_presidency_column(pres_freq_table, "pres_admin", dest_var = "presidency_year_es",
        .location = "left")
    Output
      # A tibble: 12 x 3
         presidency_year_es                    pres_admin                          n
         <fct>                                 <chr>                           <int>
       1 Hernán Siles Zuazo (1982-1985)        Hernán Siles Zuazo                  8
       2 Víctor Paz Estenssoro (1985-1989)     Víctor Paz Estenssoro              43
       3 Jaime Paz Zamora (1989-1993)          Jaime Paz Zamora                   22
       4 Gonzalo Sánchez de Lozada (1993-1997) Gonzalo Sanchez de Lozada (1st)    52
       5 Hugo Banzer (1997-2001)               Hugo Banzer (2nd)                 122
       6 Jorge Quiroga (2001-2002)             Jorge Quiroga                      32
       7 Gonzalo Sánchez de Lozada (2002-2003) Gonzalo Sanchez de Lozada (2nd)   146
       8 Carlos Mesa (2003-2005)               Carlos Diego Mesa Gisbert          19
       9 Evo Morales (2006-2019)               Evo Morales                       149
      10 Gobierno interino militar (2019)      Interim military government         9
      11 Jeanine Áñez (2019-2020)              Jeanine Áñez                       26
      12 Luis Arce (2020- )                    Luis Arce                          42

---

    Code
      add_presidency_column(pres_freq_table, "pres_admin", dest_var = "presidency_initials_num",
        .location = "right")
    Output
      # A tibble: 12 x 3
         pres_admin                          n presidency_initials_num
         <chr>                           <int> <fct>                  
       1 Hernán Siles Zuazo                  8 HSZ                    
       2 Víctor Paz Estenssoro              43 VPE 2                  
       3 Jaime Paz Zamora                   22 JPZ                    
       4 Gonzalo Sanchez de Lozada (1st)    52 GSL 1                  
       5 Hugo Banzer (2nd)                 122 HB 2                   
       6 Jorge Quiroga                      32 JQ                     
       7 Gonzalo Sanchez de Lozada (2nd)   146 GSL 2                  
       8 Carlos Diego Mesa Gisbert          19 CM                     
       9 Evo Morales                       149 EM                     
      10 Interim military government         9 Mil                    
      11 Jeanine Áñez                       26 JÁ                     
      12 Luis Arce                          42 LA                     

# Testing render_presidency

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Gonzalo Sanchez de Lozada (1st)"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "p104"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Gonzalo Sánchez de Lozada (1993-1997)"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Gonzalo Sánchez de Lozada"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Gonzalo Sánchez de Lozada"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Sánchez de Lozada"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Gonzalo Sánchez de Lozada (1993-1997)"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Gonzalo Sánchez de Lozada"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "Gonzalo Sánchez de Lozada"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "GSL"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "GSL 1"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "1993-08-06"

---

    Code
      render_presidency("p104", i, source_var = "id_presidency")
    Output
      [1] "1997-08-06"

