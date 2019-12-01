Pap Smear
================
Laura Cosgrove
11/23/2019

``` r
library(tidyverse)
library(survey)
```

# Read data

``` r
#getting pap data from cancer df
pap_dat <- read_csv("./data/cancerxx.csv") %>%
  janitor::clean_names() %>% 
  select(hhx, fmx, fpx, #identifiers
         wtfa_sa, #weights
         strat_p, psu_p, #for design
         region, 
         paphad1, # Ever had Pap smear/Pap test
         papfrst1, #Age when had first Pap test
         pap6yr1, # Number of Pap tests, last 6 years 
         rpap1_m1, #  Month of most recent Pap test 
         rpap1y1, # Year of most recent Pap test 
         rpap1n1, #  Time ago date of most recent Pap test: # of units 
         rpap1t1, # Time ago date of most recent Pap test: Time unit 
         rpap21, #  Most recent Pap test, time categories 
         rpap3a1, # Most recent Pap test, time categories (using 2005 method) 
         rpap3b1, #  Most recent Pap test, time categories (using 2000 method) 
         hpvhrd, #  Ever heard of HPV 
         hpvpap, #  Had HPV test with Pap test 
         paprea2, #  Main reason had Pap/Pap or HPV 
         papabn3, # Pap test results in last 3 years 
         papnot2, #  Most important reason never had Pap or HPV test 
         mdrecp1, #  Doctor recommended Pap test 
         paphpvpy) #Paid for Pap or HPV test out of pocket 
```

    ## Warning: 227 parsing failures.
    ##  row      col           expected actual                  file
    ## 5956 FN_AGE10 1/0/T/F/TRUE/FALSE      9 './data/cancerxx.csv'
    ## 5956 FN_AGE12 1/0/T/F/TRUE/FALSE      9 './data/cancerxx.csv'
    ## 5956 FN_AGE14 1/0/T/F/TRUE/FALSE      9 './data/cancerxx.csv'
    ## 5956 FN_AGE15 1/0/T/F/TRUE/FALSE      9 './data/cancerxx.csv'
    ## 5956 FN_AGE25 1/0/T/F/TRUE/FALSE      9 './data/cancerxx.csv'
    ## .... ........ .................. ...... .....................
    ## See problems(...) for more details.

``` r
## getting covariates
fam_dat <- read_csv("./data/familyxx/familyxx.csv") %>%
    janitor::clean_names() %>% 
  select(hhx, fmx,  #identifiers
         rat_cat4, rat_cat5) # Ratio of family income to the poverty threshold (not sure the difference)

pers_dat <- read_csv("./data/personsx/personsx.csv") %>%
   janitor::clean_names() %>% 
  select(hhx, fmx, fpx, #identifiers
         age_p, #age
         educ1, #education
         sex, #gender
         notcov, cover, cover65, cover65o,  #coverage > 65, 65+, alternate 65+
         la1ar, #limitation
         lcondrt, #limitation is chronic
         lachronr, #chronic limitation
         hiscodi3, #ethnicity recode,
         racreci3)#race recode
```

    ## Warning: 2523 parsing failures.
    ##  row      col           expected actual                           file
    ## 1265 LCTIME5  1/0/T/F/TRUE/FALSE     96 './data/personsx/personsx.csv'
    ## 1265 LCUNIT5  1/0/T/F/TRUE/FALSE     6  './data/personsx/personsx.csv'
    ## 1265 LCDURA5  1/0/T/F/TRUE/FALSE     10 './data/personsx/personsx.csv'
    ## 1265 LCDURB5  1/0/T/F/TRUE/FALSE     4  './data/personsx/personsx.csv'
    ## 1422 LAUNIT31 1/0/T/F/TRUE/FALSE     4  './data/personsx/personsx.csv'
    ## .... ........ .................. ...... ..............................
    ## See problems(...) for more details.

``` r
adult_dat <- read_csv("./data/samadult/samadult.csv") %>%
   janitor::clean_names() %>% 
  select(hhx, fmx, fpx, #identifiers
    ausualpl, ahcplrou, ahcplknd, #Usual source of care - different options
    fla1ar) #functional limitation
```

    ## Warning: 650 parsing failures.
    ##  row      col           expected actual                           file
    ## 1056 ALTIME34 1/0/T/F/TRUE/FALSE     6  './data/samadult/samadult.csv'
    ## 1056 ALUNIT34 1/0/T/F/TRUE/FALSE     3  './data/samadult/samadult.csv'
    ## 1056 ALDURB34 1/0/T/F/TRUE/FALSE     3  './data/samadult/samadult.csv'
    ## 1056 ALCHRC34 1/0/T/F/TRUE/FALSE     2  './data/samadult/samadult.csv'
    ## 1101 CANAGE24 1/0/T/F/TRUE/FALSE     35 './data/samadult/samadult.csv'
    ## .... ........ .................. ...... ..............................
    ## See problems(...) for more details.

# Data Manipulation / Creating Covariates

``` r
pap_dat <- pap_dat %>% 
  left_join(adult_dat, by = c("hhx", "fmx", "fpx")) %>% 
  left_join(pers_dat, by = c("hhx", "fmx", "fpx")) %>% 
  left_join(fam_dat, by = c("hhx", "fmx"))

##outcome##
#disregarding clustered structure: looking for 3 or less as recommended
pap_dat %>% 
  count(rpap21)
```

    ## # A tibble: 9 x 2
    ##   rpap21     n
    ##    <dbl> <int>
    ## 1      1   521
    ## 2      2  1113
    ## 3      3  1233
    ## 4      4   931
    ## 5      5  2960
    ## 6      7    54
    ## 7      8    11
    ## 8      9   141
    ## 9     NA 26708

``` r
pap_dat %>% 
  count(rpap3b1) #choose 2000 method of q for best comparison (also less NA)
```

    ## # A tibble: 9 x 2
    ##   rpap3b1     n
    ##     <dbl> <int>
    ## 1       1  7169
    ## 2       2  3087
    ## 3       3  1303
    ## 4       4  1094
    ## 5       5  3203
    ## 6       7    54
    ## 7       8    18
    ## 8       9   129
    ## 9      NA 17615

``` r
pap_dat <- pap_dat %>% 
  mutate(paprec_3bcat = if_else(rpap3b1 <= 3, 1, 0))

##covariates##
pap_dat %>% 
  ggplot() +
  geom_histogram(aes(x = age_p, weight = wtfa_sa), bins = 10)
```

![](papsmear_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
pap_dat <- pap_dat %>% 
  mutate(age_cat = case_when(age_p >= 25 & age_p < 40 ~ "25–39",
                              age_p >= 40 & age_p < 50 ~ "40–49",
                              age_p >= 50 & age_p < 65 ~ "50–64",
                              age_p >= 65 ~ "65+"))

pap_dat %>% 
  count(educ1)
```

    ## # A tibble: 24 x 2
    ##    educ1     n
    ##    <dbl> <int>
    ##  1     0   134
    ##  2     1    27
    ##  3     2    76
    ##  4     3   111
    ##  5     4   110
    ##  6     5   133
    ##  7     6   493
    ##  8     7   203
    ##  9     8   527
    ## 10     9   624
    ## # … with 14 more rows

``` r
# Less than high school: < 13
# High school graduate: 13 or 14
# Some college or AA degree 15, 16, 17
# College graduate (BA/BS) 18, 19, 20, 21

pap_dat <- pap_dat %>% 
  mutate(educ_cat = case_when(educ1 < 13 ~ "Less than high school",
                              educ1 >= 13 & educ1 < 15 ~ "High school",
                              educ1 >= 15 & educ1 < 18 ~ "Some college",
                              educ1 >= 18 & educ1 <= 21 ~ "College graduate"))

# Family income/poverty ratio   
# <200% 
# 200–299%
# 300–399%      
# 400–499%  
# ≥500%

# 01 Under 0.50 1718 4.06
# 02 0.50 - 0.74 1566 3.70
# 03 0.75 - 0.99 1953 4.62
# 04 1.00 - 1.24 1852 4.38
# 05 1.25 - 1.49 1657 3.92
# 06 1.50 - 1.74 1658 3.92
# 07 1.75 - 1.99 1500 3.55
# 08 2.00 - 2.49 3145 7.44
# 09 2.50 - 2.99 2504 5.92
# 10 3.00 - 3.49 2339 5.53
# 11 3.50 - 3.99 1746 4.13
# 12 4.00 - 4.49 1834 4.34
# 13 4.50 - 4.99 1316 3.11
# 14 5.00 and over 8318 19.67
# 15 Less than 1.00 (no further detail) 845 2.00
# 16 1.00 - 1.99 (no further detail) 1381 3.27
# 17 2.00 and over (no further detail) 3615 8.55
# 96 Undefinable 548 1.30
# 99 Unknown 2793 6.60 

pap_dat %>% count(rat_cat5)
```

    ## # A tibble: 20 x 2
    ##    rat_cat5     n
    ##       <dbl> <int>
    ##  1        1  1471
    ##  2        2  1302
    ##  3        3  1690
    ##  4        4  1585
    ##  5        5  1402
    ##  6        6  1388
    ##  7        7  1267
    ##  8        8  2637
    ##  9        9  2072
    ## 10       10  1995
    ## 11       11  1445
    ## 12       12  1517
    ## 13       13  1094
    ## 14       14  6988
    ## 15       15   606
    ## 16       16   973
    ## 17       17   911
    ## 18       18  1097
    ## 19       96   892
    ## 20       99  1340

``` r
pap_dat <- pap_dat %>% 
  mutate(finc_cat = case_when(rat_cat5 <= 7 |  rat_cat5 %in% c(15, 16) ~ "<200%",
                              rat_cat5 %in% c(8, 9) ~ "200–299%", 
                              rat_cat5 %in% c(10, 11) ~ "300–399%",
                              rat_cat5 >= 18 & educ1 <= 21 ~ "400–499%",
                              rat_cat5 == 14  ~">=500%",
                              rat_cat5 == 17  ~">=200%, no further detail",
                              rat_cat5 %in% c(96, 99) ~ "Unknown")) %>% 
  mutate(finc_cat = factor(finc_cat, levels = c("<200%", "200–299%", "300–399%", "400–499%", ">=500%", 
                                                ">=200%, no further detail", "Unknown")))

#usually go when sick
# 1 Yes 28445 84.48
# 2 There is NO place 4506 13.38
# 3 There is MORE THAN ONE place 454 1.35
# 7 Refused 7 0.02
# 8 Not ascertained 255 0.76
# 9 Don't know 5 0.01
pap_dat <- pap_dat %>% 
  mutate(ausualpl_cat  = case_when(ausualpl == 2 ~ "No",
                                 ausualpl %in% c(1, 3) ~ "Yes",
                                 ausualpl %in% c(7, 8, 9) ~ "Other"))

# Health insurance
# None
# Public
# Private/military
# NOTCOV Frequency Percent
# ------------------------------------------
# 1 Not covered 10506 10.12
# 2 Covered 92181 88.82
# 7 Refused 0 0.00
# 8 Not ascertained 0 0.00
# 9 Don't know 1102 1.06 
# cover
# 1 Private 53793 60.81
# 2 Medicaid and other public 19851 22.44
# 3 Other coverage 3413 3.86
# 4 Uninsured 10381 11.73
# 5 Don't know 1030 1.16 
# COVER65 Frequency Percent
# ---------------------------------------------------------------------
# 1 Private 6430 41.97
# 2 Dual eligible 1197 7.81
# 3 Medicare Advantage 3295 21.51
# 4 Medicare only excluding Medicare Advantage 2893 18.88
# 5 Other coverage 1290 8.42
# 6 Uninsured 125 0.82
# 7 Don't know 91 0.59 

pap_dat %>% count(cover)
```

    ## # A tibble: 6 x 2
    ##   cover     n
    ##   <dbl> <int>
    ## 1     1 16544
    ## 2     2  3763
    ## 3     3  1337
    ## 4     4  3514
    ## 5     5   136
    ## 6    NA  8378

``` r
pap_dat %>% count(!is.na(cover65))
```

    ## # A tibble: 2 x 2
    ##   `!is.na(cover65)`     n
    ##   <lgl>             <int>
    ## 1 FALSE             25294
    ## 2 TRUE               8378

``` r
pap_dat %>% count(cover65o)
```

    ## # A tibble: 7 x 2
    ##   cover65o     n
    ##      <dbl> <int>
    ## 1        1  4025
    ## 2        2   730
    ## 3        3  2819
    ## 4        4   732
    ## 5        5    53
    ## 6        6    19
    ## 7       NA 25294

``` r
pap_dat <- pap_dat %>% 
  mutate(cover_cat  = case_when(notcov == 1 | cover == 4 | cover65 == 6 ~ "None",
                                cover == 2 | cover65 %in% 2:4 ~ "Public",
                                cover %in% c(1, 3) | cover65 %in% c(1, 5) ~ "Private/Military"))

# Chronic disability
# Yes
# No
# LCONDRT Frequency
# 1 At least one condition causing limitation of activity is chronic 13835
# 2 No condition causing limitation of activity is chronic 199
# 9 Unknown if any condition causing limitation of activity is chronic 269 

pap_dat %>% count(lcondrt)
```

    ## # A tibble: 4 x 2
    ##   lcondrt     n
    ##     <dbl> <int>
    ## 1       1  6331
    ## 2       2    84
    ## 3       9    82
    ## 4      NA 27175

``` r
pap_dat <- pap_dat %>% 
  mutate(lcond_chronic_cat = if_else(lcondrt == 1, "Yes", "No"))

# Race/ethnicity
# hispanic, nonhispanic white, nonhispanic black, nonhispanic asian, nonhispanic alaska native/american indian
pap_dat %>% count(racreci3)
```

    ## # A tibble: 4 x 2
    ##   racreci3     n
    ##      <dbl> <int>
    ## 1        1 26150
    ## 2        2  4842
    ## 3        3  2065
    ## 4        4   615

``` r
pap_dat %>% count(hiscodi3)
```

    ## # A tibble: 5 x 2
    ##   hiscodi3     n
    ##      <dbl> <int>
    ## 1        1  5591
    ## 2        2 21080
    ## 3        3  4612
    ## 4        4  1960
    ## 5        5   429

``` r
# race (white, black, asian, alaska native/american indian)
# RACRECI3 Frequency Percent
# ------------------------------------------------------------------
# 1 White 79077 76.19
# 2 Black 14997 14.45
# 3 Asian 7414 7.14
# 4 All other race groups (See file layout) 2301 2.22
# HHC.200_01.000: Race/ethnicity recode
# HISCODI3 Frequency Percent
# -------------------------------------------------------------
# 1 Hispanic 23318 22.47
# 2 Non-Hispanic White 57953 55.84
# 3 Non-Hispanic Black 14039 13.53
# 4 Non-Hispanic Asian 6965 6.71
# 5 Non-Hispanic All other race groups 1514 1.46 

pap_dat <- pap_dat %>% 
  mutate(race_cat = case_when(racreci3 == 1 ~ "White",
                              racreci3 == 2 ~ "Black",
                              racreci3 == 3 ~ "Asian",
                              racreci3 == 4 ~ "AN/AI"),
         eth_cat = case_when(hiscodi3 == 1 ~ "Hispanic",
                             hiscodi3 == 2 ~ "Non-Hispanic White",
                             hiscodi3 == 3 ~ "Non-Hispanic Black",
                             hiscodi3 == 4 ~ "Non-Hispanic Asian",
                             hiscodi3 == 5 ~ "Non-Hispanic AN/AI"))

#create indicator inclusion criteria variable
##filter age less than 25 per cancer paper
##filter to only women
pap_dat %>% count(sex)
```

    ## # A tibble: 2 x 2
    ##     sex     n
    ##   <dbl> <int>
    ## 1     1 15071
    ## 2     2 18601

``` r
pap_dat <- pap_dat %>% 
  mutate(inc = if_else(age_p >= 25 & sex == 2, 1, 0))
#create indicator variable and include it in domain analysis 
```

# survey design

``` r
des <- svydesign(ids = ~psu_p, strata = ~strat_p, 
                 weights = ~wtfa_sa, nest = TRUE, data = pap_dat)
pap_dat %>% select(ends_with("cat"))  %>% names()
```

    ## [1] "paprec_3bcat"      "age_cat"           "educ_cat"         
    ## [4] "finc_cat"          "ausualpl_cat"      "cover_cat"        
    ## [7] "lcond_chronic_cat" "race_cat"          "eth_cat"

``` r
pap_dat %>% count(paprec_3bcat) #unwt
```

    ## # A tibble: 3 x 2
    ##   paprec_3bcat     n
    ##          <dbl> <int>
    ## 1            0  4498
    ## 2            1 11559
    ## 3           NA 17615

# descriptive stats

``` r
pct_func <- function(outcome = "paprec_3bcat", inclusion = "inc", var1 = "age_cat", var2 = NULL) {
  .outcome = reformulate(outcome)
  .by = reformulate(c(inclusion, var1, var2))
   svyby(.outcome , by = .by, svymean, na.rm = TRUE, design = des, vartype = "ci") 
  
}

tot_func <- function(outcome = "paprec_3bcat", inclusion = "inc", var1 = "age_cat", var2 = NULL) {
  .outcome = reformulate(outcome)
  .by = reformulate(c(inclusion, var1, var2))
   svyby(.outcome , by = .by, svytotal, na.rm = TRUE, design = des, vartype = "ci") 
  
}

pap_by <- pap_dat %>% 
  select(ends_with("cat")) %>% 
  names() %>% 
  tibble(var = .) %>% 
  mutate(pct = map(var, ~pct_func(var1 = .x))) %>% 
  mutate(tot = map(var, ~tot_func(var1 = .x))) %>% 
  mutate(pct_byage = map(var, ~pct_func(var1 = "age_cat", var2 = .x)))
```

``` r
get_comp_tables <- function(tablepct, tabletot, var) {
  tabletot <- tabletot %>% filter(inc == 1) %>% rename_all(~paste0("t_", .x)) 
  tablepct %>% 
  filter(inc == 1) %>% 
  bind_cols(tabletot) %>% 
  mutate_at(vars(paprec_3bcat, ci_l, ci_u), ~round(.x*100, 1)) %>% 
  mutate(pct = paste0(paprec_3bcat, " (", ci_l, ", ", ci_u, ")")) %>% 
  mutate_at(vars(t_paprec_3bcat, t_ci_l, t_ci_u), ~round(.x/1e6, 1)) %>% 
  mutate(tot = paste0(t_paprec_3bcat, "M (", t_ci_l, "M, ", t_ci_u, "M)")) %>% 
  select(var, pct, tot) %>% 
  rename(levels = var) %>% 
  as_tibble()
}

pap_by <- pap_by %>% 
  filter(var != "paprec_3bcat") %>% 
  rename(variable = var) %>% 
  mutate(comp_tbl = pmap(list(x = pct, y = tot, z = variable), function(x, y, z) 
    {get_comp_tables(tablepct = x, tabletot = y, var =  z)} ))

pap_sel <- pap_by %>% 
  select(variable, comp_tbl) %>% 
  unnest_wider(comp_tbl) %>% 
  unnest(-variable)
  
pap_sel %>% 
  knitr::kable(names = c("Variable", "Levels", "Percent", "Total"))
```

| variable            | levels                     | pct                | tot                  |
| :------------------ | :------------------------- | :----------------- | :------------------- |
| age\_cat            | 25–39                      | 91.2 (90.1, 92.3)  | 24.8M (23.7M, 26M)   |
| age\_cat            | 40–49                      | 83.6 (81.7, 85.5)  | 15.4M (14.4M, 16.4M) |
| age\_cat            | 50–64                      | 75.7 (74.1, 77.4)  | 22.1M (21.1M, 23.2M) |
| age\_cat            | 65+                        | 45.1 (43, 47.3)    | 10.4M (9.7M, 11.1M)  |
| educ\_cat           | College graduate           | 83.9 (82.7, 85.1)  | 28.1M (26.8M, 29.4M) |
| educ\_cat           | High school                | 65.4 (63.3, 67.6)  | 14.7M (13.8M, 15.5M) |
| educ\_cat           | Less than high school      | 62.4 (59.5, 65.4)  | 7.1M (6.5M, 7.6M)    |
| educ\_cat           | Some college               | 74.8 (73.2, 76.3)  | 22.6M (21.5M, 23.6M) |
| finc\_cat           | \<200%                     | 68.1 (66.4, 69.9)  | 19.4M (18.5M, 20.4M) |
| finc\_cat           | 200–299%                   | 72.1 (69.5, 74.6)  | 9.4M (8.8M, 10.1M)   |
| finc\_cat           | 300–399%                   | 76.4 (73.6, 79.3)  | 7.8M (7.1M, 8.4M)    |
| finc\_cat           | 400–499%                   | 70 (66.9, 73)      | 7.5M (6.8M, 8.1M)    |
| finc\_cat           | \>=500%                    | 83 (81.2, 84.7)    | 20.5M (19.3M, 21.7M) |
| finc\_cat           | \>=200%, no further detail | 66.1 (59.5, 72.6)  | 1.8M (1.5M, 2.1M)    |
| finc\_cat           | Unknown                    | 58.1 (35.8, 80.5)  | 0.1M (0M, 0.1M)      |
| ausualpl\_cat       | No                         | 68.6 (65.3, 71.8)  | 5.5M (5M, 6M)        |
| ausualpl\_cat       | Other                      | 16.8 (-17.2, 50.9) | 0M (0M, 0M)          |
| ausualpl\_cat       | Yes                        | 74.8 (73.9, 75.8)  | 67.3M (65.3M, 69.2M) |
| cover\_cat          | None                       | 69.6 (66.4, 72.8)  | 5.3M (4.9M, 5.8M)    |
| cover\_cat          | Private/Military           | 79.3 (78.3, 80.3)  | 54M (52.2M, 55.9M)   |
| cover\_cat          | Public                     | 60.2 (57.9, 62.5)  | 13.1M (12.3M, 13.9M) |
| lcond\_chronic\_cat | No                         | 58.5 (44.1, 72.9)  | 0.2M (0.1M, 0.3M)    |
| lcond\_chronic\_cat | Yes                        | 56.6 (54.1, 59.2)  | 9.7M (9.1M, 10.4M)   |
| race\_cat           | AN/AI                      | 70.5 (63.2, 77.8)  | 0.9M (0.7M, 1.1M)    |
| race\_cat           | Asian                      | 80.3 (76.7, 83.9)  | 4.6M (4.1M, 5M)      |
| race\_cat           | Black                      | 79.2 (77.1, 81.2)  | 9.9M (9.3M, 10.6M)   |
| race\_cat           | White                      | 73.2 (72.2, 74.1)  | 57.4M (55.5M, 59.3M) |
| eth\_cat            | Hispanic                   | 80.1 (78, 82.3)    | 10.7M (10.1M, 11.4M) |
| eth\_cat            | Non-Hispanic AN/AI         | 71.6 (62.3, 81)    | 0.6M (0.4M, 0.8M)    |
| eth\_cat            | Non-Hispanic Asian         | 80.1 (76.4, 83.8)  | 4.4M (3.9M, 4.8M)    |
| eth\_cat            | Non-Hispanic Black         | 78.9 (76.8, 81)    | 9.3M (8.7M, 9.9M)    |
| eth\_cat            | Non-Hispanic White         | 71.9 (70.8, 73)    | 47.8M (46M, 49.5M)   |

``` r
pap_strat <- pap_by %>% 
  select(variable, pct_byage) %>% 
  filter(variable != "age_cat") %>% 
  mutate(pct_byage = map2(.x = pct_byage, .y = variable, ~.x %>% rename(levels = .y))) %>% 
  unnest(pct_byage) %>% 
  filter(inc == 1) %>%
  mutate_at(vars(paprec_3bcat, ci_l, ci_u), ~round(.x*100, 1)) %>% 
  mutate(pct = paste0(paprec_3bcat, " (", ci_l, ", ", ci_u, ")")) %>% 
  select(-paprec_3bcat, -ci_l, -ci_u, -inc) %>% 
  pivot_wider(names_from = "age_cat", values_from = pct) 

pap_strat %>% 
  filter(!levels %in% c("Unknown", "Other")) %>% 
  knitr::kable()
```

| variable            | levels                     | 25–39              | 40–49              | 50–64             | 65+               |
| :------------------ | :------------------------- | :----------------- | :----------------- | :---------------- | :---------------- |
| educ\_cat           | College graduate           | 94.7 (93.2, 96.3)  | 91.4 (89.1, 93.7)  | 82.6 (80.1, 85.1) | 54.3 (50.3, 58.3) |
| educ\_cat           | High school                | 85.4 (82.1, 88.7)  | 75.3 (70.5, 80.1)  | 71.5 (67.7, 75.3) | 41.5 (37.6, 45.4) |
| educ\_cat           | Less than high school      | 86.5 (82.7, 90.4)  | 71.2 (63.6, 78.7)  | 66.1 (60, 72.1)   | 37.3 (32, 42.6)   |
| educ\_cat           | Some college               | 91 (88.9, 93.2)    | 83.1 (79.3, 86.9)  | 74.8 (71.7, 77.8) | 46.2 (42, 50.4)   |
| finc\_cat           | \<200%                     | 87.7 (85.8, 89.6)  | 77.5 (73.3, 81.8)  | 65.4 (61.6, 69.1) | 37.6 (34.1, 41.1) |
| finc\_cat           | 200–299%                   | 88.1 (84.9, 91.3)  | 81.1 (75.1, 87.2)  | 75 (69.8, 80.2)   | 42.9 (37.7, 48.1) |
| finc\_cat           | 300–399%                   | 92.4 (89.5, 95.3)  | 81.9 (75.8, 88)    | 74.8 (68.7, 80.9) | 51.3 (44.3, 58.3) |
| finc\_cat           | 400–499%                   | 93.4 (89.7, 97.2)  | 84.7 (78.2, 91.3)  | 77.2 (72.2, 82.3) | 44.6 (39.7, 49.4) |
| finc\_cat           | \>=500%                    | 95.5 (93.6, 97.4)  | 91.7 (88.9, 94.5)  | 82.1 (79.3, 84.9) | 56 (51.2, 60.9)   |
| finc\_cat           | \>=200%, no further detail | 90.9 (80.6, 101.2) | 75.6 (59.8, 91.4)  | 73.7 (60.9, 86.6) | 45.6 (35.7, 55.4) |
| ausualpl\_cat       | No                         | 85.2 (81.7, 88.6)  | 64.4 (55.9, 72.9)  | 47.1 (39, 55.3)   | 25.9 (15.5, 36.3) |
| ausualpl\_cat       | Yes                        | 92.3 (91, 93.5)    | 85.5 (83.6, 87.4)  | 77.5 (75.8, 79.1) | 45.6 (43.5, 47.8) |
| cover\_cat          | None                       | 82.8 (79.1, 86.6)  | 59.2 (51.8, 66.6)  | 57.6 (50.4, 64.8) | 38.6 (10.1, 67)   |
| cover\_cat          | Private/Military           | 93.2 (92, 94.4)    | 87.9 (86, 89.9)    | 78.7 (77, 80.3)   | 47.3 (44.4, 50.1) |
| cover\_cat          | Public                     | 89.7 (87.2, 92.2)  | 78.4 (72.7, 84.1)  | 65.1 (59.3, 70.8) | 43.1 (39.9, 46.2) |
| lcond\_chronic\_cat | No                         | 90.6 (76.2, 105)   | 87.3 (67.2, 107.4) | 84.9 (64.8, 105)  | 18.4 (3.6, 33.1)  |
| lcond\_chronic\_cat | Yes                        | 88.2 (83, 93.4)    | 73.7 (66.6, 80.7)  | 68.7 (64.7, 72.8) | 36.4 (33.2, 39.6) |
| race\_cat           | AN/AI                      | 84.1 (74.3, 94)    | 82.9 (70.3, 95.4)  | 63.4 (47.5, 79.4) | 35.1 (12.5, 57.6) |
| race\_cat           | Asian                      | 89.2 (84.9, 93.6)  | 87.9 (81.7, 94.1)  | 79.5 (72.4, 86.6) | 47.5 (38.6, 56.4) |
| race\_cat           | Black                      | 92.8 (90.4, 95.1)  | 87.6 (83.6, 91.5)  | 78.4 (74, 82.7)   | 45.9 (40.5, 51.4) |
| race\_cat           | White                      | 91.2 (89.9, 92.5)  | 82.5 (80.4, 84.7)  | 75.3 (73.4, 77.1) | 45 (42.7, 47.4)   |
| eth\_cat            | Hispanic                   | 89.2 (87, 91.4)    | 84.7 (80.6, 88.8)  | 78.3 (73.2, 83.5) | 49.7 (42.5, 56.9) |
| eth\_cat            | Non-Hispanic AN/AI         | 85.5 (74, 97)      | 83 (66.9, 99)      | 68.9 (49.3, 88.5) | 35.6 (8.8, 62.5)  |
| eth\_cat            | Non-Hispanic Asian         | 89 (84.4, 93.5)    | 88.7 (82.5, 94.9)  | 79 (71.8, 86.2)   | 47.8 (38.9, 56.8) |
| eth\_cat            | Non-Hispanic Black         | 92.8 (90.3, 95.2)  | 87.9 (84.1, 91.8)  | 78.7 (74.5, 82.9) | 45.5 (40.1, 51)   |
| eth\_cat            | Non-Hispanic White         | 91.8 (90.3, 93.3)  | 81.9 (79.3, 84.5)  | 74.7 (72.7, 76.6) | 44.6 (42.1, 47)   |
