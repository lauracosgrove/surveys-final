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
         racreci3, yrsinus, plborn)#race recode
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


pap_dat <- pap_dat %>%
  mutate(imm_stat = case_when(yrsinus < 4 ~ "In U.S. < 10 yrs",
                              yrsinus == 4 | yrsinus == 5 ~ "In U.S. >= 10 yrs",
                              plborn == 1 ~ "Born in U.S."))


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

##replace this function with an unweighted total 
tot_func <- function(outcome = "paprec_3bcat", inclusion = "inc", var1 = "age_cat", var2 = NULL) {
outcome_sym = rlang::sym(outcome)
inc_sym = rlang::sym(inclusion)
var1_sym = rlang::sym(var1)
var2_sym = ifelse(is.null(var2), rlang::sym(" "), rlang::sym(var2))

if (is.null(var2)) {
  pap_dat %>% 
    count(!!outcome_sym, !!inc_sym, !!var1_sym) %>% drop_na() %>% 
    group_by(!!var1_sym, !!inc_sym) %>% 
    summarize(n = sum(n))
} else  {
  pap_dat %>% 
    count(!!outcome_sym, !!inc_sym, !!var1_sym, !!var2_sym) %>% drop_na() %>% 
    group_by(!!var1_sym, !!var2_sym, !!inc_sym) %>% 
    summarize(n = sum(n))

}
}

tot_func()
```

    ## # A tibble: 4 x 3
    ## # Groups:   age_cat [4]
    ##   age_cat   inc     n
    ##   <chr>   <dbl> <int>
    ## 1 25–39       1  3980
    ## 2 40–49       1  2551
    ## 3 50–64       1  4256
    ## 4 65+         1  4384

``` r
pap_by <- pap_dat %>% 
  select(ends_with("cat"), imm_stat, -paprec_3bcat) %>% 
  mutate(ausualpl_cat = fct_explicit_na(ausualpl_cat)) %>% 
  names() %>% 
  tibble(var = .) %>% 
  mutate(pct = map(var, ~pct_func(var1 = .x))) %>% 
  mutate(tot = map(var, ~tot_func(var1 = .x))) %>% 
  mutate(pct_byage = map(var, ~pct_func(var1 = "age_cat", var2 = .x))) %>% 
  mutate(tot_byage = map(var, ~tot_func(var1 = "age_cat", var2 = .x)))
```

    ## Warning: Factor `finc_cat` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`
    
    ## Warning: Factor `finc_cat` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

``` r
get_comp_tables <- function(tablepct, tabletot, var) {
  tabletot <- tabletot %>% filter(inc == 1) %>% rename_all(~paste0("t_", .x)) 
  tablepct %>% 
  filter(inc == 1) %>% 
  bind_cols(tabletot) %>% 
  mutate_at(vars(paprec_3bcat, ci_l, ci_u), ~round(.x*100, 1)) %>% 
  mutate(pct = paste0(paprec_3bcat, " (", ci_l, ", ", ci_u, ")")) %>% 
  mutate(tot = t_n) %>% 
  select(var, pct, tot) %>% 
  rename(levels = var) %>% 
  as_tibble()
}

pap_by <- pap_by %>% 
  mutate(comp_tbl = pmap(list(x = pct, y = tot, z = var), function(x, y, z) 
    {get_comp_tables(tablepct = x, tabletot = y, var =  z)} ))

pap_sel <- pap_by %>% 
  select(var, comp_tbl) %>% 
  unnest_wider(comp_tbl) %>% 
  unnest(-var)
  
pap_sel %>% 
  filter(!levels %in% c("Unknown", "Other")) %>% 
  knitr::kable(names = c("Variable", "Levels", "Percent", "Total"))
```

| var                 | levels                     | pct               |   tot |
| :------------------ | :------------------------- | :---------------- | ----: |
| age\_cat            | 25–39                      | 91.2 (90.1, 92.3) |  3980 |
| age\_cat            | 40–49                      | 83.6 (81.7, 85.5) |  2551 |
| age\_cat            | 50–64                      | 75.7 (74.1, 77.4) |  4256 |
| age\_cat            | 65+                        | 45.1 (43, 47.3)   |  4384 |
| educ\_cat           | College graduate           | 83.9 (82.7, 85.1) |  4703 |
| educ\_cat           | High school                | 65.4 (63.3, 67.6) |  3616 |
| educ\_cat           | Less than high school      | 62.4 (59.5, 65.4) |  1991 |
| educ\_cat           | Some college               | 74.8 (73.2, 76.3) |  4798 |
| finc\_cat           | \<200%                     | 68.1 (66.4, 69.9) |  5459 |
| finc\_cat           | 200–299%                   | 72.1 (69.5, 74.6) |  2059 |
| finc\_cat           | 300–399%                   | 76.4 (73.6, 79.3) |  1559 |
| finc\_cat           | 400–499%                   | 70 (66.9, 73)     |  1443 |
| finc\_cat           | \>=500%                    | 83 (81.2, 84.7)   |  3109 |
| finc\_cat           | \>=200%, no further detail | 66.1 (59.5, 72.6) |   407 |
| ausualpl\_cat       | No                         | 68.6 (65.3, 71.8) |  1240 |
| ausualpl\_cat       | Yes                        | 74.8 (73.9, 75.8) | 13928 |
| cover\_cat          | None                       | 69.6 (66.4, 72.8) |  1253 |
| cover\_cat          | Private/Military           | 79.3 (78.3, 80.3) |  9884 |
| cover\_cat          | Public                     | 60.2 (57.9, 62.5) |  3983 |
| lcond\_chronic\_cat | No                         | 58.5 (44.1, 72.9) |    74 |
| lcond\_chronic\_cat | Yes                        | 56.6 (54.1, 59.2) |  3220 |
| race\_cat           | AN/AI                      | 70.5 (63.2, 77.8) |   251 |
| race\_cat           | Asian                      | 80.3 (76.7, 83.9) |   773 |
| race\_cat           | Black                      | 79.2 (77.1, 81.2) |  2343 |
| race\_cat           | White                      | 73.2 (72.2, 74.1) | 11804 |
| eth\_cat            | Hispanic                   | 80.1 (78, 82.3)   |  2372 |
| eth\_cat            | Non-Hispanic AN/AI         | 71.6 (62.3, 81)   |   171 |
| eth\_cat            | Non-Hispanic Asian         | 80.1 (76.4, 83.8) |   734 |
| eth\_cat            | Non-Hispanic Black         | 78.9 (76.8, 81)   |  2223 |
| eth\_cat            | Non-Hispanic White         | 71.9 (70.8, 73)   |  9671 |
| imm\_stat           | Born in U.S.               | 73.4 (72.5, 74.4) | 12532 |
| imm\_stat           | In U.S. \< 10 yrs          | 82.2 (77.2, 87.2) |   366 |
| imm\_stat           | In U.S. \>= 10 yrs         | 77.8 (75.4, 80.1) |  2246 |

``` r
get_by_tables <- function(tablepct, tabletot, var) {
  tabletot <- tabletot %>% filter(inc == 1) %>% rename_all(~paste0("t_", .x)) 
  tablepct %>% 
  filter(inc == 1) %>% 
  bind_cols(tabletot) %>% 
  mutate_at(vars(paprec_3bcat, ci_l, ci_u), ~round(.x*100, 1)) %>% 
  mutate(pct = paste0(paprec_3bcat, " (", ci_l, ", ", ci_u, ")")) %>% 
  mutate(tot = t_n) %>% 
  select(age_cat, var, pct, tot) %>% 
  rename(levels = var) %>% 
  as_tibble()
}

new_ausualpl_tot <- (pap_by %>% 
    filter(var == "ausualpl_cat") %>% 
  pull(tot_byage))[[1]] %>% ungroup() %>% 
  add_case(age_cat = "65+", ausualpl_cat = "Other", inc = 1, n = 0)

pap_strat <- pap_by %>% 
  dplyr::select(var, pct_byage, tot_byage) %>% 
  mutate(tot_byage = if_else(var == "ausualpl_cat", list(new_ausualpl_tot), tot_byage)) %>% 
  mutate(comp_tbl = pmap(list(x = pct_byage, y = tot_byage, z = var), function(x, y, z) 
    {get_by_tables(tablepct = x, tabletot = y, var =  z)} )) %>% 
  dplyr::select(var, comp_tbl) %>% unnest() 
```

    ## Warning: `cols` is now required.
    ## Please use `cols = c(comp_tbl)`

``` r
pap_strat %>% 
  filter(var != "age_cat") %>% 
  pivot_wider(names_from = age_cat, values_from = c(pct, tot), names_prefix = "Age_", names_sep = "_") %>% 
  select(var, levels, ends_with("39"), ends_with("49"), ends_with("64"), ends_with("+")) %>%
  knitr::kable()
```

| var                 | levels                     | pct\_Age\_25–39    | tot\_Age\_25–39 | pct\_Age\_40–49    | tot\_Age\_40–49 | pct\_Age\_50–64   | tot\_Age\_50–64 | pct\_Age\_65+     | tot\_Age\_65+ |
| :------------------ | :------------------------- | :----------------- | --------------: | :----------------- | --------------: | :---------------- | --------------: | :---------------- | ------------: |
| educ\_cat           | College graduate           | 94.7 (93.2, 96.3)  |            1492 | 91.4 (89.1, 93.7)  |             719 | 82.6 (80.1, 85.1) |             448 | 54.3 (50.3, 58.3) |          1314 |
| educ\_cat           | High school                | 85.4 (82.1, 88.7)  |             931 | 75.3 (70.5, 80.1)  |             511 | 71.5 (67.7, 75.3) |             298 | 41.5 (37.6, 45.4) |           802 |
| educ\_cat           | Less than high school      | 86.5 (82.7, 90.4)  |            1307 | 71.2 (63.6, 78.7)  |            1034 | 66.1 (60, 72.1)   |             484 | 37.3 (32, 42.6)   |          1417 |
| educ\_cat           | Some college               | 91 (88.9, 93.2)    |             973 | 83.1 (79.3, 86.9)  |            1352 | 74.8 (71.7, 77.8) |             761 | 46.2 (42, 50.4)   |          1265 |
| finc\_cat           | \<200%                     | 87.7 (85.8, 89.6)  |            1663 | 77.5 (73.3, 81.8)  |             587 | 65.4 (61.6, 69.1) |             442 | 37.6 (34.1, 41.1) |           193 |
| finc\_cat           | 200–299%                   | 88.1 (84.9, 91.3)  |             722 | 81.1 (75.1, 87.2)  |              63 | 75 (69.8, 80.2)   |               3 | 42.9 (37.7, 48.1) |           832 |
| finc\_cat           | 300–399%                   | 92.4 (89.5, 95.3)  |             349 | 81.9 (75.8, 88)    |             289 | 74.8 (68.7, 80.9) |             186 | 51.3 (44.3, 58.3) |           623 |
| finc\_cat           | 400–499%                   | 93.4 (89.7, 97.2)  |              51 | 84.7 (78.2, 91.3)  |               1 | 77.2 (72.2, 82.3) |            1324 | 44.6 (39.7, 49.4) |           494 |
| finc\_cat           | \>=500%                    | 95.5 (93.6, 97.4)  |             406 | 91.7 (88.9, 94.5)  |             457 | 82.1 (79.3, 84.9) |            1137 | 56 (51.2, 60.9)   |           102 |
| finc\_cat           | \>=200%, no further detail | 90.9 (80.6, 101.2) |               5 | 75.6 (59.8, 91.4)  |            1640 | 73.7 (60.9, 86.6) |             629 | 45.6 (35.7, 55.4) |           422 |
| finc\_cat           | Unknown                    | 100 (100, 100)     |             607 | 100 (100, 100)     |             627 | 100 (100, 100)    |             191 | 11 (-8.3, 30.3)   |            10 |
| ausualpl\_cat       | No                         | 85.2 (81.7, 88.6)  |             593 | 64.4 (55.9, 72.9)  |               1 | 47.1 (39, 55.3)   |            3386 | 25.9 (15.5, 36.3) |           259 |
| ausualpl\_cat       | Other                      | 100 (100, 100)     |               1 | 0 (0, 0)           |            2291 | 0 (0, 0)          |             266 | 0 (0, 0)          |             1 |
| ausualpl\_cat       | Yes                        | 92.3 (91, 93.5)    |            3989 | 85.5 (83.6, 87.4)  |             122 | 77.5 (75.8, 79.1) |            4262 | 45.6 (43.5, 47.8) |             0 |
| cover\_cat          | None                       | 82.8 (79.1, 86.6)  |             584 | 59.2 (51.8, 66.6)  |            2542 | 57.6 (50.4, 64.8) |             836 | 38.6 (10.1, 67)   |           309 |
| cover\_cat          | Private/Military           | 93.2 (92, 94.4)    |            1828 | 87.9 (86, 89.9)    |             406 | 78.7 (77, 80.3)   |             341 | 47.3 (44.4, 50.1) |          3282 |
| cover\_cat          | Public                     | 89.7 (87.2, 92.2)  |             617 | 78.4 (72.7, 84.1)  |              19 | 65.1 (59.3, 70.8) |            2232 | 43.1 (39.9, 46.2) |          2124 |
| lcond\_chronic\_cat | No                         | 90.6 (76.2, 105)   |              15 | 87.3 (67.2, 107.4) |             285 | 84.9 (64.8, 105)  |              10 | 18.4 (3.6, 33.1)  |           322 |
| lcond\_chronic\_cat | Yes                        | 88.2 (83, 93.4)    |              17 | 73.7 (66.6, 80.7)  |            1025 | 68.7 (64.7, 72.8) |              32 | 36.4 (33.2, 39.6) |          1588 |
| race\_cat           | AN/AI                      | 84.1 (74.3, 94)    |              89 | 82.9 (70.3, 95.4)  |             256 | 63.4 (47.5, 79.4) |             689 | 35.1 (12.5, 57.6) |          2946 |
| race\_cat           | Asian                      | 89.2 (84.9, 93.6)  |              47 | 87.9 (81.7, 94.1)  |             161 | 79.5 (72.4, 86.6) |             406 | 47.5 (38.6, 56.4) |          1937 |
| race\_cat           | Black                      | 92.8 (90.4, 95.1)  |              74 | 87.6 (83.6, 91.5)  |             200 | 78.4 (74, 82.7)   |             680 | 45.9 (40.5, 51.4) |          3302 |
| race\_cat           | White                      | 91.2 (89.9, 92.5)  |              41 | 82.5 (80.4, 84.7)  |             156 | 75.3 (73.4, 77.1) |             568 | 45 (42.7, 47.4)   |          3619 |
| eth\_cat            | Hispanic                   | 89.2 (87, 91.4)    |             902 | 84.7 (80.6, 88.8)  |              57 | 78.3 (73.2, 83.5) |             241 | 49.7 (42.5, 56.9) |           634 |
| eth\_cat            | Non-Hispanic AN/AI         | 85.5 (74, 97)      |            2146 | 83 (66.9, 99)      |             515 | 68.9 (49.3, 88.5) |              31 | 35.6 (8.8, 62.5)  |           149 |
| eth\_cat            | Non-Hispanic Asian         | 89 (84.4, 93.5)    |             376 | 88.7 (82.5, 94.9)  |            1480 | 79 (71.8, 86.2)   |             521 | 47.8 (38.9, 56.8) |            52 |
| eth\_cat            | Non-Hispanic Black         | 92.8 (90.3, 95.2)  |             192 | 87.9 (84.1, 91.8)  |             657 | 78.7 (74.5, 82.9) |            2834 | 45.5 (40.1, 51)   |           434 |
| eth\_cat            | Non-Hispanic White         | 91.8 (90.3, 93.3)  |              31 | 81.9 (79.3, 84.5)  |             152 | 74.7 (72.7, 76.6) |             556 | 44.6 (42.1, 47)   |          3211 |
| imm\_stat           | Born in U.S.               | 92 (90.7, 93.2)    |            3153 | 83.5 (81.4, 85.6)  |             221 | 74.9 (73.1, 76.7) |             598 | 44.6 (42.3, 46.9) |          1951 |
| imm\_stat           | In U.S. \< 10 yrs          | 86.6 (80.9, 92.3)  |              79 | 77.1 (64.4, 89.7)  |             514 | 81.8 (68.7, 94.8) |            3620 | 52.6 (29.5, 75.8) |            40 |
| imm\_stat           | In U.S. \>= 10 yrs         | 88.8 (85.9, 91.8)  |             590 | 85.2 (80.9, 89.4)  |            3808 | 80.3 (76.4, 84.3) |              26 | 48.8 (42.8, 54.7) |           544 |

# plot

``` r
p1 <- pap_by %>% 
  filter(var == "ausualpl_cat") %>% 
  select(var, pct_byage) %>% 
  unnest(pct_byage) %>% 
  filter(inc == 1) %>% 
  filter(!ausualpl_cat %in% c("Unknown", "Other")) %>% 
  ggplot(aes(x = ausualpl_cat, y = 100*paprec_3bcat, fill = ausualpl_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u)) +  ylim(0, 100) + coord_flip() +
  facet_grid(~age_cat) + ggthemes::theme_few() + ggthemes::scale_fill_few() + theme(legend.position = "none") + 
  labs(y = "", x = "Usual Source of Care")
```

``` r
p2 <- pap_by %>% 
  filter(var == "cover_cat") %>% 
  select(var, pct_byage) %>% 
  unnest(pct_byage) %>% 
  filter(inc == 1) %>% 
  filter(!cover_cat %in% c("Unknown", "Other")) %>% 
  ggplot(aes(x = cover_cat, y = 100*paprec_3bcat, fill = cover_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u)) +
  facet_grid(~age_cat) + 
  ggthemes::theme_few() + ggthemes::scale_fill_few() + theme(legend.position = "none") + ylim(0, 100) + coord_flip() +
  labs(y = "", x = "Insurance Coverage")
```

``` r
p3 <- pap_by %>% 
  filter(var == "educ_cat") %>% 
  select(var, pct_byage) %>% 
  unnest(pct_byage) %>% 
  filter(inc == 1) %>% 
  ggplot(aes(x = educ_cat, y = 100*paprec_3bcat, fill = educ_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u)) +
  facet_grid(~age_cat) + 
  ggthemes::theme_few() + ggthemes::scale_fill_few() + theme(legend.position = "none") + ylim(0, 100) +
  coord_flip() + 
  labs(y = "Percent Had Pap Smear, Last 3 years", x = "Education")
```

``` r
p4 <- pap_by %>% 
  filter(var == "imm_stat") %>% 
  select(var, pct_byage) %>% 
  unnest(pct_byage) %>% 
  filter(inc == 1) %>% 
  ggplot(aes(x = imm_stat, y = 100*paprec_3bcat, fill = imm_stat)) +
  geom_col() +
  geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u)) +
  facet_grid(~age_cat) + 
  ggthemes::theme_few() + ggthemes::scale_fill_few() + theme(legend.position = "none") + ylim(0, 100) +
  coord_flip() + 
  labs(y = "Percent Had Pap Smear, Last 3 years", x = "Immigration Status")
```

``` r
library(patchwork)
p1 / p2 / p3 
```

![](papsmear_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# models

``` r
pap_formod <- pap_dat %>% 
  select(psu_p, strat_p, wtfa_sa, ends_with("cat"), imm_stat, hpvhrd, paphad1, mdrecp1, inc) %>% 
  mutate(imm_stat = if_else(imm_stat == "Born in U.S.", "Born in U.S.", "Immigrated"))

pap_formod %>% count(imm_stat)
```

    ## # A tibble: 3 x 2
    ##   imm_stat         n
    ##   <chr>        <int>
    ## 1 Born in U.S. 27430
    ## 2 Immigrated    6154
    ## 3 <NA>            88

``` r
pap_formod <- pap_formod %>% 
  mutate_at(vars(hpvhrd, paphad1, mdrecp1), ~factor(.x)) %>% 
  mutate(ausualpl_cat = factor(ausualpl_cat, levels = c("Yes", "No"))) %>% 
  mutate(eth_cat = if_else(eth_cat == "Hispanic", "Hispanic", "Not Hispanic"))

pap_formod %>% ggplot(aes(x = paprec_3bcat)) + geom_histogram() + facet_grid(~inc)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 17615 rows containing non-finite values (stat_bin).

![](papsmear_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
vars = pap_dat %>% select(ends_with("cat"), imm_stat, inc) %>% select(-paprec_3bcat) %>% names()
vars = vars[1:9]
.form = reformulate(response = "paprec_3bcat", termlabels = c(vars) )


des2 <- svydesign(ids = ~psu_p, strata = ~strat_p, 
                 weights = ~wtfa_sa, nest = TRUE, data = pap_formod)

mod <- svyglm(paprec_3bcat ~ age_cat + educ_cat + 
                     ausualpl_cat + finc_cat + cover_cat + lcond_chronic_cat + 
                     eth_cat + race_cat + imm_stat, design = des2, 
       family = binomial,
       subset = inc == 1) 
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
jtools::summ(mod)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Observations

</td>

<td style="text-align:right;">

3145

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Dependent variable

</td>

<td style="text-align:right;">

paprec\_3bcat

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Type

</td>

<td style="text-align:right;">

Survey-weighted generalized linear
model

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Family

</td>

<td style="text-align:right;">

binomial

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Link

</td>

<td style="text-align:right;">

logit

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Pseudo-R² (Cragg-Uhler)

</td>

<td style="text-align:right;">

0.09

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Pseudo-R²
(McFadden)

</td>

<td style="text-align:right;">

0.31

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

AIC

</td>

<td style="text-align:right;">

2891.23

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

<th style="text-align:right;">

t val.

</th>

<th style="text-align:right;">

p

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

(Intercept)

</td>

<td style="text-align:right;">

2.31

</td>

<td style="text-align:right;">

0.67

</td>

<td style="text-align:right;">

3.44

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

age\_cat40–49

</td>

<td style="text-align:right;">

\-1.00

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

\-3.08

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

age\_cat50–64

</td>

<td style="text-align:right;">

\-1.38

</td>

<td style="text-align:right;">

0.27

</td>

<td style="text-align:right;">

\-5.18

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

age\_cat65+

</td>

<td style="text-align:right;">

\-2.75

</td>

<td style="text-align:right;">

0.28

</td>

<td style="text-align:right;">

\-9.87

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

educ\_catHigh school

</td>

<td style="text-align:right;">

\-0.44

</td>

<td style="text-align:right;">

0.18

</td>

<td style="text-align:right;">

\-2.48

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

educ\_catLess than high school

</td>

<td style="text-align:right;">

\-0.86

</td>

<td style="text-align:right;">

0.21

</td>

<td style="text-align:right;">

\-4.12

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

educ\_catSome college

</td>

<td style="text-align:right;">

\-0.27

</td>

<td style="text-align:right;">

0.18

</td>

<td style="text-align:right;">

\-1.52

</td>

<td style="text-align:right;">

0.13

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

ausualpl\_catNo

</td>

<td style="text-align:right;">

\-0.99

</td>

<td style="text-align:right;">

0.29

</td>

<td style="text-align:right;">

\-3.48

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat200–299%

</td>

<td style="text-align:right;">

0.14

</td>

<td style="text-align:right;">

0.16

</td>

<td style="text-align:right;">

0.83

</td>

<td style="text-align:right;">

0.41

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat300–399%

</td>

<td style="text-align:right;">

0.26

</td>

<td style="text-align:right;">

0.23

</td>

<td style="text-align:right;">

1.14

</td>

<td style="text-align:right;">

0.26

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat400–499%

</td>

<td style="text-align:right;">

\-0.11

</td>

<td style="text-align:right;">

0.21

</td>

<td style="text-align:right;">

\-0.53

</td>

<td style="text-align:right;">

0.60

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat\>=500%

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

0.20

</td>

<td style="text-align:right;">

1.53

</td>

<td style="text-align:right;">

0.13

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat\>=200%, no further detail

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

0.41

</td>

<td style="text-align:right;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

cover\_catPrivate/Military

</td>

<td style="text-align:right;">

0.77

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

2.50

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

cover\_catPublic

</td>

<td style="text-align:right;">

0.74

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

2.45

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

lcond\_chronic\_catYes

</td>

<td style="text-align:right;">

0.10

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

0.35

</td>

<td style="text-align:right;">

0.72

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

eth\_catNot Hispanic

</td>

<td style="text-align:right;">

\-0.76

</td>

<td style="text-align:right;">

0.23

</td>

<td style="text-align:right;">

\-3.26

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

race\_catAsian

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

0.56

</td>

<td style="text-align:right;">

0.71

</td>

<td style="text-align:right;">

0.48

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

race\_catBlack

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.86

</td>

<td style="text-align:right;">

0.39

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

race\_catWhite

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

imm\_statImmigrated

</td>

<td style="text-align:right;">

0.08

</td>

<td style="text-align:right;">

0.22

</td>

<td style="text-align:right;">

0.36

</td>

<td style="text-align:right;">

0.72

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<sup></sup> Standard errors: Robust

</td>

</tr>

</tfoot>

</table>

``` r
# test significance of individual terms/term groups
sig1 = tibble(vars) %>% 
  mutate(test = map(vars, ~regTermTest(mod, .x,  method = "LRT"))) %>% 
  mutate(pval = map_dbl(test, ~.x$p)) %>% 
  arrange(desc(pval)) %>% 
  mutate(sig = if_else(pval > 0.05, 0, 1))
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
sig1
```

    ## # A tibble: 9 x 4
    ##   vars              test            pval   sig
    ##   <chr>             <list>         <dbl> <dbl>
    ## 1 imm_stat          <rgTrTLRT> 3.07e-  1     0
    ## 2 race_cat          <rgTrTLRT> 2.82e-  2     1
    ## 3 cover_cat         <rgTrTLRT> 8.32e-  3     1
    ## 4 eth_cat           <rgTrTLRT> 1.06e-  3     1
    ## 5 ausualpl_cat      <rgTrTLRT> 3.40e-  4     1
    ## 6 educ_cat          <rgTrTLRT> 2.66e-  7     1
    ## 7 finc_cat          <rgTrTLRT> 5.10e- 13     1
    ## 8 age_cat           <rgTrTLRT> 7.91e- 34     1
    ## 9 lcond_chronic_cat <rgTrTLRT> 1.49e-210     1

``` r
mod2 <- svyglm(paprec_3bcat ~ age_cat + educ_cat + 
                     ausualpl_cat + finc_cat + cover_cat + lcond_chronic_cat + 
                     eth_cat + race_cat, design = des2, 
       family = binomial,
       subset = inc == 1) 
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
jtools::summ(mod2)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Observations

</td>

<td style="text-align:right;">

3148

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Dependent variable

</td>

<td style="text-align:right;">

paprec\_3bcat

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Type

</td>

<td style="text-align:right;">

Survey-weighted generalized linear
model

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Family

</td>

<td style="text-align:right;">

binomial

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Link

</td>

<td style="text-align:right;">

logit

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

Pseudo-R² (Cragg-Uhler)

</td>

<td style="text-align:right;">

0.09

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

Pseudo-R²
(McFadden)

</td>

<td style="text-align:right;">

0.31

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

AIC

</td>

<td style="text-align:right;">

2890.12

</td>

</tr>

</tbody>

</table>

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Est.

</th>

<th style="text-align:right;">

S.E.

</th>

<th style="text-align:right;">

t val.

</th>

<th style="text-align:right;">

p

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;font-weight: bold;">

(Intercept)

</td>

<td style="text-align:right;">

2.36

</td>

<td style="text-align:right;">

0.66

</td>

<td style="text-align:right;">

3.58

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

age\_cat40–49

</td>

<td style="text-align:right;">

\-1.00

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

\-3.07

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

age\_cat50–64

</td>

<td style="text-align:right;">

\-1.38

</td>

<td style="text-align:right;">

0.27

</td>

<td style="text-align:right;">

\-5.19

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

age\_cat65+

</td>

<td style="text-align:right;">

\-2.75

</td>

<td style="text-align:right;">

0.28

</td>

<td style="text-align:right;">

\-9.90

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

educ\_catHigh school

</td>

<td style="text-align:right;">

\-0.45

</td>

<td style="text-align:right;">

0.18

</td>

<td style="text-align:right;">

\-2.51

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

educ\_catLess than high school

</td>

<td style="text-align:right;">

\-0.87

</td>

<td style="text-align:right;">

0.21

</td>

<td style="text-align:right;">

\-4.14

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

educ\_catSome college

</td>

<td style="text-align:right;">

\-0.27

</td>

<td style="text-align:right;">

0.18

</td>

<td style="text-align:right;">

\-1.53

</td>

<td style="text-align:right;">

0.13

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

ausualpl\_catNo

</td>

<td style="text-align:right;">

\-0.99

</td>

<td style="text-align:right;">

0.29

</td>

<td style="text-align:right;">

\-3.48

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat200–299%

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

0.16

</td>

<td style="text-align:right;">

0.83

</td>

<td style="text-align:right;">

0.41

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat300–399%

</td>

<td style="text-align:right;">

0.26

</td>

<td style="text-align:right;">

0.23

</td>

<td style="text-align:right;">

1.14

</td>

<td style="text-align:right;">

0.25

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat400–499%

</td>

<td style="text-align:right;">

\-0.11

</td>

<td style="text-align:right;">

0.21

</td>

<td style="text-align:right;">

\-0.54

</td>

<td style="text-align:right;">

0.59

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat\>=500%

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

0.20

</td>

<td style="text-align:right;">

1.53

</td>

<td style="text-align:right;">

0.13

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

finc\_cat\>=200%, no further detail

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

0.41

</td>

<td style="text-align:right;">

0.68

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

cover\_catPrivate/Military

</td>

<td style="text-align:right;">

0.78

</td>

<td style="text-align:right;">

0.31

</td>

<td style="text-align:right;">

2.51

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

cover\_catPublic

</td>

<td style="text-align:right;">

0.74

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

2.45

</td>

<td style="text-align:right;">

0.01

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

lcond\_chronic\_catYes

</td>

<td style="text-align:right;">

0.10

</td>

<td style="text-align:right;">

0.29

</td>

<td style="text-align:right;">

0.35

</td>

<td style="text-align:right;">

0.73

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

eth\_catNot Hispanic

</td>

<td style="text-align:right;">

\-0.81

</td>

<td style="text-align:right;">

0.19

</td>

<td style="text-align:right;">

\-4.16

</td>

<td style="text-align:right;">

0.00

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

race\_catAsian

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.53

</td>

<td style="text-align:right;">

0.87

</td>

<td style="text-align:right;">

0.38

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

race\_catBlack

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.87

</td>

<td style="text-align:right;">

0.39

</td>

</tr>

<tr>

<td style="text-align:left;font-weight: bold;">

race\_catWhite

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

0.99

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="padding: 0; border: 0;" colspan="100%">

<sup></sup> Standard errors: Robust

</td>

</tr>

</tfoot>

</table>

``` r
sig2 = tibble(vars) %>% 
  filter(vars != "imm_stat") %>% 
  mutate(test = map(vars, ~regTermTest(mod, .x,  method = "LRT"))) %>% 
  mutate(pval = map_dbl(test, ~.x$p)) %>% 
  arrange(desc(pval)) %>% 
  mutate(sig = if_else(pval > 0.05, 0, 1))
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!
    
    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
sig2
```

    ## # A tibble: 8 x 4
    ##   vars              test            pval   sig
    ##   <chr>             <list>         <dbl> <dbl>
    ## 1 race_cat          <rgTrTLRT> 2.82e-  2     1
    ## 2 cover_cat         <rgTrTLRT> 8.32e-  3     1
    ## 3 eth_cat           <rgTrTLRT> 1.06e-  3     1
    ## 4 ausualpl_cat      <rgTrTLRT> 3.40e-  4     1
    ## 5 educ_cat          <rgTrTLRT> 2.66e-  7     1
    ## 6 finc_cat          <rgTrTLRT> 5.10e- 13     1
    ## 7 age_cat           <rgTrTLRT> 7.91e- 34     1
    ## 8 lcond_chronic_cat <rgTrTLRT> 1.49e-210     1

``` r
new_coef <- stringr::str_remove(names(coef(mod2)), "^[^_]*_cat")  #make pretty names
coef <- names(coef(mod2)) #assign pretty names
names(coef) <- new_coef #names original coef vector with pretty names
coef <- coef[-1] #remove intercept

jtools::plot_summs(mod2, ci_level = 0.95, 
                   coefs = coef,
                   exp = TRUE, robust = TRUE) + labs(title = "Pap Smear Significant Predictors")
```

![](papsmear_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->
