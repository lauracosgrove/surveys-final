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
                              rat_cat5 %in% c(96, 99) ~ "Unknown"))

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

#filters
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
  filter(age_p >= 25) %>% 
  filter(sex == 2)
```

# survey design

``` r
des <- svydesign(ids = ~psu_p, strata = ~strat_p, weights = ~wtfa_sa, nest = TRUE, data = pap_dat)
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
    ## 1            0  4454
    ## 2            1 10717
    ## 3           NA  1914

# unstratified descriptive stats

``` r
##nonstratified by age
#pap smear by age
age_pct <- svyby(~paprec_3bcat, by = ~age_cat, svymean, na.rm = TRUE, design = des)
age_pct %>% knitr::kable()
```

|       | age\_cat | paprec\_3bcat |        se |
| ----- | :------- | ------------: | --------: |
| 25–39 | 25–39    |     0.9118483 | 0.0056491 |
| 40–49 | 40–49    |     0.8360096 | 0.0097709 |
| 50–64 | 50–64    |     0.7572308 | 0.0084005 |
| 65+   | 65+      |     0.4513425 | 0.0109663 |

``` r
age_tot <- svyby(~paprec_3bcat, by = ~age_cat, svytotal, na.rm = TRUE, design = des)
age_tot  %>% knitr::kable()
```

|       | age\_cat | paprec\_3bcat |       se |
| ----- | :------- | ------------: | -------: |
| 25–39 | 25–39    |      24848829 | 603168.3 |
| 40–49 | 40–49    |      15402712 | 497181.9 |
| 50–64 | 50–64    |      22122078 | 542617.9 |
| 65+   | 65+      |      10411780 | 369833.2 |

``` r
#pap smear by education
edu_pct <- svyby(~paprec_3bcat, by = ~educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct %>% knitr::kable()
```

|                       | educ\_cat             | paprec\_3bcat |        se |
| --------------------- | :-------------------- | ------------: | --------: |
| College graduate      | College graduate      |     0.8389502 | 0.0061301 |
| High school           | High school           |     0.6540881 | 0.0109846 |
| Less than high school | Less than high school |     0.6242459 | 0.0151225 |
| Some college          | Some college          |     0.7477613 | 0.0078717 |

``` r
edu_tot <- svyby(~paprec_3bcat, by = ~educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot %>% knitr::kable()
```

|                       | educ\_cat             | paprec\_3bcat |       se |
| --------------------- | :-------------------- | ------------: | -------: |
| College graduate      | College graduate      |      28126961 | 665994.8 |
| High school           | High school           |      14654401 | 433286.2 |
| Less than high school | Less than high school |       7077365 | 290122.6 |
| Some college          | Some college          |      22579798 | 542206.9 |

``` r
#pap smear by finc
finc_pct <- svyby(~paprec_3bcat, by = ~finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct  %>% knitr::kable()
```

|                            | finc\_cat                  | paprec\_3bcat |        se |
| -------------------------- | :------------------------- | ------------: | --------: |
| \<200%                     | \<200%                     |     0.6813715 | 0.0087923 |
| \>=200%, no further detail | \>=200%, no further detail |     0.6605067 | 0.0333328 |
| \>=500%                    | \>=500%                    |     0.8295802 | 0.0089082 |
| 200–299%                   | 200–299%                   |     0.7206086 | 0.0128300 |
| 300–399%                   | 300–399%                   |     0.7642451 | 0.0144710 |
| 400–499%                   | 400–499%                   |     0.6996050 | 0.0157193 |
| Unknown                    | Unknown                    |     0.5812789 | 0.1140846 |

``` r
finc_tot <- svyby(~paprec_3bcat, by = ~finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot %>% knitr::kable()
```

|                            | finc\_cat                  | paprec\_3bcat |        se |
| -------------------------- | :------------------------- | ------------: | --------: |
| \<200%                     | \<200%                     |      19449221 | 460568.27 |
| \>=200%, no further detail | \>=200%, no further detail |       1788806 | 143051.07 |
| \>=500%                    | \>=500%                    |      20505140 | 608634.23 |
| 200–299%                   | 200–299%                   |       9447411 | 347393.60 |
| 300–399%                   | 300–399%                   |       7750416 | 321799.06 |
| 400–499%                   | 400–499%                   |       7488371 | 326456.48 |
| Unknown                    | Unknown                    |         67222 |  17657.63 |

``` r
#pap smear by usual care
ausualp_pct <- svyby(~paprec_3bcat, by = ~ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct %>% knitr::kable()
```

|       | ausualpl\_cat | paprec\_3bcat |        se |
| ----- | :------------ | ------------: | --------: |
| No    | No            |     0.6855347 | 0.0165861 |
| Other | Other         |     0.1684978 | 0.1736668 |
| Yes   | Yes           |     0.7482931 | 0.0047148 |

``` r
ausualp_tot <- svyby(~paprec_3bcat, by = ~ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot %>% knitr::kable()
```

|       | ausualpl\_cat | paprec\_3bcat |        se |
| ----- | :------------ | ------------: | --------: |
| No    | No            |       5505522 |  256455.5 |
| Other | Other         |          3466 |    3466.0 |
| Yes   | Yes           |      67276411 | 1000055.1 |

``` r
#pap smear by health coverage
cover_pct <- svyby(~paprec_3bcat, by = ~cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct %>% knitr::kable()
```

|                  | cover\_cat       | paprec\_3bcat |        se |
| ---------------- | :--------------- | ------------: | --------: |
| None             | None             |     0.6960178 | 0.0163527 |
| Private/Military | Private/Military |     0.7930466 | 0.0051312 |
| Public           | Public           |     0.6023640 | 0.0117842 |

``` r
cover_tot <- svyby(~paprec_3bcat, by = ~cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot %>% knitr::kable()
```

|                  | cover\_cat       | paprec\_3bcat |       se |
| ---------------- | :--------------- | ------------: | -------: |
| None             | None             |       5319273 | 220282.7 |
| Private/Military | Private/Military |      54048236 | 961840.4 |
| Public           | Public           |      13122914 | 410612.3 |

``` r
#pap smear by chronic conditions
lcond_chronic_pct <- svyby(~paprec_3bcat, by = ~lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct %>% knitr::kable()
```

|     | lcond\_chronic\_cat | paprec\_3bcat |        se |
| --- | :------------------ | ------------: | --------: |
| No  | No                  |     0.5851722 | 0.0735809 |
| Yes | Yes                 |     0.5664789 | 0.0128104 |

``` r
lcond_chronic_tot <- svyby(~paprec_3bcat, by = ~lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot %>% knitr::kable()
```

|     | lcond\_chronic\_cat | paprec\_3bcat |        se |
| --- | :------------------ | ------------: | --------: |
| No  | No                  |        228350 |  46155.76 |
| Yes | Yes                 |       9743073 | 327007.88 |

``` r
#pap smear by race
race_pct <- svyby(~paprec_3bcat, by = ~race_cat, svymean, na.rm = TRUE, design = des)
race_pct %>% knitr::kable()
```

|       | race\_cat | paprec\_3bcat |        se |
| ----- | :-------- | ------------: | --------: |
| AN/AI | AN/AI     |     0.7049639 | 0.0372061 |
| Asian | Asian     |     0.8026416 | 0.0184303 |
| Black | Black     |     0.7915125 | 0.0105988 |
| White | White     |     0.7315896 | 0.0049630 |

``` r
race_tot <- svyby(~paprec_3bcat, by = ~race_cat, svytotal, na.rm = TRUE, design = des)
race_tot %>% knitr::kable()
```

|       | race\_cat | paprec\_3bcat |       se |
| ----- | :-------- | ------------: | -------: |
| AN/AI | AN/AI     |        921961 | 110006.7 |
| Asian | Asian     |       4577200 | 239675.6 |
| Black | Black     |       9905861 | 331426.1 |
| White | White     |      57380377 | 961510.5 |

``` r
#pap smear by ethnicity
eth_pct <- svyby(~paprec_3bcat, by = ~eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct %>% knitr::kable()
```

|                    | eth\_cat           | paprec\_3bcat |        se |
| ------------------ | :----------------- | ------------: | --------: |
| Hispanic           | Hispanic           |     0.8012985 | 0.0110508 |
| Non-Hispanic AN/AI | Non-Hispanic AN/AI |     0.7163454 | 0.0475685 |
| Non-Hispanic Asian | Non-Hispanic Asian |     0.8007410 | 0.0187717 |
| Non-Hispanic Black | Non-Hispanic Black |     0.7890760 | 0.0106335 |
| Non-Hispanic White | Non-Hispanic White |     0.7187315 | 0.0055232 |

``` r
eth_tot <- svyby(~paprec_3bcat, by = ~eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot %>% knitr::kable()
```

|                    | eth\_cat           | paprec\_3bcat |        se |
| ------------------ | :----------------- | ------------: | --------: |
| Hispanic           | Hispanic           |      10717749 | 331516.09 |
| Non-Hispanic AN/AI | Non-Hispanic AN/AI |        632927 |  97175.69 |
| Non-Hispanic Asian | Non-Hispanic Asian |       4386154 | 232879.76 |
| Non-Hispanic Black | Non-Hispanic Black |       9292190 | 322545.77 |
| Non-Hispanic White | Non-Hispanic White |      47756379 | 898964.78 |

# stratified descriptive stats by age

``` r
#pap smear by education
edu_pct_strat <- svyby(~paprec_3bcat, by = ~age_cat+educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             | paprec\_3bcat |        se |
| --------------------------- | :------- | :-------------------- | ------------: | --------: |
| 25–39.College graduate      | 25–39    | College graduate      |     0.9471838 | 0.0078325 |
| 40–49.College graduate      | 40–49    | College graduate      |     0.9137014 | 0.0117034 |
| 50–64.College graduate      | 50–64    | College graduate      |     0.8260366 | 0.0125492 |
| 65+.College graduate        | 65+      | College graduate      |     0.5429112 | 0.0205418 |
| 25–39.High school           | 25–39    | High school           |     0.8543179 | 0.0169174 |
| 40–49.High school           | 40–49    | High school           |     0.7534464 | 0.0244711 |
| 50–64.High school           | 50–64    | High school           |     0.7150563 | 0.0194172 |
| 65+.High school             | 65+      | High school           |     0.4152511 | 0.0198915 |
| 25–39.Less than high school | 25–39    | Less than high school |     0.8654930 | 0.0197246 |
| 40–49.Less than high school | 40–49    | Less than high school |     0.7118792 | 0.0385087 |
| 50–64.Less than high school | 50–64    | Less than high school |     0.6607830 | 0.0308250 |
| 65+.Less than high school   | 65+      | Less than high school |     0.3728320 | 0.0271387 |
| 25–39.Some college          | 25–39    | Some college          |     0.9104437 | 0.0111332 |
| 40–49.Some college          | 40–49    | Some college          |     0.8311824 | 0.0194021 |
| 50–64.Some college          | 50–64    | Some college          |     0.7477419 | 0.0155282 |
| 65+.Some college            | 65+      | Some college          |     0.4621783 | 0.0214259 |

``` r
edu_tot_strat <- svyby(~paprec_3bcat, by = ~age_cat+educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             | paprec\_3bcat |       se |
| --------------------------- | :------- | :-------------------- | ------------: | -------: |
| 25–39.College graduate      | 25–39    | College graduate      |      10642150 | 398179.8 |
| 40–49.College graduate      | 40–49    | College graduate      |       6446124 | 309152.5 |
| 50–64.College graduate      | 50–64    | College graduate      |       8072821 | 367073.5 |
| 65+.College graduate        | 65+      | College graduate      |       2965866 | 174213.0 |
| 25–39.High school           | 25–39    | High school           |       3961003 | 233945.6 |
| 40–49.High school           | 40–49    | High school           |       2765357 | 185682.2 |
| 50–64.High school           | 50–64    | High school           |       4946632 | 233956.8 |
| 65+.High school             | 65+      | High school           |       2981409 | 193978.8 |
| 25–39.Less than high school | 25–39    | Less than high school |       2327141 | 170246.6 |
| 40–49.Less than high school | 40–49    | Less than high school |       1388272 | 119700.1 |
| 50–64.Less than high school | 50–64    | Less than high school |       1983911 | 151751.2 |
| 65+.Less than high school   | 65+      | Less than high school |       1378041 | 132380.6 |
| 25–39.Some college          | 25–39    | Some college          |       7845177 | 314373.7 |
| 40–49.Some college          | 40–49    | Some college          |       4656410 | 251549.0 |
| 50–64.Some college          | 50–64    | Some college          |       7053534 | 298796.0 |
| 65+.Some college            | 65+      | Some college          |       3024677 | 193841.5 |

``` r
#pap smear by finc
finc_pct_strat <- svyby(~paprec_3bcat, by = ~age_cat+finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct_strat  %>% knitr::kable()
```

|                                  | age\_cat | finc\_cat                  | paprec\_3bcat |        se |
| -------------------------------- | :------- | :------------------------- | ------------: | --------: |
| 25–39.\<200%                     | 25–39    | \<200%                     |     0.8771484 | 0.0098442 |
| 40–49.\<200%                     | 40–49    | \<200%                     |     0.7751960 | 0.0217382 |
| 50–64.\<200%                     | 50–64    | \<200%                     |     0.6536725 | 0.0192185 |
| 65+.\<200%                       | 65+      | \<200%                     |     0.3756896 | 0.0179107 |
| 25–39.\>=200%, no further detail | 25–39    | \>=200%, no further detail |     0.9091719 | 0.0524672 |
| 40–49.\>=200%, no further detail | 40–49    | \>=200%, no further detail |     0.7558410 | 0.0806729 |
| 50–64.\>=200%, no further detail | 50–64    | \>=200%, no further detail |     0.7372555 | 0.0655803 |
| 65+.\>=200%, no further detail   | 65+      | \>=200%, no further detail |     0.4557447 | 0.0502746 |
| 25–39.\>=500%                    | 25–39    | \>=500%                    |     0.9551460 | 0.0095834 |
| 40–49.\>=500%                    | 40–49    | \>=500%                    |     0.9168959 | 0.0142156 |
| 50–64.\>=500%                    | 50–64    | \>=500%                    |     0.8208860 | 0.0142663 |
| 65+.\>=500%                      | 65+      | \>=500%                    |     0.5600515 | 0.0247398 |
| 25–39.200–299%                   | 25–39    | 200–299%                   |     0.8808275 | 0.0162143 |
| 40–49.200–299%                   | 40–49    | 200–299%                   |     0.8113526 | 0.0308821 |
| 50–64.200–299%                   | 50–64    | 200–299%                   |     0.7498960 | 0.0265852 |
| 65+.200–299%                     | 65+      | 200–299%                   |     0.4287577 | 0.0265594 |
| 25–39.300–399%                   | 25–39    | 300–399%                   |     0.9237410 | 0.0146946 |
| 40–49.300–399%                   | 40–49    | 300–399%                   |     0.8192055 | 0.0311688 |
| 50–64.300–399%                   | 50–64    | 300–399%                   |     0.7482020 | 0.0311689 |
| 65+.300–399%                     | 65+      | 300–399%                   |     0.5125942 | 0.0356718 |
| 25–39.400–499%                   | 25–39    | 400–499%                   |     0.9342555 | 0.0191213 |
| 40–49.400–499%                   | 40–49    | 400–499%                   |     0.8472023 | 0.0334769 |
| 50–64.400–499%                   | 50–64    | 400–499%                   |     0.7724836 | 0.0257746 |
| 65+.400–499%                     | 65+      | 400–499%                   |     0.4458375 | 0.0247029 |
| 25–39.Unknown                    | 25–39    | Unknown                    |     1.0000000 | 0.0000000 |
| 40–49.Unknown                    | 40–49    | Unknown                    |     1.0000000 | 0.0000000 |
| 50–64.Unknown                    | 50–64    | Unknown                    |     1.0000000 | 0.0000000 |
| 65+.Unknown                      | 65+      | Unknown                    |     0.1100513 | 0.0984146 |

``` r
finc_tot_strat <- svyby(~paprec_3bcat, by = ~age_cat+finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot_strat %>% knitr::kable()
```

|                                  | age\_cat | finc\_cat                  | paprec\_3bcat |         se |
| -------------------------------- | :------- | :------------------------- | ------------: | ---------: |
| 25–39.\<200%                     | 25–39    | \<200%                     |       8260529 | 306216.076 |
| 40–49.\<200%                     | 40–49    | \<200%                     |       3928185 | 210278.364 |
| 50–64.\<200%                     | 50–64    | \<200%                     |       4652498 | 227517.921 |
| 65+.\<200%                       | 65+      | \<200%                     |       2608009 | 155437.564 |
| 25–39.\>=200%, no further detail | 25–39    | \>=200%, no further detail |        457448 |  74076.431 |
| 40–49.\>=200%, no further detail | 40–49    | \>=200%, no further detail |        319429 |  61441.032 |
| 50–64.\>=200%, no further detail | 50–64    | \>=200%, no further detail |        522678 |  75305.810 |
| 65+.\>=200%, no further detail   | 65+      | \>=200%, no further detail |        489251 |  69967.479 |
| 25–39.\>=500%                    | 25–39    | \>=500%                    |       5877894 | 305115.972 |
| 40–49.\>=500%                    | 40–49    | \>=500%                    |       4791995 | 276216.324 |
| 50–64.\>=500%                    | 50–64    | \>=500%                    |       7445240 | 329673.711 |
| 65+.\>=500%                      | 65+      | \>=500%                    |       2390011 | 174760.846 |
| 25–39.200–299%                   | 25–39    | 200–299%                   |       3501919 | 201082.795 |
| 40–49.200–299%                   | 40–49    | 200–299%                   |       2060524 | 169456.935 |
| 50–64.200–299%                   | 50–64    | 200–299%                   |       2468954 | 172107.532 |
| 65+.200–299%                     | 65+      | 200–299%                   |       1416014 | 122966.046 |
| 25–39.300–399%                   | 25–39    | 300–399%                   |       2984876 | 196534.720 |
| 40–49.300–399%                   | 40–49    | 300–399%                   |       1588867 | 141926.634 |
| 50–64.300–399%                   | 50–64    | 300–399%                   |       1996978 | 168466.918 |
| 65+.300–399%                     | 65+      | 300–399%                   |       1179695 | 104908.107 |
| 25–39.400–499%                   | 25–39    | 400–499%                   |       1554519 | 159203.224 |
| 40–49.400–499%                   | 40–49    | 400–499%                   |       1357562 | 142194.697 |
| 50–64.400–499%                   | 50–64    | 400–499%                   |       2980761 | 198805.337 |
| 65+.400–499%                     | 65+      | 400–499%                   |       1595529 | 122704.563 |
| 25–39.Unknown                    | 25–39    | Unknown                    |         33259 |   5546.581 |
| 40–49.Unknown                    | 40–49    | Unknown                    |          5549 |   5549.000 |
| 50–64.Unknown                    | 50–64    | Unknown                    |         22426 |  12554.212 |
| 65+.Unknown                      | 65+      | Unknown                    |          5988 |   5549.848 |

``` r
#pap smear by usual care
ausualp_pct_strat <- svyby(~paprec_3bcat, by = ~age_cat+ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat | paprec\_3bcat |        se |
| ----------- | :------- | :------------ | ------------: | --------: |
| 25–39.No    | 25–39    | No            |     0.8515891 | 0.0175078 |
| 40–49.No    | 40–49    | No            |     0.6443043 | 0.0433071 |
| 50–64.No    | 50–64    | No            |     0.4712996 | 0.0417092 |
| 65+.No      | 65+      | No            |     0.2589401 | 0.0531604 |
| 25–39.Other | 25–39    | Other         |     1.0000000 | 0.0000000 |
| 40–49.Other | 40–49    | Other         |     0.0000000 | 0.0000000 |
| 50–64.Other | 50–64    | Other         |     0.0000000 | 0.0000000 |
| 65+.Other   | 65+      | Other         |     0.0000000 | 0.0000000 |
| 25–39.Yes   | 25–39    | Yes           |     0.9225212 | 0.0061817 |
| 40–49.Yes   | 40–49    | Yes           |     0.8553270 | 0.0096282 |
| 50–64.Yes   | 50–64    | Yes           |     0.7748111 | 0.0084745 |
| 65+.Yes     | 65+      | Yes           |     0.4564902 | 0.0111519 |

``` r
ausualp_tot_strat <- svyby(~paprec_3bcat, by = ~age_cat+ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat | paprec\_3bcat |        se |
| ----------- | :------- | :------------ | ------------: | --------: |
| 25–39.No    | 25–39    | No            |       3495046 | 220043.13 |
| 40–49.No    | 40–49    | No            |       1070366 | 116134.74 |
| 50–64.No    | 50–64    | No            |        784458 |  95240.67 |
| 65+.No      | 65+      | No            |        155652 |  36751.91 |
| 25–39.Other | 25–39    | Other         |          3466 |   3466.00 |
| 40–49.Other | 40–49    | Other         |             0 |      0.00 |
| 50–64.Other | 50–64    | Other         |             0 |      0.00 |
| 65+.Other   | 65+      | Other         |             0 |      0.00 |
| 25–39.Yes   | 25–39    | Yes           |      21350317 | 555456.07 |
| 40–49.Yes   | 40–49    | Yes           |      14332346 | 482145.07 |
| 50–64.Yes   | 50–64    | Yes           |      21337620 | 534450.05 |
| 65+.Yes     | 65+      | Yes           |      10256128 | 368317.31 |

``` r
#pap smear by health coverage
cover_pct_strat <- svyby(~paprec_3bcat, by = ~age_cat+cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       | paprec\_3bcat |        se |
| ---------------------- | :------- | :--------------- | ------------: | --------: |
| 25–39.None             | 25–39    | None             |     0.8283526 | 0.0190853 |
| 40–49.None             | 40–49    | None             |     0.5916928 | 0.0376586 |
| 50–64.None             | 50–64    | None             |     0.5761341 | 0.0367198 |
| 65+.None               | 65+      | None             |     0.3855791 | 0.1452166 |
| 25–39.Private/Military | 25–39    | Private/Military |     0.9322334 | 0.0062056 |
| 40–49.Private/Military | 40–49    | Private/Military |     0.8794993 | 0.0098123 |
| 50–64.Private/Military | 50–64    | Private/Military |     0.7866563 | 0.0084727 |
| 65+.Private/Military   | 65+      | Private/Military |     0.4728668 | 0.0145351 |
| 25–39.Public           | 25–39    | Public           |     0.8968009 | 0.0127223 |
| 40–49.Public           | 40–49    | Public           |     0.7839200 | 0.0289562 |
| 50–64.Public           | 50–64    | Public           |     0.6506406 | 0.0293174 |
| 65+.Public             | 65+      | Public           |     0.4307551 | 0.0160159 |

``` r
cover_tot_strat <- svyby(~paprec_3bcat, by = ~age_cat+cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       | paprec\_3bcat |        se |
| ---------------------- | :------- | :--------------- | ------------: | --------: |
| 25–39.None             | 25–39    | None             |       2961517 | 166753.72 |
| 40–49.None             | 40–49    | None             |       1201244 | 113432.20 |
| 50–64.None             | 50–64    | None             |       1121887 | 107622.08 |
| 65+.None               | 65+      | None             |         34625 |  15439.93 |
| 25–39.Private/Military | 25–39    | Private/Military |      17518508 | 536954.72 |
| 40–49.Private/Military | 40–49    | Private/Military |      12284028 | 452995.52 |
| 50–64.Private/Military | 50–64    | Private/Military |      18825324 | 510167.66 |
| 65+.Private/Military   | 65+      | Private/Military |       5420376 | 252459.22 |
| 25–39.Public           | 25–39    | Public           |       4223847 | 224735.92 |
| 40–49.Public           | 40–49    | Public           |       1848038 | 137028.82 |
| 50–64.Public           | 50–64    | Public           |       2103920 | 143849.44 |
| 65+.Public             | 65+      | Public           |       4947109 | 253013.72 |

``` r
#pap smear by chronic conditions
lcond_chronic_pct_strat <- svyby(~paprec_3bcat, by = ~age_cat+lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat | paprec\_3bcat |        se |
| --------- | :------- | :------------------ | ------------: | --------: |
| 25–39.No  | 25–39    | No                  |     0.9059142 | 0.0734692 |
| 40–49.No  | 40–49    | No                  |     0.8732465 | 0.1025453 |
| 50–64.No  | 50–64    | No                  |     0.8492254 | 0.1026274 |
| 65+.No    | 65+      | No                  |     0.1835597 | 0.0753486 |
| 25–39.Yes | 25–39    | Yes                 |     0.8821307 | 0.0263871 |
| 40–49.Yes | 40–49    | Yes                 |     0.7366569 | 0.0358244 |
| 50–64.Yes | 50–64    | Yes                 |     0.6873337 | 0.0206148 |
| 65+.Yes   | 65+      | Yes                 |     0.3640214 | 0.0161840 |

``` r
lcond_chronic_tot_strat <- svyby(~paprec_3bcat, by = ~age_cat+lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat | paprec\_3bcat |        se |
| --------- | :------- | :------------------ | ------------: | --------: |
| 25–39.No  | 25–39    | No                  |         77385 |  31009.41 |
| 40–49.No  | 40–49    | No                  |         48494 |  20074.89 |
| 50–64.No  | 50–64    | No                  |         72354 |  26631.97 |
| 65+.No    | 65+      | No                  |         30117 |  12476.65 |
| 25–39.Yes | 25–39    | Yes                 |       1487133 | 132194.87 |
| 40–49.Yes | 40–49    | Yes                 |       1317810 | 111047.26 |
| 50–64.Yes | 50–64    | Yes                 |       4128689 | 223085.96 |
| 65+.Yes   | 65+      | Yes                 |       2809441 | 165615.32 |

``` r
#pap smear by race
race_pct_strat <- svyby(~paprec_3bcat, by = ~age_cat+race_cat, svymean, na.rm = TRUE, design = des)
race_pct_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat | paprec\_3bcat |        se |
| ----------- | :------- | :-------- | ------------: | --------: |
| 25–39.AN/AI | 25–39    | AN/AI     |     0.8412853 | 0.0501269 |
| 40–49.AN/AI | 40–49    | AN/AI     |     0.8286915 | 0.0640800 |
| 50–64.AN/AI | 50–64    | AN/AI     |     0.6342469 | 0.0814119 |
| 65+.AN/AI   | 65+      | AN/AI     |     0.3505602 | 0.1152580 |
| 25–39.Asian | 25–39    | Asian     |     0.8921702 | 0.0222786 |
| 40–49.Asian | 40–49    | Asian     |     0.8788452 | 0.0317331 |
| 50–64.Asian | 50–64    | Asian     |     0.7952636 | 0.0362177 |
| 65+.Asian   | 65+      | Asian     |     0.4752252 | 0.0453682 |
| 25–39.Black | 25–39    | Black     |     0.9277282 | 0.0119667 |
| 40–49.Black | 40–49    | Black     |     0.8755324 | 0.0201607 |
| 50–64.Black | 50–64    | Black     |     0.7836555 | 0.0220272 |
| 65+.Black   | 65+      | Black     |     0.4593838 | 0.0279215 |
| 25–39.White | 25–39    | White     |     0.9120055 | 0.0064214 |
| 40–49.White | 40–49    | White     |     0.8254604 | 0.0111748 |
| 50–64.White | 50–64    | White     |     0.7525809 | 0.0092745 |
| 65+.White   | 65+      | White     |     0.4504337 | 0.0119170 |

``` r
race_tot_strat <- svyby(~paprec_3bcat, by = ~age_cat+race_cat, svytotal, na.rm = TRUE, design = des)
race_tot_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat | paprec\_3bcat |        se |
| ----------- | :------- | :-------- | ------------: | --------: |
| 25–39.AN/AI | 25–39    | AN/AI     |        358120 |  62229.15 |
| 40–49.AN/AI | 40–49    | AN/AI     |        255087 |  64501.05 |
| 50–64.AN/AI | 50–64    | AN/AI     |        240169 |  46323.65 |
| 65+.AN/AI   | 65+      | AN/AI     |         68585 |  26487.87 |
| 25–39.Asian | 25–39    | Asian     |       1774713 | 143803.86 |
| 40–49.Asian | 40–49    | Asian     |       1222196 | 140017.63 |
| 50–64.Asian | 50–64    | Asian     |       1183928 | 107730.70 |
| 65+.Asian   | 65+      | Asian     |        396363 |  61128.83 |
| 25–39.Black | 25–39    | Black     |       3851803 | 194799.75 |
| 40–49.Black | 40–49    | Black     |       2082924 | 152347.77 |
| 50–64.Black | 50–64    | Black     |       2953378 | 177909.32 |
| 65+.Black   | 65+      | Black     |       1017756 |  91413.73 |
| 25–39.White | 25–39    | White     |      18864193 | 541115.69 |
| 40–49.White | 40–49    | White     |      11842505 | 443981.95 |
| 50–64.White | 50–64    | White     |      17744603 | 494147.22 |
| 65+.White   | 65+      | White     |       8929076 | 353462.07 |

``` r
#pap smear by ethnicity
eth_pct_strat <- svyby(~paprec_3bcat, by = ~age_cat+eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           | paprec\_3bcat |        se |
| ------------------------ | :------- | :----------------- | ------------: | --------: |
| 25–39.Hispanic           | 25–39    | Hispanic           |     0.8919156 | 0.0112860 |
| 40–49.Hispanic           | 40–49    | Hispanic           |     0.8469995 | 0.0211484 |
| 50–64.Hispanic           | 50–64    | Hispanic           |     0.7834117 | 0.0262670 |
| 65+.Hispanic             | 65+      | Hispanic           |     0.4970869 | 0.0369224 |
| 25–39.Non-Hispanic AN/AI | 25–39    | Non-Hispanic AN/AI |     0.8552614 | 0.0587696 |
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI |     0.8296240 | 0.0818666 |
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI |     0.6889181 | 0.0998406 |
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI |     0.3563445 | 0.1369989 |
| 25–39.Non-Hispanic Asian | 25–39    | Non-Hispanic Asian |     0.8897936 | 0.0232639 |
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian |     0.8869015 | 0.0318117 |
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian |     0.7901748 | 0.0368663 |
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian |     0.4784734 | 0.0458715 |
| 25–39.Non-Hispanic Black | 25–39    | Non-Hispanic Black |     0.9275256 | 0.0126295 |
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black |     0.8791014 | 0.0196815 |
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black |     0.7869242 | 0.0215570 |
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black |     0.4552759 | 0.0278532 |
| 25–39.Non-Hispanic White | 25–39    | Non-Hispanic White |     0.9181738 | 0.0077202 |
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White |     0.8190159 | 0.0130813 |
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White |     0.7466970 | 0.0099942 |
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White |     0.4457339 | 0.0124970 |

``` r
eth_tot_strat <- svyby(~paprec_3bcat, by = ~age_cat+eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           | paprec\_3bcat |        se |
| ------------------------ | :------- | :----------------- | ------------: | --------: |
| 25–39.Hispanic           | 25–39    | Hispanic           |       4693646 | 196105.67 |
| 40–49.Hispanic           | 40–49    | Hispanic           |       2633822 | 165859.86 |
| 50–64.Hispanic           | 50–64    | Hispanic           |       2471049 | 158508.55 |
| 65+.Hispanic             | 65+      | Hispanic           |        919232 |  87782.84 |
| 25–39.Non-Hispanic AN/AI | 25–39    | Non-Hispanic AN/AI |        217995 |  53115.51 |
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI |        181832 |  57696.65 |
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI |        180593 |  40989.18 |
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI |         52507 |  24309.36 |
| 25–39.Non-Hispanic Asian | 25–39    | Non-Hispanic Asian |       1682476 | 143539.76 |
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian |       1161158 | 134924.81 |
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian |       1147823 | 106510.51 |
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian |        394697 |  61528.78 |
| 25–39.Non-Hispanic Black | 25–39    | Non-Hispanic Black |       3521439 | 186171.62 |
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black |       1973290 | 150025.14 |
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black |       2815419 | 172509.86 |
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black |        982042 |  89606.05 |
| 25–39.Non-Hispanic White | 25–39    | Non-Hispanic White |      14733273 | 500118.23 |
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White |       9452610 | 398967.96 |
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White |      15507194 | 478730.90 |
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White |       8063302 | 346315.30 |
