Colorectal Screening
================
Justin Hsie
11/25/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(survey)
```

    ## Loading required package: grid

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survey'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     dotchart

Data Import

``` r
cancer = read_csv("./data/cancerxx.csv") %>% 
  janitor::clean_names() %>% 
  select(hhx, fmx, fpx, wtfa_sa, strat_p, psu_p, region, hfobhad1, 
         rhfo2_mt, rhfo2yr, rhfo2n, rhfo2t, rhfo2, rhfob3a,
         rhfob3b, hfobrea2, fobhad1, rfob2_mt, rfob2yr,
         rfob2n, rfob2t, rofob3a, rofob3b, rfobres1)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FPX = col_character(),
    ##   FMX = col_character(),
    ##   FN_AGE10 = col_logical(),
    ##   FN_AGE12 = col_logical(),
    ##   FN_AGE14 = col_logical(),
    ##   FN_AGE15 = col_logical(),
    ##   FN_AGE25 = col_logical(),
    ##   FN_AGE28 = col_logical(),
    ##   FN_AGE33 = col_logical(),
    ##   FN_MAN10 = col_logical(),
    ##   FN_MAN12 = col_logical(),
    ##   FN_MAN14 = col_logical(),
    ##   FN_MAN15 = col_logical(),
    ##   FN_MAN25 = col_logical(),
    ##   FN_MAN28 = col_logical(),
    ##   FN_MAN33 = col_logical(),
    ##   HHX = col_character()
    ## )

    ## See spec(...) for full column specifications.

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
adult = read_csv("./data/samadult.csv") %>%
  janitor::clean_names() %>% 
  select(hhx, fmx, fpx, ausualpl, ahcplrou, ahcplknd, fla1ar)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FPX = col_character(),
    ##   FMX = col_character(),
    ##   HHX = col_character(),
    ##   CNKIND31 = col_logical(),
    ##   CANAGE2 = col_logical(),
    ##   CANAGE4 = col_logical(),
    ##   CANAGE8 = col_logical(),
    ##   CANAGE9 = col_logical(),
    ##   CANAGE13 = col_logical(),
    ##   CANAGE17 = col_logical(),
    ##   CANAGE19 = col_logical(),
    ##   CANAGE24 = col_logical(),
    ##   CANAGE25 = col_logical(),
    ##   CANAGE27 = col_logical(),
    ##   ALTIME26 = col_logical(),
    ##   ALTIME27 = col_logical(),
    ##   ALTIME29 = col_logical(),
    ##   ALTIME30 = col_logical(),
    ##   ALTIME34 = col_logical(),
    ##   ALUNIT26 = col_logical()
    ##   # ... with 24 more columns
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: 650 parsing failures.
    ##  row      col           expected actual                  file
    ## 1056 ALTIME34 1/0/T/F/TRUE/FALSE     6  './data/samadult.csv'
    ## 1056 ALUNIT34 1/0/T/F/TRUE/FALSE     3  './data/samadult.csv'
    ## 1056 ALDURB34 1/0/T/F/TRUE/FALSE     3  './data/samadult.csv'
    ## 1056 ALCHRC34 1/0/T/F/TRUE/FALSE     2  './data/samadult.csv'
    ## 1101 CANAGE24 1/0/T/F/TRUE/FALSE     35 './data/samadult.csv'
    ## .... ........ .................. ...... .....................
    ## See problems(...) for more details.

``` r
family = read_csv("./data/familyxx.csv") %>%
  janitor::clean_names() %>% 
  select(hhx, fmx, rat_cat4, rat_cat5)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FMX = col_character(),
    ##   HHX = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
person = read_csv("./data/personsx.csv") %>%
  janitor::clean_names() %>% 
  select(hhx, fmx, fpx, age_p, educ1, sex, notcov, cover65, cover65o, 
         la1ar, lcondrt, lachronr, hiscodi3, racreci3, cover)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   FPX = col_character(),
    ##   AGE_CHG = col_logical(),
    ##   FSPOUS2 = col_character(),
    ##   FCOHAB3 = col_character(),
    ##   FMX = col_character(),
    ##   HHREFLG = col_character(),
    ##   FMREFLG = col_character(),
    ##   FMRPFLG = col_character(),
    ##   FMOTHER1 = col_character(),
    ##   FFATHER1 = col_character(),
    ##   HHX = col_character(),
    ##   LCTIME5 = col_logical(),
    ##   LCUNIT5 = col_logical(),
    ##   LCTIME6 = col_logical(),
    ##   LCUNIT6 = col_logical(),
    ##   LCTIME10 = col_logical(),
    ##   LCUNIT10 = col_logical(),
    ##   LCTIME11 = col_logical(),
    ##   LCUNIT11 = col_logical(),
    ##   LCTIME90 = col_logical()
    ##   # ... with 68 more columns
    ## )
    ## See spec(...) for full column specifications.

    ## Warning: 2523 parsing failures.
    ##  row      col           expected actual                  file
    ## 1265 LCTIME5  1/0/T/F/TRUE/FALSE     96 './data/personsx.csv'
    ## 1265 LCUNIT5  1/0/T/F/TRUE/FALSE     6  './data/personsx.csv'
    ## 1265 LCDURA5  1/0/T/F/TRUE/FALSE     10 './data/personsx.csv'
    ## 1265 LCDURB5  1/0/T/F/TRUE/FALSE     4  './data/personsx.csv'
    ## 1422 LAUNIT31 1/0/T/F/TRUE/FALSE     4  './data/personsx.csv'
    ## .... ........ .................. ...... .....................
    ## See problems(...) for more details.

``` r
col_dat = cancer %>%
  left_join(adult, by = c("hhx", "fmx", "fpx")) %>%
  left_join(person, by = c("hhx", "fmx", "fpx")) %>%
  left_join(family, by = c("hhx", "fmx"))
```

Data Manipulation

``` r
#home stool test within last year
col_dat = col_dat %>%
  mutate(col_2 = if_else(rhfo2 <= 1, 1, 0))

# create the age category
col_dat = col_dat %>% 
  mutate(age_cat = case_when(age_p >= 25 & age_p < 40 ~ "25–39",
                             age_p >= 40 & age_p < 50 ~ "40–49",
                             age_p >= 50 & age_p < 65 ~ "50–64",
                             age_p >= 65 ~ "65+"))
# create educ category
col_dat = col_dat %>% 
  mutate(educ_cat = case_when(educ1 < 13 ~ "Less than high school",
                              educ1 >= 13 & educ1 < 15 ~ "High school",
                              educ1 >= 15 & educ1 < 18 ~ "Some college",
                              educ1 >= 18 & educ1 <= 21 ~ "College graduate"))

# create financial category
col_dat = col_dat %>% 
  mutate(finc_cat = case_when(rat_cat5 <= 7 |  rat_cat5 %in% c(15, 16) ~ "<200%",
                              rat_cat5 %in% c(8, 9) ~ "200–299%", 
                              rat_cat5 %in% c(10, 11) ~ "300–399%",
                              rat_cat5 >= 18 & educ1 <= 21 ~ "400–499%",
                              rat_cat5 == 14  ~">=500%",
                              rat_cat5 == 17  ~">=200%, no further detail",
                              rat_cat5 %in% c(96, 99) ~ "Unknown"))

# create as usual category
col_dat = col_dat %>% 
  mutate(ausualpl_cat  = case_when(ausualpl == 2 ~ "No",
                                   ausualpl %in% c(1, 3) ~ "Yes",
                                   ausualpl %in% c(7, 8, 9) ~ "Other"))
# coverage status
col_dat = col_dat %>% 
  mutate(cover_cat  = case_when(notcov == 1 | cover == 4 | cover65 == 6 ~ "None",
                                cover == 2 | cover65 %in% 2:4 ~ "Public",
                                cover %in% c(1, 3) | cover65 %in% c(1, 5) ~
                                  "Private/Military"))

# disability
col_dat = col_dat %>% 
  mutate(lcond_chronic_cat = if_else(lcondrt == 1, "Yes", "No"))

# race
col_dat = col_dat %>% 
  mutate(race_cat = case_when(racreci3 == 1 ~ "White",
                              racreci3 == 2 ~ "Black",
                              racreci3 == 3 ~ "Asian",
                              racreci3 == 4 ~ "AN/AI"),
         eth_cat = case_when(hiscodi3 == 1 ~ "Hispanic",
                             hiscodi3 == 2 ~ "Non-Hispanic White",
                             hiscodi3 == 3 ~ "Non-Hispanic Black",
                             hiscodi3 == 4 ~ "Non-Hispanic Asian",
                             hiscodi3 == 5 ~ "Non-Hispanic AN/AI"))
```

Survey Design

``` r
des = svydesign(ids = ~ psu_p, strata = ~ strat_p, 
                weights = ~ wtfa_sa, nest = TRUE, data = col_dat)
```

Tables

``` r
#percentage
age_pct = svyby(~col_2, by = ~age_cat, svymean, na.rm = TRUE, design = des)
age_pct %>% knitr::kable()
```

|       | age\_cat |    col\_2 |        se |
| ----- | :------- | --------: | --------: |
| 25–39 | 25–39    | 0.0000000 | 0.0000000 |
| 40–49 | 40–49    | 0.0242476 | 0.0132072 |
| 50–64 | 50–64    | 0.0501523 | 0.0091700 |
| 65+   | 65+      | 0.0560049 | 0.0079592 |

``` r
#total
age_tot = svyby(~col_2, by = ~age_cat, svytotal, na.rm = TRUE, design = des)
age_tot  %>% knitr::kable()
```

|       | age\_cat | col\_2 |       se |
| ----- | :------- | -----: | -------: |
| 25–39 | 25–39    |      0 |     0.00 |
| 40–49 | 40–49    |  26883 | 14878.36 |
| 50–64 | 50–64    | 320599 | 60184.50 |
| 65+   | 65+      | 513198 | 75947.86 |

``` r
#education
edu_pct = svyby(~col_2, by = ~educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct %>% knitr::kable()
```

|                       | educ\_cat             |    col\_2 |        se |
| --------------------- | :-------------------- | --------: | --------: |
| College graduate      | College graduate      | 0.0432943 | 0.0085130 |
| High school           | High school           | 0.0529747 | 0.0128739 |
| Less than high school | Less than high school | 0.0835979 | 0.0187281 |
| Some college          | Some college          | 0.0495303 | 0.0103965 |

``` r
#education total
edu_tot = svyby(~col_2, by = ~educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot %>% knitr::kable()
```

|                       | educ\_cat             | col\_2 |       se |
| --------------------- | :-------------------- | -----: | -------: |
| College graduate      | College graduate      | 247570 | 49365.75 |
| High school           | High school           | 213476 | 53580.98 |
| Less than high school | Less than high school | 146626 | 33835.71 |
| Some college          | Some college          | 253008 | 53690.00 |

``` r
#finc
finc_pct = svyby(~col_2, by = ~finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct  %>% knitr::kable()
```

|                            | finc\_cat                  |    col\_2 |        se |
| -------------------------- | :------------------------- | --------: | --------: |
| \<200%                     | \<200%                     | 0.0559747 | 0.0115947 |
| \>=200%, no further detail | \>=200%, no further detail | 0.0652067 | 0.0322066 |
| \>=500%                    | \>=500%                    | 0.0431301 | 0.0105471 |
| 200–299%                   | 200–299%                   | 0.0629765 | 0.0178133 |
| 300–399%                   | 300–399%                   | 0.0337548 | 0.0133127 |
| 400–499%                   | 400–499%                   | 0.0786185 | 0.0200258 |
| Unknown                    | Unknown                    | 0.0000000 | 0.0000000 |

``` r
#finc total
finc_tot = svyby(~col_2, by = ~finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot %>% knitr::kable()
```

|                            | finc\_cat                  | col\_2 |       se |
| -------------------------- | :------------------------- | -----: | -------: |
| \<200%                     | \<200%                     | 207461 | 44111.64 |
| \>=200%, no further detail | \>=200%, no further detail |  45963 | 23309.43 |
| \>=500%                    | \>=500%                    | 197884 | 48168.04 |
| 200–299%                   | 200–299%                   | 139879 | 41020.86 |
| 300–399%                   | 300–399%                   |  56867 | 22961.36 |
| 400–499%                   | 400–499%                   | 198770 | 52368.19 |
| Unknown                    | Unknown                    |      0 |     0.00 |

``` r
#usual care
ausualp_pct = svyby(~col_2, by = ~ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct %>% knitr::kable()
```

|       | ausualpl\_cat |    col\_2 |        se |
| ----- | :------------ | --------: | --------: |
| No    | No            | 0.0658332 | 0.0380378 |
| Other | Other         | 0.0000000 | 0.0000000 |
| Yes   | Yes           | 0.0512443 | 0.0060074 |

``` r
#usual care total
ausualp_tot = svyby(~col_2, by = ~ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot %>% knitr::kable()
```

|       | ausualpl\_cat | col\_2 |       se |
| ----- | :------------ | -----: | -------: |
| No    | No            |  32796 | 17860.96 |
| Other | Other         |      0 |     0.00 |
| Yes   | Yes           | 827884 | 98587.45 |

``` r
#health coverage
cover_pct = svyby(~col_2, by = ~cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct %>% knitr::kable()
```

|                  | cover\_cat       |    col\_2 |        se |
| ---------------- | :--------------- | --------: | --------: |
| None             | None             | 0.0567792 | 0.0302743 |
| Private/Military | Private/Military | 0.0478797 | 0.0069155 |
| Public           | Public           | 0.0611999 | 0.0121303 |

``` r
#health coverage total
cover_tot = svyby(~col_2, by = ~cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot %>% knitr::kable()
```

|                  | cover\_cat       | col\_2 |       se |
| ---------------- | :--------------- | -----: | -------: |
| None             | None             |  18341 | 10272.79 |
| Private/Military | Private/Military | 555557 | 80425.26 |
| Public           | Public           | 286782 | 56851.29 |

``` r
#chronic conditions
lcond_chronic_pct = svyby(~col_2, by = ~lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct %>% knitr::kable()
```

|     | lcond\_chronic\_cat |    col\_2 |        se |
| --- | :------------------ | --------: | --------: |
| No  | No                  | 0.1749593 | 0.1181827 |
| Yes | Yes                 | 0.0504843 | 0.0103390 |

``` r
#chronic conditions total
lcond_chronic_tot = svyby(~col_2, by = ~lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot %>% knitr::kable()
```

|     | lcond\_chronic\_cat | col\_2 |       se |
| --- | :------------------ | -----: | -------: |
| No  | No                  |  14396 | 10395.89 |
| Yes | Yes                 | 259452 | 55657.21 |

``` r
#race
race_pct = svyby(~col_2, by = ~race_cat, svymean, na.rm = TRUE, design = des)
race_pct %>% knitr::kable()
```

|       | race\_cat |    col\_2 |        se |
| ----- | :-------- | --------: | --------: |
| AN/AI | AN/AI     | 0.0918380 | 0.0491059 |
| Asian | Asian     | 0.1011152 | 0.0411859 |
| Black | Black     | 0.0966555 | 0.0266832 |
| White | White     | 0.0449669 | 0.0061850 |

``` r
#race total
race_tot = svyby(~col_2, by = ~race_cat, svytotal, na.rm = TRUE, design = des)
race_tot %>% knitr::kable()
```

|       | race\_cat | col\_2 |       se |
| ----- | :-------- | -----: | -------: |
| AN/AI | AN/AI     |  21508 | 11109.11 |
| Asian | Asian     |  55243 | 25314.27 |
| Black | Black     | 130279 | 37252.45 |
| White | White     | 653650 | 91465.68 |

``` r
#ethnicity
eth_pct = svyby(~col_2, by = ~eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct %>% knitr::kable()
```

|                    | eth\_cat           |    col\_2 |        se |
| ------------------ | :----------------- | --------: | --------: |
| Hispanic           | Hispanic           | 0.0910891 | 0.0195652 |
| Non-Hispanic AN/AI | Non-Hispanic AN/AI | 0.0703206 | 0.0484005 |
| Non-Hispanic Asian | Non-Hispanic Asian | 0.1076050 | 0.0435295 |
| Non-Hispanic Black | Non-Hispanic Black | 0.0902316 | 0.0248582 |
| Non-Hispanic White | Non-Hispanic White | 0.0423973 | 0.0063856 |

``` r
#ethnicity total
eth_tot = svyby(~col_2, by = ~eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot %>% knitr::kable()
```

|                    | eth\_cat           | col\_2 |       se |
| ------------------ | :----------------- | -----: | -------: |
| Hispanic           | Hispanic           |  96811 | 20724.42 |
| Non-Hispanic AN/AI | Non-Hispanic AN/AI |  14779 |  9980.01 |
| Non-Hispanic Asian | Non-Hispanic Asian |  55243 | 25314.27 |
| Non-Hispanic Black | Non-Hispanic Black | 118930 | 33731.39 |
| Non-Hispanic White | Non-Hispanic White | 574917 | 87947.65 |

Tables by Age

``` r
#education
edu_pct_strat = svyby(~col_2, by = ~age_cat+educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             |    col\_2 |        se |
| --------------------------- | :------- | :-------------------- | --------: | --------: |
| 25–39.College graduate      | 25–39    | College graduate      | 0.0000000 | 0.0000000 |
| 40–49.College graduate      | 40–49    | College graduate      | 0.0176389 | 0.0175208 |
| 50–64.College graduate      | 50–64    | College graduate      | 0.0384105 | 0.0126712 |
| 65+.College graduate        | 65+      | College graduate      | 0.0515906 | 0.0129772 |
| 25–39.High school           | 25–39    | High school           | 0.0000000 | 0.0000000 |
| 40–49.High school           | 40–49    | High school           | 0.0582745 | 0.0452753 |
| 50–64.High school           | 50–64    | High school           | 0.0465396 | 0.0196704 |
| 65+.High school             | 65+      | High school           | 0.0560237 | 0.0169129 |
| 25–39.Less than high school | 25–39    | Less than high school | 0.0000000 | 0.0000000 |
| 40–49.Less than high school | 40–49    | Less than high school | 0.0000000 | 0.0000000 |
| 50–64.Less than high school | 50–64    | Less than high school | 0.1146182 | 0.0449764 |
| 65+.Less than high school   | 65+      | Less than high school | 0.0682056 | 0.0190201 |
| 25–39.Some college          | 25–39    | Some college          | 0.0000000 | 0.0000000 |
| 40–49.Some college          | 40–49    | Some college          | 0.0101141 | 0.0102001 |
| 50–64.Some college          | 50–64    | Some college          | 0.0471921 | 0.0153282 |
| 65+.Some college            | 65+      | Some college          | 0.0564221 | 0.0148777 |

``` r
#education total
edu_tot_strat = svyby(~col_2, by = ~age_cat+educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             | col\_2 |       se |
| --------------------------- | :------- | :-------------------- | -----: | -------: |
| 25–39.College graduate      | 25–39    | College graduate      |      0 |     0.00 |
| 40–49.College graduate      | 40–49    | College graduate      |   8599 |  8599.00 |
| 50–64.College graduate      | 50–64    | College graduate      |  90020 | 29432.74 |
| 65+.College graduate        | 65+      | College graduate      | 148951 | 38410.51 |
| 25–39.High school           | 25–39    | High school           |      0 |     0.00 |
| 40–49.High school           | 40–49    | High school           |  14761 | 11619.44 |
| 50–64.High school           | 50–64    | High school           |  63091 | 27291.41 |
| 65+.High school             | 65+      | High school           | 135624 | 42564.27 |
| 25–39.Less than high school | 25–39    | Less than high school |      0 |     0.00 |
| 40–49.Less than high school | 40–49    | Less than high school |      0 |     0.00 |
| 50–64.Less than high school | 50–64    | Less than high school |  69965 | 28144.37 |
| 65+.Less than high school   | 65+      | Less than high school |  76661 | 21753.78 |
| 25–39.Some college          | 25–39    | Some college          |      0 |     0.00 |
| 40–49.Some college          | 40–49    | Some college          |   3523 |  3523.00 |
| 50–64.Some college          | 50–64    | Some college          |  97523 | 32330.83 |
| 65+.Some college            | 65+      | Some college          | 151962 | 40781.52 |

``` r
#finc
finc_pct_strat = svyby(~col_2, by = ~age_cat+finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct_strat  %>% knitr::kable()
```

|                                  | age\_cat | finc\_cat                  |    col\_2 |        se |
| -------------------------------- | :------- | :------------------------- | --------: | --------: |
| 25–39.\<200%                     | 25–39    | \<200%                     | 0.0000000 | 0.0000000 |
| 40–49.\<200%                     | 40–49    | \<200%                     | 0.0202414 | 0.0203599 |
| 50–64.\<200%                     | 50–64    | \<200%                     | 0.0599149 | 0.0180956 |
| 65+.\<200%                       | 65+      | \<200%                     | 0.0560606 | 0.0150386 |
| 25–39.\>=200%, no further detail | 25–39    | \>=200%, no further detail | 0.0000000 | 0.0000000 |
| 40–49.\>=200%, no further detail | 40–49    | \>=200%, no further detail | 0.0000000 | 0.0000000 |
| 50–64.\>=200%, no further detail | 50–64    | \>=200%, no further detail | 0.0465024 | 0.0463602 |
| 65+.\>=200%, no further detail   | 65+      | \>=200%, no further detail | 0.0722225 | 0.0396187 |
| 25–39.\>=500%                    | 25–39    | \>=500%                    | 0.0000000 | 0.0000000 |
| 40–49.\>=500%                    | 40–49    | \>=500%                    | 0.0610962 | 0.0387010 |
| 50–64.\>=500%                    | 50–64    | \>=500%                    | 0.0468751 | 0.0165927 |
| 65+.\>=500%                      | 65+      | \>=500%                    | 0.0362256 | 0.0144696 |
| 25–39.200–299%                   | 25–39    | 200–299%                   | 0.0000000 | 0.0000000 |
| 40–49.200–299%                   | 40–49    | 200–299%                   | 0.0087680 | 0.0092296 |
| 50–64.200–299%                   | 50–64    | 200–299%                   | 0.0646203 | 0.0345842 |
| 65+.200–299%                     | 65+      | 200–299%                   | 0.0677208 | 0.0226340 |
| 25–39.300–399%                   | 25–39    | 300–399%                   | 0.0000000 | 0.0000000 |
| 40–49.300–399%                   | 40–49    | 300–399%                   | 0.0000000 | 0.0000000 |
| 50–64.300–399%                   | 50–64    | 300–399%                   | 0.0502669 | 0.0253551 |
| 65+.300–399%                     | 65+      | 300–399%                   | 0.0306652 | 0.0179009 |
| 25–39.400–499%                   | 25–39    | 400–499%                   | 0.0000000 | 0.0000000 |
| 40–49.400–499%                   | 40–49    | 400–499%                   | 0.0000000 | 0.0000000 |
| 50–64.400–499%                   | 50–64    | 400–499%                   | 0.0569698 | 0.0303246 |
| 65+.400–499%                     | 65+      | 400–499%                   | 0.1008534 | 0.0255721 |
| 25–39.Unknown                    | 25–39    | Unknown                    | 0.0000000 | 0.0000000 |
| 40–49.Unknown                    | 40–49    | Unknown                    | 0.0000000 | 0.0000000 |
| 50–64.Unknown                    | 50–64    | Unknown                    | 0.0000000 | 0.0000000 |
| 65+.Unknown                      | 65+      | Unknown                    | 0.0000000 | 0.0000000 |

``` r
#finc total
finc_tot_strat = svyby(~col_2, by = ~age_cat+finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot_strat %>% knitr::kable()
```

|                                  | age\_cat | finc\_cat                  | col\_2 |       se |
| -------------------------------- | :------- | :------------------------- | -----: | -------: |
| 25–39.\<200%                     | 25–39    | \<200%                     |      0 |     0.00 |
| 40–49.\<200%                     | 40–49    | \<200%                     |   3523 |  3523.00 |
| 50–64.\<200%                     | 50–64    | \<200%                     |  91963 | 28948.64 |
| 65+.\<200%                       | 65+      | \<200%                     | 111975 | 30889.48 |
| 25–39.\>=200%, no further detail | 25–39    | \>=200%, no further detail |      0 |     0.00 |
| 40–49.\>=200%, no further detail | 40–49    | \>=200%, no further detail |      0 |     0.00 |
| 50–64.\>=200%, no further detail | 50–64    | \>=200%, no further detail |   7115 |  7115.00 |
| 65+.\>=200%, no further detail   | 65+      | \>=200%, no further detail |  38848 | 22196.99 |
| 25–39.\>=500%                    | 25–39    | \>=500%                    |      0 |     0.00 |
| 40–49.\>=500%                    | 40–49    | \>=500%                    |  22096 | 14399.88 |
| 50–64.\>=500%                    | 50–64    | \>=500%                    |  99845 | 35495.43 |
| 65+.\>=500%                      | 65+      | \>=500%                    |  75943 | 31209.34 |
| 25–39.200–299%                   | 25–39    | 200–299%                   |      0 |     0.00 |
| 40–49.200–299%                   | 40–49    | 200–299%                   |   1264 |  1264.00 |
| 50–64.200–299%                   | 50–64    | 200–299%                   |  42496 | 23528.51 |
| 65+.200–299%                     | 65+      | 200–299%                   |  96119 | 33514.11 |
| 25–39.300–399%                   | 25–39    | 300–399%                   |      0 |     0.00 |
| 40–49.300–399%                   | 40–49    | 300–399%                   |      0 |     0.00 |
| 50–64.300–399%                   | 50–64    | 300–399%                   |  25496 | 13745.98 |
| 65+.300–399%                     | 65+      | 300–399%                   |  31371 | 18430.38 |
| 25–39.400–499%                   | 25–39    | 400–499%                   |      0 |     0.00 |
| 40–49.400–499%                   | 40–49    | 400–499%                   |      0 |     0.00 |
| 50–64.400–499%                   | 50–64    | 400–499%                   |  52297 | 28536.34 |
| 65+.400–499%                     | 65+      | 400–499%                   | 146473 | 40524.58 |
| 25–39.Unknown                    | 25–39    | Unknown                    |      0 |     0.00 |
| 40–49.Unknown                    | 40–49    | Unknown                    |      0 |     0.00 |
| 50–64.Unknown                    | 50–64    | Unknown                    |      0 |     0.00 |
| 65+.Unknown                      | 65+      | Unknown                    |      0 |     0.00 |

``` r
#usual care
ausualp_pct_strat = svyby(~col_2, by = ~age_cat+ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat |    col\_2 |        se |
| ----------- | :------- | :------------ | --------: | --------: |
| 25–39.No    | 25–39    | No            | 0.0000000 | 0.0000000 |
| 40–49.No    | 40–49    | No            | 0.0000000 | 0.0000000 |
| 50–64.No    | 50–64    | No            | 0.0324944 | 0.0321795 |
| 65+.No      | 65+      | No            | 0.1405067 | 0.0926791 |
| 25–39.Other | 25–39    | Other         | 0.0000000 | 0.0000000 |
| 40–49.Other | 40–49    | Other         | 0.0000000 | 0.0000000 |
| 50–64.Other | 50–64    | Other         | 0.0000000 | 0.0000000 |
| 65+.Other   | 65+      | Other         | 0.0000000 | 0.0000000 |
| 25–39.Yes   | 25–39    | Yes           | 0.0000000 | 0.0000000 |
| 40–49.Yes   | 40–49    | Yes           | 0.0265830 | 0.0144412 |
| 50–64.Yes   | 50–64    | Yes           | 0.0508643 | 0.0094393 |
| 65+.Yes     | 65+      | Yes           | 0.0542823 | 0.0079371 |

``` r
#usual care total
ausualp_tot_strat = svyby(~col_2, by = ~age_cat+ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat | col\_2 |       se |
| ----------- | :------- | :------------ | -----: | -------: |
| 25–39.No    | 25–39    | No            |      0 |     0.00 |
| 40–49.No    | 40–49    | No            |      0 |     0.00 |
| 50–64.No    | 50–64    | No            |   7074 |  7074.00 |
| 65+.No      | 65+      | No            |  25722 | 16400.38 |
| 25–39.Other | 25–39    | Other         |      0 |     0.00 |
| 40–49.Other | 40–49    | Other         |      0 |     0.00 |
| 50–64.Other | 50–64    | Other         |      0 |     0.00 |
| 65+.Other   | 65+      | Other         |      0 |     0.00 |
| 25–39.Yes   | 25–39    | Yes           |      0 |     0.00 |
| 40–49.Yes   | 40–49    | Yes           |  26883 | 14878.36 |
| 50–64.Yes   | 50–64    | Yes           | 313525 | 59767.32 |
| 65+.Yes     | 65+      | Yes           | 487476 | 74155.95 |

``` r
#health coverage
cover_pct_strat = svyby(~col_2, by = ~age_cat+cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       |    col\_2 |        se |
| ---------------------- | :------- | :--------------- | --------: | --------: |
| 25–39.None             | 25–39    | None             | 0.0000000 | 0.0000000 |
| 40–49.None             | 40–49    | None             | 0.0459142 | 0.0468759 |
| 50–64.None             | 50–64    | None             | 0.0638401 | 0.0389689 |
| 65+.None               | 65+      | None             | 0.0000000 | 0.0000000 |
| 25–39.Private/Military | 25–39    | Private/Military | 0.0000000 | 0.0000000 |
| 40–49.Private/Military | 40–49    | Private/Military | 0.0250113 | 0.0152205 |
| 50–64.Private/Military | 50–64    | Private/Military | 0.0497173 | 0.0097501 |
| 65+.Private/Military   | 65+      | Private/Military | 0.0500577 | 0.0107514 |
| 25–39.Public           | 25–39    | Public           | 0.0000000 | 0.0000000 |
| 40–49.Public           | 40–49    | Public           | 0.0000000 | 0.0000000 |
| 50–64.Public           | 50–64    | Public           | 0.0527764 | 0.0265722 |
| 65+.Public             | 65+      | Public           | 0.0639513 | 0.0132489 |

``` r
#health coverage total
cover_tot_strat = svyby(~col_2, by = ~age_cat+cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       | col\_2 |        se |
| ---------------------- | :------- | :--------------- | -----: | --------: |
| 25–39.None             | 25–39    | None             |      0 |     0.000 |
| 40–49.None             | 40–49    | None             |   3523 |  3523.000 |
| 50–64.None             | 50–64    | None             |  14818 |  9649.805 |
| 65+.None               | 65+      | None             |      0 |     0.000 |
| 25–39.Private/Military | 25–39    | Private/Military |      0 |     0.000 |
| 40–49.Private/Military | 40–49    | Private/Military |  23360 | 14455.248 |
| 50–64.Private/Military | 50–64    | Private/Military | 274484 | 55201.688 |
| 65+.Private/Military   | 65+      | Private/Military | 257713 | 57216.931 |
| 25–39.Public           | 25–39    | Public           |      0 |     0.000 |
| 40–49.Public           | 40–49    | Public           |      0 |     0.000 |
| 50–64.Public           | 50–64    | Public           |  31297 | 15682.711 |
| 65+.Public             | 65+      | Public           | 255485 | 53131.205 |

``` r
#chronic conditions
lcond_chronic_pct_strat = svyby(~col_2, by = ~age_cat+lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat |    col\_2 |        se |
| --------- | :------- | :------------------ | --------: | --------: |
| 25–39.No  | 25–39    | No                  | 0.0000000 | 0.0000000 |
| 40–49.No  | 40–49    | No                  | 0.0000000 | 0.0000000 |
| 50–64.No  | 50–64    | No                  | 0.0000000 | 0.0000000 |
| 65+.No    | 65+      | No                  | 0.3375460 | 0.2140763 |
| 25–39.Yes | 25–39    | Yes                 | 0.0000000 | 0.0000000 |
| 40–49.Yes | 40–49    | Yes                 | 0.0000000 | 0.0000000 |
| 50–64.Yes | 50–64    | Yes                 | 0.0354157 | 0.0144129 |
| 65+.Yes   | 65+      | Yes                 | 0.0612846 | 0.0146360 |

``` r
#chronic conditions total
lcond_chronic_tot_strat = svyby(~col_2, by = ~age_cat+lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat | col\_2 |       se |
| --------- | :------- | :------------------ | -----: | -------: |
| 25–39.No  | 25–39    | No                  |      0 |     0.00 |
| 40–49.No  | 40–49    | No                  |      0 |     0.00 |
| 50–64.No  | 50–64    | No                  |      0 |     0.00 |
| 65+.No    | 65+      | No                  |  14396 | 10395.89 |
| 25–39.Yes | 25–39    | Yes                 |      0 |     0.00 |
| 40–49.Yes | 40–49    | Yes                 |      0 |     0.00 |
| 50–64.Yes | 50–64    | Yes                 |  62722 | 26219.03 |
| 65+.Yes   | 65+      | Yes                 | 196730 | 49109.01 |

``` r
#race
race_pct_strat = svyby(~col_2, by = ~age_cat+race_cat, svymean, na.rm = TRUE, design = des)
race_pct_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat |    col\_2 |        se |
| ----------- | :------- | :-------- | --------: | --------: |
| 25–39.AN/AI | 25–39    | AN/AI     | 0.0000000 | 0.0000000 |
| 40–49.AN/AI | 40–49    | AN/AI     | 0.0000000 | 0.0000000 |
| 50–64.AN/AI | 50–64    | AN/AI     | 0.2041201 | 0.1303459 |
| 65+.AN/AI   | 65+      | AN/AI     | 0.0517580 | 0.0384845 |
| 25–39.Asian | 25–39    | Asian     | 0.0000000 | 0.0000000 |
| 40–49.Asian | 40–49    | Asian     | 0.0000000 | 0.0000000 |
| 50–64.Asian | 50–64    | Asian     | 0.2031466 | 0.0872926 |
| 65+.Asian   | 65+      | Asian     | 0.0756814 | 0.0384574 |
| 25–39.Black | 25–39    | Black     | 0.0000000 | 0.0000000 |
| 40–49.Black | 40–49    | Black     | 0.1151084 | 0.0848042 |
| 50–64.Black | 50–64    | Black     | 0.0898619 | 0.0407149 |
| 65+.Black   | 65+      | Black     | 0.0993391 | 0.0304231 |
| 25–39.White | 25–39    | White     | 0.0000000 | 0.0000000 |
| 40–49.White | 40–49    | White     | 0.0139971 | 0.0102613 |
| 50–64.White | 50–64    | White     | 0.0392907 | 0.0086679 |
| 65+.White   | 65+      | White     | 0.0521228 | 0.0087939 |

``` r
#race total
race_tot_strat = svyby(~col_2, by = ~age_cat+race_cat, svytotal, na.rm = TRUE, design = des)
race_tot_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat | col\_2 |        se |
| ----------- | :------- | :-------- | -----: | --------: |
| 25–39.AN/AI | 25–39    | AN/AI     |      0 |     0.000 |
| 40–49.AN/AI | 40–49    | AN/AI     |      0 |     0.000 |
| 50–64.AN/AI | 50–64    | AN/AI     |  13525 |  9694.680 |
| 65+.AN/AI   | 65+      | AN/AI     |   7983 |  5424.527 |
| 25–39.Asian | 25–39    | Asian     |      0 |     0.000 |
| 40–49.Asian | 40–49    | Asian     |      0 |     0.000 |
| 50–64.Asian | 50–64    | Asian     |  35134 | 17823.673 |
| 65+.Asian   | 65+      | Asian     |  20109 | 10849.795 |
| 25–39.Black | 25–39    | Black     |      0 |     0.000 |
| 40–49.Black | 40–49    | Black     |  14872 | 11883.237 |
| 50–64.Black | 50–64    | Black     |  53615 | 24428.717 |
| 65+.Black   | 65+      | Black     |  61792 | 20634.843 |
| 25–39.White | 25–39    | White     |      0 |     0.000 |
| 40–49.White | 40–49    | White     |  12011 |  8952.899 |
| 50–64.White | 50–64    | White     | 218325 | 49294.568 |
| 65+.White   | 65+      | White     | 423314 | 74278.122 |

``` r
#ethnicity
eth_pct_strat = svyby(~col_2, by = ~age_cat+eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           |    col\_2 |        se |
| ------------------------ | :------- | :----------------- | --------: | --------: |
| 25–39.Hispanic           | 25–39    | Hispanic           | 0.0000000 | 0.0000000 |
| 40–49.Hispanic           | 40–49    | Hispanic           | 0.1109942 | 0.0908434 |
| 50–64.Hispanic           | 50–64    | Hispanic           | 0.0956981 | 0.0389279 |
| 65+.Hispanic             | 65+      | Hispanic           | 0.0807451 | 0.0266941 |
| 25–39.Non-Hispanic AN/AI | 25–39    | Non-Hispanic AN/AI | 0.0000000 | 0.0000000 |
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI | 0.0000000 | 0.0000000 |
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI | 0.1613402 | 0.1364376 |
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI | 0.0389476 | 0.0367564 |
| 25–39.Non-Hispanic Asian | 25–39    | Non-Hispanic Asian | 0.0000000 | 0.0000000 |
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian | 0.0000000 | 0.0000000 |
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian | 0.2235499 | 0.0946623 |
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian | 0.0795272 | 0.0403508 |
| 25–39.Non-Hispanic Black | 25–39    | Non-Hispanic Black | 0.0000000 | 0.0000000 |
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black | 0.0298937 | 0.0297829 |
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black | 0.0910202 | 0.0412068 |
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black | 0.1011068 | 0.0309140 |
| 25–39.Non-Hispanic White | 25–39    | Non-Hispanic White | 0.0000000 | 0.0000000 |
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White | 0.0131127 | 0.0113474 |
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White | 0.0344465 | 0.0087341 |
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White | 0.0505132 | 0.0090999 |

``` r
#ethnicity total
eth_tot_strat = svyby(~col_2, by = ~age_cat+eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           | col\_2 |        se |
| ------------------------ | :------- | :----------------- | -----: | --------: |
| 25–39.Hispanic           | 25–39    | Hispanic           |      0 |     0.000 |
| 40–49.Hispanic           | 40–49    | Hispanic           |  13497 | 11550.485 |
| 50–64.Hispanic           | 50–64    | Hispanic           |  46818 | 19881.400 |
| 65+.Hispanic             | 65+      | Hispanic           |  36496 | 12530.376 |
| 25–39.Non-Hispanic AN/AI | 25–39    | Non-Hispanic AN/AI |      0 |     0.000 |
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI |      0 |     0.000 |
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI |   9395 |  8770.970 |
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI |   5384 |  4761.375 |
| 25–39.Non-Hispanic Asian | 25–39    | Non-Hispanic Asian |      0 |     0.000 |
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian |      0 |     0.000 |
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian |  35134 | 17823.673 |
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian |  20109 | 10849.795 |
| 25–39.Non-Hispanic Black | 25–39    | Non-Hispanic Black |      0 |     0.000 |
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black |   3523 |  3523.000 |
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black |  53615 | 24428.717 |
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black |  61792 | 20634.843 |
| 25–39.Non-Hispanic White | 25–39    | Non-Hispanic White |      0 |     0.000 |
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White |   9863 |  8691.404 |
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White | 175637 | 45296.151 |
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White | 389417 | 72646.878 |
