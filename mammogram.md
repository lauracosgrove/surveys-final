Mammogram
================

Read in the data.

``` r
cancer = read_csv("./data/cancerxx.csv") %>%
  select(HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, REGION, MAMHAD, MAM6YR, 
         RMAM1_MT, RMAM1YR, RMAM1N, RMAM1T, RMAM2, RMAM3A, RMAM3B, MAMPAY, 
         MAMREAS, MDRECMAM, MAMDNBR, MAMABN1, MFOLLOW0, MFOLLO01, MFOLLO02, 
         MFOLLO03, MFOLLO04, MFOLLO05, MNOTFOL1, MAMMODE, MAMCAN1)

adult = read_csv("./data/samadult.csv") %>%
  select(HHX, FMX, FPX, AUSUALPL, AHCPLROU, AHCPLKND, FLA1AR)

family = read_csv("./data/familyxx.csv") %>%
  select(HHX, FMX, RAT_CAT4, RAT_CAT5)

person = read_csv("./data/personsx.csv") %>%
  select(HHX, FMX, FPX, AGE_P, EDUC1, SEX, NOTCOV, COVER65, COVER65O, LA1AR,
         LCONDRT, LACHRONR, HISCODI3, RACRECI3, COVER)

mam_dat = cancer %>%
  left_join(adult, by = c("HHX", "FMX", "FPX")) %>%
  left_join(person, by = c("HHX", "FMX", "FPX")) %>%
  left_join(family, by = c("HHX", "FMX")) 
```

Data Manipulation

``` r
# outcome is having a mammogram in the last 2 years:RMAM2 = 1,2
mam_dat = mam_dat %>%
  mutate(mam_2 = if_else(RMAM2 <= 2, 1, 0))

# create the age category
mam_dat = mam_dat %>% 
  mutate(age_cat = case_when(AGE_P >= 25 & AGE_P < 40 ~ "25–39",
                             AGE_P >= 40 & AGE_P < 50 ~ "40–49",
                             AGE_P >= 50 & AGE_P < 65 ~ "50–64",
                             AGE_P >= 65 ~ "65+"))
# create educ category
mam_dat = mam_dat %>% 
  mutate(educ_cat = case_when(EDUC1 < 13 ~ "Less than high school",
                              EDUC1 >= 13 & EDUC1 < 15 ~ "High school",
                              EDUC1 >= 15 & EDUC1 < 18 ~ "Some college",
                              EDUC1 >= 18 & EDUC1 <= 21 ~ "College graduate"))

# create financial category
mam_dat = mam_dat %>% 
  mutate(finc_cat = case_when(RAT_CAT5 <= 7 |  RAT_CAT5 %in% c(15, 16) ~ "<200%",
                              RAT_CAT5 %in% c(8, 9) ~ "200–299%", 
                              RAT_CAT5 %in% c(10, 11) ~ "300–399%",
                              RAT_CAT5 >= 18 & EDUC1 <= 21 ~ "400–499%",
                              RAT_CAT5 == 14  ~">=500%",
                              RAT_CAT5 == 17  ~">=200%, no further detail",
                              RAT_CAT5 %in% c(96, 99) ~ "Unknown"))

# create as usual category
mam_dat = mam_dat %>% 
  mutate(ausualpl_cat  = case_when(AUSUALPL == 2 ~ "No",
                                   AUSUALPL %in% c(1, 3) ~ "Yes",
                                   AUSUALPL %in% c(7, 8, 9) ~ "Other"))
# coverage status
mam_dat = mam_dat %>% 
  mutate(cover_cat  = case_when(NOTCOV == 1 | COVER == 4 | COVER65 == 6 ~ "None",
                                COVER == 2 | COVER65 %in% 2:4 ~ "Public",
                                COVER %in% c(1, 3) | COVER65 %in% c(1, 5) ~ "Private/Military"))

# disability
mam_dat = mam_dat %>% 
  mutate(lcond_chronic_cat = if_else(LCONDRT == 1, "Yes", "No"))

# race
mam_dat = mam_dat %>% 
  mutate(race_cat = case_when(RACRECI3 == 1 ~ "White",
                              RACRECI3 == 2 ~ "Black",
                              RACRECI3 == 3 ~ "Asian",
                              RACRECI3 == 4 ~ "AN/AI"),
         eth_cat = case_when(HISCODI3 == 1 ~ "Hispanic",
                             HISCODI3 == 2 ~ "Non-Hispanic White",
                             HISCODI3 == 3 ~ "Non-Hispanic Black",
                             HISCODI3 == 4 ~ "Non-Hispanic Asian",
                             HISCODI3 == 5 ~ "Non-Hispanic AN/AI"))
```

Survey Design

``` r
mam_dat = mam_dat %>%
  filter(SEX == 2) %>%
  filter(AGE_P >= 40) 

des = svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, nest = TRUE, data = mam_dat)
```

Tables

``` r
# percent of women who have had mammogram in the last two years
# unsure-can't filter before the design because then it gives me an error about only one PSU in strata 73
options(survey.lonely.psu = "certainty")
age_pct = svyby(~mam_2, by = ~age_cat, svymean, na.rm = TRUE, design = des)
age_pct %>% knitr::kable()
```

|       | age\_cat |     mam\_2|         se|
|-------|:---------|----------:|----------:|
| 40–49 | 40–49    |  0.3176038|  0.0266974|
| 50–64 | 50–64    |  0.3264640|  0.0174216|
| 65+   | 65+      |  0.2648560|  0.0140081|

``` r
age_tot = svyby(~mam_2, by = ~age_cat, svytotal, na.rm = TRUE, design = des)
age_tot  %>% knitr::kable()
```

|       | age\_cat |   mam\_2|        se|
|-------|:---------|--------:|---------:|
| 40–49 | 40–49    |  1160711|  107452.8|
| 50–64 | 50–64    |  2793509|  183442.9|
| 65+   | 65+      |  2282234|  140819.0|

``` r
# education percent
edu_pct = svyby(~mam_2, by = ~educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct %>% knitr::kable()
```

|                       | educ\_cat             |     mam\_2|         se|
|-----------------------|:----------------------|----------:|----------:|
| College graduate      | College graduate      |  0.3859343|  0.0231881|
| High school           | High school           |  0.2395907|  0.0162449|
| Less than high school | Less than high school |  0.2921496|  0.0247296|
| Some college          | Some college          |  0.2944866|  0.0190413|

``` r
edu_tot = svyby(~mam_2, by = ~educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot %>% knitr::kable()
```

|                       | educ\_cat             |   mam\_2|        se|
|-----------------------|:----------------------|--------:|---------:|
| College graduate      | College graduate      |  2009887|  148288.3|
| High school           | High school           |  1413381|  105605.7|
| Less than high school | Less than high school |  1013725|  105001.2|
| Some college          | Some college          |  1783546|  125719.6|
