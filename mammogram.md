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
# mammogram by education
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

``` r
# mammogram by finc
finc_pct = svyby(~mam_2, by = ~finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct  %>% knitr::kable()
```

|                              | finc\_cat                    |     mam\_2|         se|
|------------------------------|:-----------------------------|----------:|----------:|
| &lt;200%                     | &lt;200%                     |  0.2466641|  0.0148690|
| &gt;=200%, no further detail | &gt;=200%, no further detail |  0.3228125|  0.0467298|
| &gt;=500%                    | &gt;=500%                    |  0.3792062|  0.0276650|
| 200–299%                     | 200–299%                     |  0.2452990|  0.0293613|
| 300–399%                     | 300–399%                     |  0.2986085|  0.0335513|
| 400–499%                     | 400–499%                     |  0.3954329|  0.0279231|
| Unknown                      | Unknown                      |  0.1058113|  0.1009204|

``` r
finc_tot = svyby(~mam_2, by = ~finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot %>% knitr::kable()
```

|                              | finc\_cat                    |   mam\_2|         se|
|------------------------------|:-----------------------------|--------:|----------:|
| &lt;200%                     | &lt;200%                     |  1681121|  114447.99|
| &gt;=200%, no further detail | &gt;=200%, no further detail |   254425|   41905.66|
| &gt;=500%                    | &gt;=500%                    |  1584621|  145667.19|
| 200–299%                     | 200–299%                     |   696807|   90389.81|
| 300–399%                     | 300–399%                     |   539346|   72701.72|
| 400–499%                     | 400–499%                     |  1224889|  112609.63|
| Unknown                      | Unknown                      |     5994|    5994.00|

``` r
# mammogram by usual care
ausualp_pct = svyby(~mam_2, by = ~ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct %>% knitr::kable()
```

|       | ausualpl\_cat |     mam\_2|         se|
|-------|:--------------|----------:|----------:|
| No    | No            |  0.1324871|  0.0242305|
| Other | Other         |  0.0000000|  0.0000000|
| Yes   | Yes           |  0.3125982|  0.0106713|

``` r
ausualp_tot = svyby(~mam_2, by = ~ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot %>% knitr::kable()
```

|       | ausualpl\_cat |   mam\_2|         se|
|-------|:--------------|--------:|----------:|
| No    | No            |   200445|   37909.37|
| Other | Other         |        0|       0.00|
| Yes   | Yes           |  6036009|  246912.84|

``` r
# mammogram by health coverage
cover_pct = svyby(~mam_2, by = ~cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct %>% knitr::kable()
```

|                  | cover\_cat       |     mam\_2|         se|
|------------------|:-----------------|----------:|----------:|
| None             | None             |  0.1599616|  0.0323559|
| Private/Military | Private/Military |  0.3296367|  0.0139620|
| Public           | Public           |  0.2666126|  0.0171297|

``` r
cover_tot = svyby(~mam_2, by = ~cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot %>% knitr::kable()
```

|                  | cover\_cat       |   mam\_2|         se|
|------------------|:-----------------|--------:|----------:|
| None             | None             |   216060|   45028.34|
| Private/Military | Private/Military |  4362043|  214710.68|
| Public           | Public           |  1645117|  123461.69|

``` r
# mammogram by chronic conditions
lcond_chronic_pct = svyby(~mam_2, by = ~lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct %>% knitr::kable()
```

|     | lcond\_chronic\_cat |     mam\_2|         se|
|-----|:--------------------|----------:|----------:|
| No  | No                  |  0.3227098|  0.1272247|
| Yes | Yes                 |  0.2268148|  0.0154030|

``` r
lcond_chronic_tot = svyby(~mam_2, by = ~lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot %>% knitr::kable()
```

|     | lcond\_chronic\_cat |   mam\_2|         se|
|-----|:--------------------|--------:|----------:|
| No  | No                  |    44330|   21246.71|
| Yes | Yes                 |  1434243|  118030.84|

``` r
#mammogram by race
race_pct = svyby(~mam_2, by = ~race_cat, svymean, na.rm = TRUE, design = des)
race_pct %>% knitr::kable()
```

|       | race\_cat |     mam\_2|         se|
|-------|:----------|----------:|----------:|
| AN/AI | AN/AI     |  0.1850641|  0.0724294|
| Asian | Asian     |  0.3160724|  0.0452987|
| Black | Black     |  0.3768600|  0.0316087|
| White | White     |  0.2894254|  0.0110401|

``` r
race_tot = svyby(~mam_2, by = ~race_cat, svytotal, na.rm = TRUE, design = des)
race_tot %>% knitr::kable()
```

|       | race\_cat |   mam\_2|         se|
|-------|:----------|--------:|----------:|
| AN/AI | AN/AI     |    47322|   20171.97|
| Asian | Asian     |   380012|   68840.07|
| Black | Black     |   874328|   96259.80|
| White | White     |  4934792|  217264.18|

``` r
# mammogram by ethnicity
eth_pct = svyby(~mam_2, by = ~eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct %>% knitr::kable()
```

|                    | eth\_cat           |     mam\_2|         se|
|--------------------|:-------------------|----------:|----------:|
| Hispanic           | Hispanic           |  0.3987374|  0.0298913|
| Non-Hispanic AN/AI | Non-Hispanic AN/AI |  0.2048993|  0.0996913|
| Non-Hispanic Asian | Non-Hispanic Asian |  0.3204003|  0.0458688|
| Non-Hispanic Black | Non-Hispanic Black |  0.3728957|  0.0320404|
| Non-Hispanic White | Non-Hispanic White |  0.2720955|  0.0116544|

``` r
eth_tot = svyby(~mam_2, by = ~eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot %>% knitr::kable()
```

|                    | eth\_cat           |   mam\_2|         se|
|--------------------|:-------------------|--------:|----------:|
| Hispanic           | Hispanic           |   949318|   92055.11|
| Non-Hispanic AN/AI | Non-Hispanic AN/AI |    34654|   18680.84|
| Non-Hispanic Asian | Non-Hispanic Asian |   378931|   68831.58|
| Non-Hispanic Black | Non-Hispanic Black |   820815|   93199.85|
| Non-Hispanic White | Non-Hispanic White |  4052736|  205233.96|

Tables By Age Group

``` r
# mammogram by education
edu_pct_strat = svyby(~mam_2, by = ~age_cat+educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             |     mam\_2|         se|
|-----------------------------|:---------|:----------------------|----------:|----------:|
| 40–49.College graduate      | 40–49    | College graduate      |  0.3628165|  0.0478107|
| 50–64.College graduate      | 50–64    | College graduate      |  0.4368191|  0.0353693|
| 65+.College graduate        | 65+      | College graduate      |  0.3249096|  0.0360755|
| 40–49.High school           | 40–49    | High school           |  0.2553434|  0.0490819|
| 50–64.High school           | 50–64    | High school           |  0.2663692|  0.0334539|
| 65+.High school             | 65+      | High school           |  0.2156457|  0.0228904|
| 40–49.Less than high school | 40–49    | Less than high school |  0.3408288|  0.0647031|
| 50–64.Less than high school | 50–64    | Less than high school |  0.2907496|  0.0427511|
| 65+.Less than high school   | 65+      | Less than high school |  0.2797153|  0.0357203|
| 40–49.Some college          | 40–49    | Some college          |  0.3060292|  0.0453259|
| 50–64.Some college          | 50–64    | Some college          |  0.2958289|  0.0316095|
| 65+.Some college            | 65+      | Some college          |  0.2870106|  0.0277993|

``` r
edu_tot_strat = svyby(~mam_2, by = ~age_cat+educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot_strat %>% knitr::kable()
```

|                             | age\_cat | educ\_cat             |   mam\_2|         se|
|-----------------------------|:---------|:----------------------|--------:|----------:|
| 40–49.College graduate      | 40–49    | College graduate      |   437536|   65748.91|
| 50–64.College graduate      | 50–64    | College graduate      |  1062071|  112747.72|
| 65+.College graduate        | 65+      | College graduate      |   510280|   74104.67|
| 40–49.High school           | 40–49    | High school           |   218276|   46052.65|
| 50–64.High school           | 50–64    | High school           |   563580|   80530.10|
| 65+.High school             | 65+      | High school           |   631525|   73685.38|
| 40–49.Less than high school | 40–49    | Less than high school |   169381|   39157.20|
| 50–64.Less than high school | 50–64    | Less than high school |   336595|   61788.58|
| 65+.Less than high school   | 65+      | Less than high school |   507749|   75161.28|
| 40–49.Some college          | 40–49    | Some college          |   329856|   55526.38|
| 50–64.Some college          | 50–64    | Some college          |   831263|  107184.96|
| 65+.Some college            | 65+      | Some college          |   622427|   68225.66|

``` r
# mammogram by finc
finc_pct_strat = svyby(~mam_2, by = ~age_cat+finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct_strat  %>% knitr::kable()
```

|                                    | age\_cat | finc\_cat                    |     mam\_2|         se|
|------------------------------------|:---------|:-----------------------------|----------:|----------:|
| 40–49.&lt;200%                     | 40–49    | &lt;200%                     |  0.3110863|  0.0390506|
| 50–64.&lt;200%                     | 50–64    | &lt;200%                     |  0.2659748|  0.0257286|
| 65+.&lt;200%                       | 65+      | &lt;200%                     |  0.2059262|  0.0191127|
| 40–49.&gt;=200%, no further detail | 40–49    | &gt;=200%, no further detail |  0.4289634|  0.1753400|
| 50–64.&gt;=200%, no further detail | 50–64    | &gt;=200%, no further detail |  0.3433714|  0.0922681|
| 65+.&gt;=200%, no further detail   | 65+      | &gt;=200%, no further detail |  0.3070764|  0.0593150|
| 40–49.&gt;=500%                    | 40–49    | &gt;=500%                    |  0.3471008|  0.0541245|
| 50–64.&gt;=500%                    | 50–64    | &gt;=500%                    |  0.4140524|  0.0390973|
| 65+.&gt;=500%                      | 65+      | &gt;=500%                    |  0.3340848|  0.0521188|
| 40–49.200–299%                     | 40–49    | 200–299%                     |  0.1798488|  0.0543433|
| 50–64.200–299%                     | 50–64    | 200–299%                     |  0.2578120|  0.0558329|
| 65+.200–299%                       | 65+      | 200–299%                     |  0.2643767|  0.0377457|
| 40–49.300–399%                     | 40–49    | 300–399%                     |  0.3098936|  0.0667132|
| 50–64.300–399%                     | 50–64    | 300–399%                     |  0.2946467|  0.0661746|
| 65+.300–399%                       | 65+      | 300–399%                     |  0.2968388|  0.0485862|
| 40–49.400–499%                     | 40–49    | 400–499%                     |  0.4729520|  0.0909933|
| 50–64.400–499%                     | 50–64    | 400–499%                     |  0.4412923|  0.0529979|
| 65+.400–499%                       | 65+      | 400–499%                     |  0.3316844|  0.0389359|
| 40–49.Unknown                      | 40–49    | Unknown                      |  0.0000000|  0.0000000|
| 50–64.Unknown                      | 50–64    | Unknown                      |  0.0000000|  0.0000000|
| 65+.Unknown                        | 65+      | Unknown                      |  0.1173017|  0.1114487|

``` r
finc_tot_strat = svyby(~mam_2, by = ~age_cat+finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot_strat %>% knitr::kable()
```

|                                    | age\_cat | finc\_cat                    |  mam\_2|          se|
|------------------------------------|:---------|:-----------------------------|-------:|-----------:|
| 40–49.&lt;200%                     | 40–49    | &lt;200%                     |  352188|   50676.763|
| 50–64.&lt;200%                     | 50–64    | &lt;200%                     |  702455|   77193.829|
| 65+.&lt;200%                       | 65+      | &lt;200%                     |  626478|   63637.829|
| 40–49.&gt;=200%, no further detail | 40–49    | &gt;=200%, no further detail |   13134|    7731.931|
| 50–64.&gt;=200%, no further detail | 50–64    | &gt;=200%, no further detail |   82028|   25300.291|
| 65+.&gt;=200%, no further detail   | 65+      | &gt;=200%, no further detail |  159263|   32502.689|
| 40–49.&gt;=500%                    | 40–49    | &gt;=500%                    |  319626|   59706.408|
| 50–64.&gt;=500%                    | 50–64    | &gt;=500%                    |  914221|  116437.289|
| 65+.&gt;=500%                      | 65+      | &gt;=500%                    |  350774|   63900.992|
| 40–49.200–299%                     | 40–49    | 200–299%                     |  100961|   31224.810|
| 50–64.200–299%                     | 50–64    | 200–299%                     |  264775|   60399.999|
| 65+.200–299%                       | 65+      | 200–299%                     |  331071|   55917.687|
| 40–49.300–399%                     | 40–49    | 300–399%                     |  111095|   26321.583|
| 50–64.300–399%                     | 50–64    | 300–399%                     |  199434|   50060.376|
| 65+.300–399%                       | 65+      | 300–399%                     |  228817|   49987.009|
| 40–49.400–499%                     | 40–49    | 400–499%                     |  187341|   48987.596|
| 50–64.400–499%                     | 50–64    | 400–499%                     |  569730|   81080.573|
| 65+.400–499%                       | 65+      | 400–499%                     |  467818|   69514.070|
| 40–49.Unknown                      | 40–49    | Unknown                      |       0|       0.000|
| 50–64.Unknown                      | 50–64    | Unknown                      |       0|       0.000|
| 65+.Unknown                        | 65+      | Unknown                      |    5994|    5994.000|

``` r
# mammogram by usual care
ausualp_pct_strat = svyby(~mam_2, by = ~age_cat+ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat |     mam\_2|         se|
|-------------|:---------|:--------------|----------:|----------:|
| 40–49.No    | 40–49    | No            |  0.1409714|  0.0433310|
| 50–64.No    | 50–64    | No            |  0.1129043|  0.0343025|
| 65+.No      | 65+      | No            |  0.1682582|  0.0676474|
| 40–49.Other | 40–49    | Other         |  0.0000000|  0.0000000|
| 50–64.Other | 50–64    | Other         |  0.0000000|  0.0000000|
| 65+.Other   | 65+      | Other         |  0.0000000|  0.0000000|
| 40–49.Yes   | 40–49    | Yes           |  0.3454294|  0.0297565|
| 50–64.Yes   | 50–64    | Yes           |  0.3466129|  0.0187589|
| 65+.Yes     | 65+      | Yes           |  0.2682009|  0.0142926|

``` r
ausualp_tot_strat = svyby(~mam_2, by = ~age_cat+ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot_strat %>% knitr::kable()
```

|             | age\_cat | ausualpl\_cat |   mam\_2|         se|
|-------------|:---------|:--------------|--------:|----------:|
| 40–49.No    | 40–49    | No            |    68628|   22194.04|
| 50–64.No    | 50–64    | No            |    83292|   24348.83|
| 65+.No      | 65+      | No            |    48525|   21088.45|
| 40–49.Other | 40–49    | Other         |        0|       0.00|
| 50–64.Other | 50–64    | Other         |        0|       0.00|
| 65+.Other   | 65+      | Other         |        0|       0.00|
| 40–49.Yes   | 40–49    | Yes           |  1092083|  105813.38|
| 50–64.Yes   | 50–64    | Yes           |  2710217|  182462.00|
| 65+.Yes     | 65+      | Yes           |  2233709|  139039.63|

``` r
# mammogram by health coverage
cover_pct_strat = svyby(~mam_2, by = ~age_cat+cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       |     mam\_2|         se|
|------------------------|:---------|:-----------------|----------:|----------:|
| 40–49.None             | 40–49    | None             |  0.1959259|  0.0542023|
| 50–64.None             | 50–64    | None             |  0.1200942|  0.0413150|
| 65+.None               | 65+      | None             |  0.3796540|  0.1828650|
| 40–49.Private/Military | 40–49    | Private/Military |  0.3407561|  0.0323471|
| 50–64.Private/Military | 50–64    | Private/Military |  0.3580790|  0.0219703|
| 65+.Private/Military   | 65+      | Private/Military |  0.2782850|  0.0206320|
| 40–49.Public           | 40–49    | Public           |  0.3221841|  0.0597471|
| 50–64.Public           | 50–64    | Public           |  0.2962089|  0.0437991|
| 65+.Public             | 65+      | Public           |  0.2513543|  0.0203941|

``` r
cover_tot_strat = svyby(~mam_2, by = ~age_cat+cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot_strat %>% knitr::kable()
```

|                        | age\_cat | cover\_cat       |   mam\_2|         se|
|------------------------|:---------|:-----------------|--------:|----------:|
| 40–49.None             | 40–49    | None             |   104942|   28758.01|
| 50–64.None             | 50–64    | None             |    91764|   32659.78|
| 65+.None               | 65+      | None             |    19354|   12639.78|
| 40–49.Private/Military | 40–49    | Private/Military |   874479|   94469.97|
| 50–64.Private/Military | 50–64    | Private/Military |  2329984|  168166.39|
| 65+.Private/Military   | 65+      | Private/Military |  1157580|  105357.32|
| 40–49.Public           | 40–49    | Public           |   173875|   38282.26|
| 50–64.Public           | 50–64    | Public           |   369318|   63476.88|
| 65+.Public             | 65+      | Public           |  1101924|  103095.18|

``` r
# mammogram by chronic conditions
lcond_chronic_pct_strat = svyby(~mam_2, by = ~age_cat+lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat |     mam\_2|         se|
|-----------|:---------|:--------------------|----------:|----------:|
| 40–49.No  | 40–49    | No                  |  0.3736354|  0.2892106|
| 50–64.No  | 50–64    | No                  |  0.6058108|  0.2703505|
| 65+.No    | 65+      | No                  |  0.2284719|  0.1158288|
| 40–49.Yes | 40–49    | Yes                 |  0.3141957|  0.0615773|
| 50–64.Yes | 50–64    | Yes                 |  0.2564085|  0.0298696|
| 65+.Yes   | 65+      | Yes                 |  0.1994143|  0.0195057|

``` r
lcond_chronic_tot_strat = svyby(~mam_2, by = ~age_cat+lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot_strat %>% knitr::kable()
```

|           | age\_cat | lcond\_chronic\_cat |  mam\_2|        se|
|-----------|:---------|:--------------------|-------:|---------:|
| 40–49.No  | 40–49    | No                  |    6811|   6811.00|
| 50–64.No  | 50–64    | No                  |   16535|  16535.00|
| 65+.No    | 65+      | No                  |   20984|  11472.87|
| 40–49.Yes | 40–49    | Yes                 |  152192|  35247.06|
| 50–64.Yes | 50–64    | Yes                 |  529361|  71274.65|
| 65+.Yes   | 65+      | Yes                 |  752690|  87192.97|

``` r
# mammogram by race
race_pct_strat = svyby(~mam_2, by = ~age_cat+race_cat, svymean, na.rm = TRUE, design = des)
race_pct_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat |     mam\_2|         se|
|-------------|:---------|:----------|----------:|----------:|
| 40–49.AN/AI | 40–49    | AN/AI     |  0.1064797|  0.1033082|
| 50–64.AN/AI | 50–64    | AN/AI     |  0.1820756|  0.0882584|
| 65+.AN/AI   | 65+      | AN/AI     |  0.2507746|  0.1812324|
| 40–49.Asian | 40–49    | Asian     |  0.3662753|  0.0893177|
| 50–64.Asian | 50–64    | Asian     |  0.3539920|  0.0786165|
| 65+.Asian   | 65+      | Asian     |  0.2158222|  0.0611581|
| 40–49.Black | 40–49    | Black     |  0.3995041|  0.0680587|
| 50–64.Black | 50–64    | Black     |  0.4135665|  0.0471393|
| 65+.Black   | 65+      | Black     |  0.3147076|  0.0406966|
| 40–49.White | 40–49    | White     |  0.3000826|  0.0304385|
| 50–64.White | 50–64    | White     |  0.3143985|  0.0205401|
| 65+.White   | 65+      | White     |  0.2620824|  0.0152894|

``` r
race_tot_strat = svyby(~mam_2, by = ~age_cat+race_cat, svytotal, na.rm = TRUE, design = des)
race_tot_strat %>% knitr::kable()
```

|             | age\_cat | race\_cat |   mam\_2|          se|
|-------------|:---------|:----------|--------:|-----------:|
| 40–49.AN/AI | 40–49    | AN/AI     |     6044|    5832.964|
| 50–64.AN/AI | 50–64    | AN/AI     |    22825|   11121.083|
| 65+.AN/AI   | 65+      | AN/AI     |    18453|   16410.649|
| 40–49.Asian | 40–49    | Asian     |   130909|   38409.554|
| 50–64.Asian | 50–64    | Asian     |   171033|   49568.800|
| 65+.Asian   | 65+      | Asian     |    78070|   23884.416|
| 40–49.Black | 40–49    | Black     |   206397|   42985.884|
| 50–64.Black | 50–64    | Black     |   419959|   65546.706|
| 65+.Black   | 65+      | Black     |   247972|   42291.679|
| 40–49.White | 40–49    | White     |   817361|   90029.174|
| 50–64.White | 50–64    | White     |  2179692|  173365.512|
| 65+.White   | 65+      | White     |  1937739|  132249.394|

``` r
# mammogram by ethnicity
eth_pct_strat = svyby(~mam_2, by = ~age_cat+eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           |     mam\_2|         se|
|--------------------------|:---------|:-------------------|----------:|----------:|
| 40–49.Hispanic           | 40–49    | Hispanic           |  0.3614476|  0.0494467|
| 50–64.Hispanic           | 50–64    | Hispanic           |  0.4632209|  0.0557511|
| 65+.Hispanic             | 65+      | Hispanic           |  0.3555218|  0.0427762|
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI |  0.0141961|  0.0156116|
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI |  0.1715973|  0.1068567|
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI |  0.3033935|  0.2083078|
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian |  0.3744387|  0.0915374|
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian |  0.3560777|  0.0790576|
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian |  0.2195271|  0.0622091|
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black |  0.4116241|  0.0746826|
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black |  0.4052808|  0.0484458|
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black |  0.3084453|  0.0393852|
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White |  0.2753570|  0.0347804|
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White |  0.2934804|  0.0217902|
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White |  0.2514847|  0.0159211|

``` r
eth_tot_strat = svyby(~mam_2, by = ~age_cat+eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot_strat %>% knitr::kable()
```

|                          | age\_cat | eth\_cat           |   mam\_2|          se|
|--------------------------|:---------|:-------------------|--------:|-----------:|
| 40–49.Hispanic           | 40–49    | Hispanic           |   252058|   42549.974|
| 50–64.Hispanic           | 50–64    | Hispanic           |   424755|   68281.878|
| 65+.Hispanic             | 65+      | Hispanic           |   272505|   41259.328|
| 40–49.Non-Hispanic AN/AI | 40–49    | Non-Hispanic AN/AI |      215|     215.000|
| 50–64.Non-Hispanic AN/AI | 50–64    | Non-Hispanic AN/AI |    15986|    9986.047|
| 65+.Non-Hispanic AN/AI   | 65+      | Non-Hispanic AN/AI |    18453|   16410.649|
| 40–49.Non-Hispanic Asian | 40–49    | Non-Hispanic Asian |   129828|   38394.339|
| 50–64.Non-Hispanic Asian | 50–64    | Non-Hispanic Asian |   171033|   49568.800|
| 65+.Non-Hispanic Asian   | 65+      | Non-Hispanic Asian |    78070|   23884.416|
| 40–49.Non-Hispanic Black | 40–49    | Non-Hispanic Black |   193204|   41870.391|
| 50–64.Non-Hispanic Black | 50–64    | Non-Hispanic Black |   391064|   63879.518|
| 65+.Non-Hispanic Black   | 65+      | Non-Hispanic Black |   236547|   39354.178|
| 40–49.Non-Hispanic White | 40–49    | Non-Hispanic White |   585406|   79913.930|
| 50–64.Non-Hispanic White | 50–64    | Non-Hispanic White |  1790671|  159762.452|
| 65+.Non-Hispanic White   | 65+      | Non-Hispanic White |  1676659|  127528.890|
