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
  mutate(domain = if_else(SEX == 2 & AGE_P >= 40, 1, 0))


des = svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, nest = TRUE, data = mam_dat)
```

Tables

``` r
# percent of women who have had mammogram in the last two years
# unsure-can't filter before the design because then it gives me an error about only one PSU in strata 73
# options(survey.lonely.psu = "certainty")
age_pct = svyby(~mam_2, by = ~domain+age_cat, svymean, na.rm = TRUE, design = des)
age_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat |     mam\_2|         se|
|:---------|----------:|----------:|
| 40–49    |  0.3176038|  0.0266974|
| 50–64    |  0.3264640|  0.0174223|
| 65+      |  0.2648560|  0.0140089|

``` r
age_tot = svyby(~mam_2, by = ~domain+age_cat, svytotal, na.rm = TRUE, design = des)
age_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat |   mam\_2|        se|
|:---------|--------:|---------:|
| 40–49    |  1160711|  107452.8|
| 50–64    |  2793509|  183442.9|
| 65+      |  2282234|  140867.4|

``` r
# mammogram by education
edu_pct = svyby(~mam_2, by = ~domain+educ_cat, svymean, na.rm = TRUE, design = des)
edu_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| educ\_cat             |     mam\_2|         se|
|:----------------------|----------:|----------:|
| College graduate      |  0.3859343|  0.0231881|
| High school           |  0.2395907|  0.0162470|
| Less than high school |  0.2921496|  0.0247320|
| Some college          |  0.2944866|  0.0190413|

``` r
edu_tot = svyby(~mam_2, by = ~domain+educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| educ\_cat             |   mam\_2|        se|
|:----------------------|--------:|---------:|
| College graduate      |  2009887|  148288.3|
| High school           |  1413381|  105670.2|
| Less than high school |  1013725|  105001.2|
| Some college          |  1783546|  125719.6|

``` r
# mammogram by finc
finc_pct = svyby(~mam_2, by = ~domain+finc_cat, svymean, na.rm = TRUE, design = des)
finc_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| finc\_cat                    |     mam\_2|         se|
|:-----------------------------|----------:|----------:|
| &lt;200%                     |  0.2466641|  0.0148705|
| &gt;=200%, no further detail |  0.3228125|  0.0467298|
| &gt;=500%                    |  0.3792062|  0.0276675|
| 200–299%                     |  0.2452990|  0.0293613|
| 300–399%                     |  0.2986085|  0.0335513|
| 400–499%                     |  0.3954329|  0.0279231|
| Unknown                      |  0.1058113|  0.1009204|

``` r
finc_tot = svyby(~mam_2, by = ~domain+finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| finc\_cat                    |   mam\_2|         se|
|:-----------------------------|--------:|----------:|
| &lt;200%                     |  1681121|  114507.49|
| &gt;=200%, no further detail |   254425|   41905.66|
| &gt;=500%                    |  1584621|  145667.19|
| 200–299%                     |   696807|   90389.81|
| 300–399%                     |   539346|   72701.72|
| 400–499%                     |  1224889|  112609.63|
| Unknown                      |     5994|    5994.00|

``` r
# mammogram by usual care
ausualp_pct = svyby(~mam_2, by = ~domain+ausualpl_cat, svymean, na.rm = TRUE, design = des)
ausualp_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| ausualpl\_cat |     mam\_2|         se|
|:--------------|----------:|----------:|
| No            |  0.1324871|  0.0242305|
| Other         |  0.0000000|  0.0000000|
| Yes           |  0.3125982|  0.0106714|

``` r
ausualp_tot = svyby(~mam_2, by = ~domain+ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| ausualpl\_cat |   mam\_2|         se|
|:--------------|--------:|----------:|
| No            |   200445|   37909.37|
| Other         |        0|       0.00|
| Yes           |  6036009|  246940.43|

``` r
# mammogram by health coverage
cover_pct = svyby(~mam_2, by = ~domain+cover_cat, svymean, na.rm = TRUE, design = des)
cover_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| cover\_cat       |     mam\_2|         se|
|:-----------------|----------:|----------:|
| None             |  0.1599616|  0.0323559|
| Private/Military |  0.3296367|  0.0139620|
| Public           |  0.2666126|  0.0171297|

``` r
cover_tot = svyby(~mam_2, by = ~domain+cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| cover\_cat       |   mam\_2|         se|
|:-----------------|--------:|----------:|
| None             |   216060|   45028.34|
| Private/Military |  4362043|  214710.68|
| Public           |  1645117|  123516.85|

``` r
# mammogram by chronic conditions
lcond_chronic_pct = svyby(~mam_2, by = ~domain+lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
lcond_chronic_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| lcond\_chronic\_cat |     mam\_2|         se|
|:--------------------|----------:|----------:|
| No                  |  0.3227098|  0.1272247|
| Yes                 |  0.2268148|  0.0154096|

``` r
lcond_chronic_tot = svyby(~mam_2, by = ~domain+lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| lcond\_chronic\_cat |   mam\_2|         se|
|:--------------------|--------:|----------:|
| No                  |    44330|   21246.71|
| Yes                 |  1434243|  118088.54|

``` r
#mammogram by race
race_pct = svyby(~mam_2, by = ~domain+race_cat, svymean, na.rm = TRUE, design = des)
race_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| race\_cat |     mam\_2|         se|
|:----------|----------:|----------:|
| AN/AI     |  0.1850641|  0.0724294|
| Asian     |  0.3160724|  0.0453117|
| Black     |  0.3768600|  0.0316087|
| White     |  0.2894254|  0.0110402|

``` r
race_tot = svyby(~mam_2, by = ~domain+race_cat, svytotal, na.rm = TRUE, design = des)
race_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| race\_cat |   mam\_2|         se|
|:----------|--------:|----------:|
| AN/AI     |    47322|   20171.97|
| Asian     |   380012|   68840.07|
| Black     |   874328|   96259.80|
| White     |  4934792|  217295.53|

``` r
# mammogram by ethnicity
eth_pct = svyby(~mam_2, by = ~domain+eth_cat, svymean, na.rm = TRUE, design = des)
eth_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| eth\_cat           |     mam\_2|         se|
|:-------------------|----------:|----------:|
| Hispanic           |  0.3987374|  0.0298913|
| Non-Hispanic AN/AI |  0.2048993|  0.0996913|
| Non-Hispanic Asian |  0.3204003|  0.0458825|
| Non-Hispanic Black |  0.3728957|  0.0320404|
| Non-Hispanic White |  0.2720955|  0.0116547|

``` r
eth_tot = svyby(~mam_2, by = ~domain+eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| eth\_cat           |   mam\_2|         se|
|:-------------------|--------:|----------:|
| Hispanic           |   949318|   92055.11|
| Non-Hispanic AN/AI |    34654|   18680.84|
| Non-Hispanic Asian |   378931|   68831.58|
| Non-Hispanic Black |   820815|   93199.85|
| Non-Hispanic White |  4052736|  205267.14|

Tables By Age Group

``` r
# mammogram by education
edu_pct_strat = svyby(~mam_2, by = ~domain+age_cat+educ_cat, svymean, na.rm = TRUE, design = des)
edu_tab = edu_pct_strat %>% filter(domain == 1) %>% select(-domain, -se) 
edu_tab %>% knitr::kable()
```

| age\_cat | educ\_cat             |     mam\_2|
|:---------|:----------------------|----------:|
| 40–49    | College graduate      |  0.3628165|
| 50–64    | College graduate      |  0.4368191|
| 65+      | College graduate      |  0.3249096|
| 40–49    | High school           |  0.2553434|
| 50–64    | High school           |  0.2663692|
| 65+      | High school           |  0.2156457|
| 40–49    | Less than high school |  0.3408288|
| 50–64    | Less than high school |  0.2907496|
| 65+      | Less than high school |  0.2797153|
| 40–49    | Some college          |  0.3060292|
| 50–64    | Some college          |  0.2958289|
| 65+      | Some college          |  0.2870106|

``` r
edu_tot_strat = svyby(~mam_2, by = ~domain+age_cat+educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot_strat %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat | educ\_cat             |   mam\_2|         se|
|:---------|:----------------------|--------:|----------:|
| 40–49    | College graduate      |   437536|   65748.91|
| 50–64    | College graduate      |  1062071|  112747.72|
| 65+      | College graduate      |   510280|   74104.67|
| 40–49    | High school           |   218276|   46052.65|
| 50–64    | High school           |   563580|   80530.10|
| 65+      | High school           |   631525|   73777.77|
| 40–49    | Less than high school |   169381|   39157.20|
| 50–64    | Less than high school |   336595|   61788.58|
| 65+      | Less than high school |   507749|   75161.28|
| 40–49    | Some college          |   329856|   55526.38|
| 50–64    | Some college          |   831263|  107184.96|
| 65+      | Some college          |   622427|   68225.66|

``` r
# mammogram by finc
finc_pct_strat = svyby(~mam_2, by = ~domain+age_cat+finc_cat, svymean, na.rm = TRUE, design = des)
finc_tab = finc_pct_strat %>% filter(domain == 1) %>% select(-domain, -se) 
finc_tab %>% knitr::kable()
```

| age\_cat | finc\_cat                    |     mam\_2|
|:---------|:-----------------------------|----------:|
| 40–49    | &lt;200%                     |  0.3110863|
| 50–64    | &lt;200%                     |  0.2659748|
| 65+      | &lt;200%                     |  0.2059262|
| 40–49    | &gt;=200%, no further detail |  0.4289634|
| 50–64    | &gt;=200%, no further detail |  0.3433714|
| 65+      | &gt;=200%, no further detail |  0.3070764|
| 40–49    | &gt;=500%                    |  0.3471008|
| 50–64    | &gt;=500%                    |  0.4140524|
| 65+      | &gt;=500%                    |  0.3340848|
| 40–49    | 200–299%                     |  0.1798488|
| 50–64    | 200–299%                     |  0.2578120|
| 65+      | 200–299%                     |  0.2643767|
| 40–49    | 300–399%                     |  0.3098936|
| 50–64    | 300–399%                     |  0.2946467|
| 65+      | 300–399%                     |  0.2968388|
| 40–49    | 400–499%                     |  0.4729520|
| 50–64    | 400–499%                     |  0.4412923|
| 65+      | 400–499%                     |  0.3316844|
| 40–49    | Unknown                      |  0.0000000|
| 50–64    | Unknown                      |  0.0000000|
| 65+      | Unknown                      |  0.1173017|

``` r
finc_tot_strat = svyby(~mam_2, by = ~domain+age_cat+finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot_strat %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat | finc\_cat                    |  mam\_2|          se|
|:---------|:-----------------------------|-------:|-----------:|
| 40–49    | &lt;200%                     |  352188|   50676.763|
| 50–64    | &lt;200%                     |  702455|   77193.829|
| 65+      | &lt;200%                     |  626478|   63744.778|
| 40–49    | &gt;=200%, no further detail |   13134|    7731.931|
| 50–64    | &gt;=200%, no further detail |   82028|   25300.291|
| 65+      | &gt;=200%, no further detail |  159263|   32502.689|
| 40–49    | &gt;=500%                    |  319626|   59706.408|
| 50–64    | &gt;=500%                    |  914221|  116437.289|
| 65+      | &gt;=500%                    |  350774|   63900.992|
| 40–49    | 200–299%                     |  100961|   31224.810|
| 50–64    | 200–299%                     |  264775|   60399.999|
| 65+      | 200–299%                     |  331071|   55917.687|
| 40–49    | 300–399%                     |  111095|   26321.583|
| 50–64    | 300–399%                     |  199434|   50060.376|
| 65+      | 300–399%                     |  228817|   49987.009|
| 40–49    | 400–499%                     |  187341|   48987.596|
| 50–64    | 400–499%                     |  569730|   81080.573|
| 65+      | 400–499%                     |  467818|   69514.070|
| 40–49    | Unknown                      |       0|       0.000|
| 50–64    | Unknown                      |       0|       0.000|
| 65+      | Unknown                      |    5994|    5994.000|

``` r
# mammogram by usual care
ausualp_pct_strat = svyby(~mam_2, by = ~domain+age_cat+ausualpl_cat, svymean, na.rm = TRUE, design = des)
usual_tab = ausualp_pct_strat %>% filter(domain == 1) %>% select(-domain, -se) 
usual_tab %>% knitr::kable()
```

| age\_cat | ausualpl\_cat |     mam\_2|
|:---------|:--------------|----------:|
| 40–49    | No            |  0.1409714|
| 50–64    | No            |  0.1129043|
| 65+      | No            |  0.1682582|
| 40–49    | Other         |  0.0000000|
| 50–64    | Other         |  0.0000000|
| 65+      | Other         |  0.0000000|
| 40–49    | Yes           |  0.3454294|
| 50–64    | Yes           |  0.3466129|
| 65+      | Yes           |  0.2682009|

``` r
ausualp_tot_strat = svyby(~mam_2, by = ~domain+age_cat+ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot_strat %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat | ausualpl\_cat |   mam\_2|         se|
|:---------|:--------------|--------:|----------:|
| 40–49    | No            |    68628|   22194.04|
| 50–64    | No            |    83292|   24348.83|
| 65+      | No            |    48525|   21088.45|
| 40–49    | Other         |        0|       0.00|
| 50–64    | Other         |        0|       0.00|
| 65+      | Other         |        0|       0.00|
| 40–49    | Yes           |  1092083|  105813.38|
| 50–64    | Yes           |  2710217|  182462.00|
| 65+      | Yes           |  2233709|  139088.61|

``` r
# mammogram by health coverage
cover_pct_strat = svyby(~mam_2, by = ~domain+age_cat+cover_cat, svymean, na.rm = TRUE, design = des)
ins_tab = cover_pct_strat %>% filter(domain == 1) %>% select(-domain, -se) 
ins_tab %>% knitr::kable()
```

| age\_cat | cover\_cat       |     mam\_2|
|:---------|:-----------------|----------:|
| 40–49    | None             |  0.1959259|
| 50–64    | None             |  0.1200942|
| 65+      | None             |  0.3796540|
| 40–49    | Private/Military |  0.3407561|
| 50–64    | Private/Military |  0.3580790|
| 65+      | Private/Military |  0.2782850|
| 40–49    | Public           |  0.3221841|
| 50–64    | Public           |  0.2962089|
| 65+      | Public           |  0.2513543|

``` r
cover_tot_strat = svyby(~mam_2, by = ~domain+age_cat+cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot_strat %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat | cover\_cat       |   mam\_2|         se|
|:---------|:-----------------|--------:|----------:|
| 40–49    | None             |   104942|   28758.01|
| 50–64    | None             |    91764|   32659.78|
| 65+      | None             |    19354|   12639.78|
| 40–49    | Private/Military |   874479|   94469.97|
| 50–64    | Private/Military |  2329984|  168166.39|
| 65+      | Private/Military |  1157580|  105357.32|
| 40–49    | Public           |   173875|   38282.26|
| 50–64    | Public           |   369318|   63476.88|
| 65+      | Public           |  1101924|  103161.23|

``` r
# mammogram by chronic conditions
lcond_chronic_pct_strat = svyby(~mam_2, by = ~domain+age_cat+lcond_chronic_cat, svymean, na.rm = TRUE, design = des)
dis_tab = lcond_chronic_pct_strat %>% filter(domain == 1) %>% select(-domain, -se) 
dis_tab %>% knitr::kable()
```

| age\_cat | lcond\_chronic\_cat |     mam\_2|
|:---------|:--------------------|----------:|
| 40–49    | No                  |  0.3736354|
| 50–64    | No                  |  0.6058108|
| 65+      | No                  |  0.2284719|
| 40–49    | Yes                 |  0.3141957|
| 50–64    | Yes                 |  0.2564085|
| 65+      | Yes                 |  0.1994143|

``` r
lcond_chronic_tot_strat = svyby(~mam_2, by = ~domain+age_cat+lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot_strat %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat | lcond\_chronic\_cat |  mam\_2|        se|
|:---------|:--------------------|-------:|---------:|
| 40–49    | No                  |    6811|   6811.00|
| 50–64    | No                  |   16535|  16535.00|
| 65+      | No                  |   20984|  11472.87|
| 40–49    | Yes                 |  152192|  35247.06|
| 50–64    | Yes                 |  529361|  71274.65|
| 65+      | Yes                 |  752690|  87271.06|

``` r
# mammogram by race
race_pct_strat = svyby(~mam_2, by = ~domain+age_cat+race_cat, svymean, na.rm = TRUE, design = des)
race_tab = race_pct_strat %>% filter(domain == 1) %>% select(-domain, -se) 
race_tab %>% knitr::kable()
```

| age\_cat | race\_cat |     mam\_2|
|:---------|:----------|----------:|
| 40–49    | AN/AI     |  0.1064797|
| 50–64    | AN/AI     |  0.1820756|
| 65+      | AN/AI     |  0.2507746|
| 40–49    | Asian     |  0.3662753|
| 50–64    | Asian     |  0.3539920|
| 65+      | Asian     |  0.2158222|
| 40–49    | Black     |  0.3995041|
| 50–64    | Black     |  0.4135665|
| 65+      | Black     |  0.3147076|
| 40–49    | White     |  0.3000826|
| 50–64    | White     |  0.3143985|
| 65+      | White     |  0.2620824|

``` r
race_tot_strat = svyby(~mam_2, by = ~domain+age_cat+race_cat, svytotal, na.rm = TRUE, design = des)
race_tot_strat %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat | race\_cat |   mam\_2|          se|
|:---------|:----------|--------:|-----------:|
| 40–49    | AN/AI     |     6044|    5832.964|
| 50–64    | AN/AI     |    22825|   11121.083|
| 65+      | AN/AI     |    18453|   16410.649|
| 40–49    | Asian     |   130909|   38409.554|
| 50–64    | Asian     |   171033|   49568.800|
| 65+      | Asian     |    78070|   23884.416|
| 40–49    | Black     |   206397|   42985.884|
| 50–64    | Black     |   419959|   65546.706|
| 65+      | Black     |   247972|   42291.679|
| 40–49    | White     |   817361|   90029.174|
| 50–64    | White     |  2179692|  173365.512|
| 65+      | White     |  1937739|  132300.891|

``` r
# mammogram by ethnicity
eth_pct_strat = svyby(~mam_2, by = ~domain+age_cat+eth_cat, svymean, na.rm = TRUE, design = des)
eth_tab = eth_pct_strat %>% filter(domain == 1) %>% select(-domain, -se) 
eth_tab %>% knitr::kable()
```

| age\_cat | eth\_cat           |     mam\_2|
|:---------|:-------------------|----------:|
| 40–49    | Hispanic           |  0.3614476|
| 50–64    | Hispanic           |  0.4632209|
| 65+      | Hispanic           |  0.3555218|
| 40–49    | Non-Hispanic AN/AI |  0.0141961|
| 50–64    | Non-Hispanic AN/AI |  0.1715973|
| 65+      | Non-Hispanic AN/AI |  0.3033935|
| 40–49    | Non-Hispanic Asian |  0.3744387|
| 50–64    | Non-Hispanic Asian |  0.3560777|
| 65+      | Non-Hispanic Asian |  0.2195271|
| 40–49    | Non-Hispanic Black |  0.4116241|
| 50–64    | Non-Hispanic Black |  0.4052808|
| 65+      | Non-Hispanic Black |  0.3084453|
| 40–49    | Non-Hispanic White |  0.2753570|
| 50–64    | Non-Hispanic White |  0.2934804|
| 65+      | Non-Hispanic White |  0.2514847|

``` r
eth_tot_strat = svyby(~mam_2, by = ~domain+age_cat+eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot_strat %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat | eth\_cat           |   mam\_2|          se|
|:---------|:-------------------|--------:|-----------:|
| 40–49    | Hispanic           |   252058|   42549.974|
| 50–64    | Hispanic           |   424755|   68281.878|
| 65+      | Hispanic           |   272505|   41259.328|
| 40–49    | Non-Hispanic AN/AI |      215|     215.000|
| 50–64    | Non-Hispanic AN/AI |    15986|    9986.047|
| 65+      | Non-Hispanic AN/AI |    18453|   16410.649|
| 40–49    | Non-Hispanic Asian |   129828|   38394.339|
| 50–64    | Non-Hispanic Asian |   171033|   49568.800|
| 65+      | Non-Hispanic Asian |    78070|   23884.416|
| 40–49    | Non-Hispanic Black |   193204|   41870.391|
| 50–64    | Non-Hispanic Black |   391064|   63879.518|
| 65+      | Non-Hispanic Black |   236547|   39354.178|
| 40–49    | Non-Hispanic White |   585406|   79913.930|
| 50–64    | Non-Hispanic White |  1790671|  159762.452|
| 65+      | Non-Hispanic White |  1676659|  127582.292|

``` r
# total gotten mammogram
total = svyby(~mam_2, by = ~domain+age_cat, svymean, na.rm = TRUE, design = des)
tot_tab = total %>% filter(domain == 1) %>% select(-domain, -se) 
tot_tab %>% knitr::kable()
```

| age\_cat |     mam\_2|
|:---------|----------:|
| 40–49    |  0.3176038|
| 50–64    |  0.3264640|
| 65+      |  0.2648560|

Combine into paper table

``` r
tot_tab2 = tot_tab %>%
  mutate(type = "Total",
         level = "-")

edu_tab2 = edu_tab %>%
  mutate(type = "Education") %>%
  rename(level = educ_cat)

finc_tab2 = finc_tab %>%
  mutate(type = "Family Income Poverty Ratio") %>%
  rename(level = finc_cat)

usual_tab2 = usual_tab %>%
  mutate(type = "Usual Source of Care") %>%
  rename(level = ausualpl_cat)

ins_tab2 = ins_tab %>%
  mutate(type = "Insurance Type") %>%
  rename(level = cover_cat)

dis_tab2 = dis_tab %>%
  mutate(type = "Chronic Disability") %>%
  rename(level = lcond_chronic_cat)

eth_tab2 = eth_tab %>%
  mutate(type = "Ethnicity") %>%
  rename(level = eth_cat)

race_tab2 = race_tab %>%
  mutate(type = "Race") %>%
  rename(level = race_cat)

# create table of percentages of women who have gotten mammograms within the last two years (still need to add CIs)
tab_one = rbind(tot_tab2, edu_tab2, finc_tab2, usual_tab2, ins_tab2, dis_tab2, eth_tab2, race_tab2) %>%
  mutate(mam_2 = round(mam_2*100, 2)) %>%
  pivot_wider(names_from = age_cat, values_from = mam_2) 

# print percentages
tab_one %>% knitr::kable()
```

| type                        | level                        |  40–49|  50–64|    65+|
|:----------------------------|:-----------------------------|------:|------:|------:|
| Total                       | -                            |  31.76|  32.65|  26.49|
| Education                   | College graduate             |  36.28|  43.68|  32.49|
| Education                   | High school                  |  25.53|  26.64|  21.56|
| Education                   | Less than high school        |  34.08|  29.07|  27.97|
| Education                   | Some college                 |  30.60|  29.58|  28.70|
| Family Income Poverty Ratio | &lt;200%                     |  31.11|  26.60|  20.59|
| Family Income Poverty Ratio | &gt;=200%, no further detail |  42.90|  34.34|  30.71|
| Family Income Poverty Ratio | &gt;=500%                    |  34.71|  41.41|  33.41|
| Family Income Poverty Ratio | 200–299%                     |  17.98|  25.78|  26.44|
| Family Income Poverty Ratio | 300–399%                     |  30.99|  29.46|  29.68|
| Family Income Poverty Ratio | 400–499%                     |  47.30|  44.13|  33.17|
| Family Income Poverty Ratio | Unknown                      |   0.00|   0.00|  11.73|
| Usual Source of Care        | No                           |  14.10|  11.29|  16.83|
| Usual Source of Care        | Other                        |   0.00|   0.00|   0.00|
| Usual Source of Care        | Yes                          |  34.54|  34.66|  26.82|
| Insurance Type              | None                         |  19.59|  12.01|  37.97|
| Insurance Type              | Private/Military             |  34.08|  35.81|  27.83|
| Insurance Type              | Public                       |  32.22|  29.62|  25.14|
| Chronic Disability          | No                           |  37.36|  60.58|  22.85|
| Chronic Disability          | Yes                          |  31.42|  25.64|  19.94|
| Ethnicity                   | Hispanic                     |  36.14|  46.32|  35.55|
| Ethnicity                   | Non-Hispanic AN/AI           |   1.42|  17.16|  30.34|
| Ethnicity                   | Non-Hispanic Asian           |  37.44|  35.61|  21.95|
| Ethnicity                   | Non-Hispanic Black           |  41.16|  40.53|  30.84|
| Ethnicity                   | Non-Hispanic White           |  27.54|  29.35|  25.15|
| Race                        | AN/AI                        |  10.65|  18.21|  25.08|
| Race                        | Asian                        |  36.63|  35.40|  21.58|
| Race                        | Black                        |  39.95|  41.36|  31.47|
| Race                        | White                        |  30.01|  31.44|  26.21|
