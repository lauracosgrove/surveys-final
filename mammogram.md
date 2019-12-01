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
# outcome is having a mammogram in the last 2 years:RMAM3A = 1,2
mam_dat = mam_dat %>%
  mutate(mam_2 = if_else(RMAM3A <= 2, 1, 0))

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
| 40–49    |  0.7609720|  0.0133452|
| 50–64    |  0.7510356|  0.0087326|
| 65+      |  0.6669962|  0.0095942|

``` r
age_tot = svyby(~mam_2, by = ~domain+age_cat, svytotal, na.rm = TRUE, design = des)
age_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| age\_cat |    mam\_2|        se|
|:---------|---------:|---------:|
| 40–49    |  10999732|  412376.4|
| 50–64    |  20982399|  538420.9|
| 65+      |  14921885|  399328.3|

``` r
# mammogram by education
edu_pct = svyby(~mam_2, by = ~domain+educ_cat, svymean, na.rm = TRUE, design = des, vartype = c("se", "ci"))
edu_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| educ\_cat             |     mam\_2|         se|      ci\_l|      ci\_u|
|:----------------------|----------:|----------:|----------:|----------:|
| College graduate      |  0.7952259|  0.0098218|  0.7759755|  0.8144763|
| High school           |  0.6785310|  0.0120987|  0.6548180|  0.7022440|
| Less than high school |  0.6233975|  0.0180323|  0.5880548|  0.6587402|
| Some college          |  0.7298794|  0.0108725|  0.7085698|  0.7511891|

``` r
edu_tot = svyby(~mam_2, by = ~domain+educ_cat, svytotal, na.rm = TRUE, design = des)
edu_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| educ\_cat             |    mam\_2|        se|
|:----------------------|---------:|---------:|
| College graduate      |  16620845|  482553.4|
| High school           |  10865536|  340931.8|
| Less than high school |   4819008|  242908.6|
| Some college          |  14435418|  395746.8|

``` r
# mammogram by finc
finc_pct = svyby(~mam_2, by = ~domain+finc_cat, svymean, na.rm = TRUE, design = des, vartype = c("se", "ci"))
finc_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| finc\_cat                    |     mam\_2|         se|      ci\_l|      ci\_u|
|:-----------------------------|----------:|----------:|----------:|----------:|
| &lt;200%                     |  0.6336404|  0.0119245|  0.6102688|  0.6570120|
| &gt;=200%, no further detail |  0.6829528|  0.0329961|  0.6182815|  0.7476241|
| &gt;=500%                    |  0.8049806|  0.0113068|  0.7828196|  0.8271415|
| 200–299%                     |  0.6846054|  0.0190000|  0.6473660|  0.7218448|
| 300–399%                     |  0.7389614|  0.0185501|  0.7026038|  0.7753189|
| 400–499%                     |  0.7582742|  0.0162872|  0.7263519|  0.7901964|
| Unknown                      |  0.4531864|  0.1295039|  0.1993634|  0.7070094|

``` r
finc_tot = svyby(~mam_2, by = ~domain+finc_cat, svytotal, na.rm = TRUE, design = des)
finc_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| finc\_cat                    |    mam\_2|         se|
|:-----------------------------|---------:|----------:|
| &lt;200%                     |  10630008|  335667.41|
| &gt;=200%, no further detail |   1318243|  104446.60|
| &gt;=500%                    |  14180699|  469467.56|
| 200–299%                     |   5604138|  261874.72|
| 300–399%                     |   4781717|  222429.80|
| 400–499%                     |   6498847|  280585.63|
| Unknown                      |     42077|   17089.33|

``` r
# mammogram by usual care
ausualp_pct = svyby(~mam_2, by = ~domain+ausualpl_cat, svymean, na.rm = TRUE, design = des, vartype = c("se", "ci"))
ausualp_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| ausualpl\_cat |     mam\_2|         se|       ci\_l|      ci\_u|
|:--------------|----------:|----------:|-----------:|----------:|
| No            |  0.4426809|  0.0290530|   0.3857382|  0.4996237|
| Other         |  0.6349977|  0.3277802|  -0.0074398|  1.2774351|
| Yes           |  0.7374589|  0.0059183|   0.7258591|  0.7490586|

``` r
ausualp_tot = svyby(~mam_2, by = ~domain+ausualpl_cat, svytotal, na.rm = TRUE, design = des)
ausualp_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| ausualpl\_cat |    mam\_2|        se|
|:--------------|---------:|---------:|
| No            |   1284645|  106112.8|
| Other         |     10861|   10861.0|
| Yes           |  45608510|  727187.8|

``` r
# mammogram by health coverage
cover_pct = svyby(~mam_2, by = ~domain+cover_cat, svymean, na.rm = TRUE, design = des, vartype = c("se", "ci"))
cover_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| cover\_cat       |     mam\_2|         se|      ci\_l|      ci\_u|
|:-----------------|----------:|----------:|----------:|----------:|
| None             |  0.4711737|  0.0361064|  0.4004064|  0.5419410|
| Private/Military |  0.7605363|  0.0070446|  0.7467293|  0.7743434|
| Public           |  0.6643714|  0.0119487|  0.6409525|  0.6877904|

``` r
cover_tot = svyby(~mam_2, by = ~domain+cover_cat, svytotal, na.rm = TRUE, design = des)
cover_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| cover\_cat       |    mam\_2|        se|
|:-----------------|---------:|---------:|
| None             |   1314471|  119245.3|
| Private/Military |  34891787|  684081.3|
| Public           |  10565726|  364737.9|

``` r
# mammogram by chronic conditions
lcond_chronic_pct = svyby(~mam_2, by = ~domain+lcond_chronic_cat, svymean, na.rm = TRUE, design = des, vartype = c("se", "ci"))
lcond_chronic_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| lcond\_chronic\_cat |     mam\_2|         se|      ci\_l|      ci\_u|
|:--------------------|----------:|----------:|----------:|----------:|
| No                  |  0.6235415|  0.0832900|  0.4602961|  0.7867868|
| Yes                 |  0.6105575|  0.0133315|  0.5844282|  0.6366868|

``` r
lcond_chronic_tot = svyby(~mam_2, by = ~domain+lcond_chronic_cat, svytotal, na.rm = TRUE, design = des)
lcond_chronic_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| lcond\_chronic\_cat |   mam\_2|         se|
|:--------------------|--------:|----------:|
| No                  |   177632|   40049.09|
| Yes                 |  8938239|  282408.67|

``` r
#mammogram by race
race_pct = svyby(~mam_2, by = ~domain+race_cat, svymean, na.rm = TRUE, design = des, vartype = c("se", "ci"))
race_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| race\_cat |     mam\_2|         se|      ci\_l|      ci\_u|
|:----------|----------:|----------:|----------:|----------:|
| AN/AI     |  0.6382206|  0.0564566|  0.5275676|  0.7488735|
| Asian     |  0.7023464|  0.0277336|  0.6479896|  0.7567032|
| Black     |  0.7688270|  0.0154351|  0.7385748|  0.7990792|
| White     |  0.7203100|  0.0066914|  0.7071951|  0.7334248|

``` r
race_tot = svyby(~mam_2, by = ~domain+race_cat, svytotal, na.rm = TRUE, design = des)
race_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| race\_cat |    mam\_2|        se|
|:----------|---------:|---------:|
| AN/AI     |    459245|   70794.4|
| Asian     |   2368921|  173279.3|
| Black     |   5912505|  269555.2|
| White     |  38163345|  681107.4|

``` r
# mammogram by ethnicity
eth_pct = svyby(~mam_2, by = ~domain+eth_cat, svymean, na.rm = TRUE, design = des, vartype = c("se", "ci"))
eth_pct %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| eth\_cat           |     mam\_2|         se|      ci\_l|      ci\_u|
|:-------------------|----------:|----------:|----------:|----------:|
| Hispanic           |  0.7434129|  0.0152008|  0.7136199|  0.7732058|
| Non-Hispanic AN/AI |  0.6787829|  0.0660549|  0.5493178|  0.8082481|
| Non-Hispanic Asian |  0.7005102|  0.0280754|  0.6454834|  0.7555370|
| Non-Hispanic Black |  0.7684556|  0.0156438|  0.7377942|  0.7991169|
| Non-Hispanic White |  0.7165402|  0.0071231|  0.7025792|  0.7305012|

``` r
eth_tot = svyby(~mam_2, by = ~domain+eth_cat, svytotal, na.rm = TRUE, design = des)
eth_tot %>% filter(domain == 1) %>% select(-domain) %>% knitr::kable()
```

| eth\_cat           |    mam\_2|        se|
|:-------------------|---------:|---------:|
| Hispanic           |   5247845|  229324.5|
| Non-Hispanic AN/AI |    356900|   65104.9|
| Non-Hispanic Asian |   2301826|  169231.2|
| Non-Hispanic Black |   5631161|  263569.5|
| Non-Hispanic White |  33366284|  653414.1|

Tables By Age Group

``` r
# mammogram by education
edu_pct_strat = svyby(~mam_2, by = ~domain + age_cat + educ_cat, svymean, 
                      na.rm = TRUE, design = des, vartype = c("se", "ci"))
edu_tab = edu_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

edu_tab %>% knitr::kable()
```

| age\_cat | educ\_cat             |     mam\_2|      ci\_l|      ci\_u|
|:---------|:----------------------|----------:|----------:|----------:|
| 40–49    | College graduate      |  0.8085672|  0.7707166|  0.8464178|
| 50–64    | College graduate      |  0.8109270|  0.7831941|  0.8386598|
| 65+      | College graduate      |  0.7537526|  0.7172640|  0.7902411|
| 40–49    | High school           |  0.6818410|  0.6144456|  0.7492364|
| 50–64    | High school           |  0.7247121|  0.6848474|  0.7645768|
| 65+      | High school           |  0.6336142|  0.5983451|  0.6688834|
| 40–49    | Less than high school |  0.6999305|  0.6228325|  0.7770285|
| 50–64    | Less than high school |  0.6506867|  0.5928630|  0.7085104|
| 65+      | Less than high school |  0.5719782|  0.5172242|  0.6267323|
| 40–49    | Some college          |  0.7628288|  0.7164342|  0.8092234|
| 50–64    | Some college          |  0.7406601|  0.7105614|  0.7707588|
| 65+      | Some college          |  0.6909571|  0.6565986|  0.7253157|

``` r
edu_tot_strat = svyby(~mam_2, by = ~domain + age_cat + educ_cat, svytotal, 
                      na.rm = TRUE, design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)
edu_tot_strat %>% 
  knitr::kable()
```

| age\_cat | educ\_cat             |      num|
|:---------|:----------------------|--------:|
| 40–49    | College graduate      |  4757007|
| 50–64    | College graduate      |  7720546|
| 65+      | College graduate      |  4143292|
| 40–49    | High school           |  1803601|
| 50–64    | High school           |  4707140|
| 65+      | High school           |  4354795|
| 40–49    | Less than high school |   936057|
| 50–64    | Less than high school |  1871371|
| 65+      | Less than high school |  2011580|
| 40–49    | Some college          |  3450026|
| 50–64    | Some college          |  6627479|
| 65+      | Some college          |  4357913|

``` r
num = edu_tot_strat$num

edu_tab = cbind(edu_tab, num) 

# mammogram by finc
finc_pct_strat = svyby(~mam_2, by = ~domain + age_cat + finc_cat, svymean, 
                       na.rm = TRUE, design = des, vartype = c("se", "ci"))

finc_tab = finc_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

finc_tab %>% knitr::kable()
```

| age\_cat | finc\_cat                    |     mam\_2|       ci\_l|      ci\_u|
|:---------|:-----------------------------|----------:|-----------:|----------:|
| 40–49    | &lt;200%                     |  0.6956259|   0.6448771|  0.7463748|
| 50–64    | &lt;200%                     |  0.6495575|   0.6101441|  0.6889709|
| 65+      | &lt;200%                     |  0.5848766|   0.5493209|  0.6204322|
| 40–49    | &gt;=200%, no further detail |  0.8594243|   0.7506689|  0.9681797|
| 50–64    | &gt;=200%, no further detail |  0.7350658|   0.6237214|  0.8464101|
| 65+      | &gt;=200%, no further detail |  0.6088342|   0.5150514|  0.7026171|
| 40–49    | &gt;=500%                    |  0.8120277|   0.7701024|  0.8539529|
| 50–64    | &gt;=500%                    |  0.8105147|   0.7804614|  0.8405680|
| 65+      | &gt;=500%                    |  0.7855235|   0.7417109|  0.8293361|
| 40–49    | 200–299%                     |  0.7072433|   0.6214273|  0.7930594|
| 50–64    | 200–299%                     |  0.7089483|   0.6459559|  0.7719406|
| 65+      | 200–299%                     |  0.6470099|   0.5991062|  0.6949135|
| 40–49    | 300–399%                     |  0.7310784|   0.6541823|  0.8079745|
| 50–64    | 300–399%                     |  0.7706667|   0.7051139|  0.8362195|
| 65+      | 300–399%                     |  0.7085731|   0.6507690|  0.7663771|
| 40–49    | 400–499%                     |  0.8174002|   0.7382089|  0.8965915|
| 50–64    | 400–499%                     |  0.7917000|   0.7382696|  0.8451305|
| 65+      | 400–499%                     |  0.7010436|   0.6564844|  0.7456028|
| 40–49    | Unknown                      |  0.0000000|   0.0000000|  0.0000000|
| 50–64    | Unknown                      |  0.9962176|   0.9879882|  1.0044469|
| 65+      | Unknown                      |  0.2035140|  -0.0544638|  0.4614919|

``` r
finc_tot_strat = svyby(~mam_2, by = ~ domain + age_cat + finc_cat, svytotal,
                       na.rm = TRUE, design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)

num = finc_tot_strat$num

finc_tab = cbind(finc_tab, num)


# mammogram by usual care
ausualp_pct_strat = svyby(~mam_2, by = ~domain + age_cat + ausualpl_cat,
                          svymean, na.rm = TRUE, design = des, 
                          vartype = c("se", "ci"))

usual_tab = ausualp_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

usual_tab %>% knitr::kable()
```

| age\_cat | ausualpl\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|:--------------|----------:|----------:|----------:|
| 40–49    | No            |  0.5056792|  0.4057956|  0.6055628|
| 50–64    | No            |  0.3810310|  0.2889497|  0.4731123|
| 65+      | No            |  0.4840104|  0.3523452|  0.6156756|
| 40–49    | Other         |  0.0000000|  0.0000000|  0.0000000|
| 50–64    | Other         |  1.0000000|  1.0000000|  1.0000000|
| 65+      | Other         |  0.0000000|  0.0000000|  0.0000000|
| 40–49    | Yes           |  0.7802444|  0.7542141|  0.8062747|
| 50–64    | Yes           |  0.7700861|  0.7529459|  0.7872262|
| 65+      | Yes           |  0.6714421|  0.6525374|  0.6903469|

``` r
ausualp_tot_strat = svyby(~mam_2, by = ~domain + age_cat +ausualpl_cat,
                          svytotal, na.rm = TRUE, design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)

num = ausualp_tot_strat$num

usual_tab = cbind(usual_tab, num)

# mammogram by health coverage
cover_pct_strat = svyby(~mam_2, by = ~domain + age_cat + cover_cat, svymean,
                        na.rm = TRUE, design = des, vartype = c("se", "ci"))

ins_tab = cover_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se)

ins_tab %>% knitr::kable()
```

| age\_cat | cover\_cat       |     mam\_2|      ci\_l|      ci\_u|
|:---------|:-----------------|----------:|----------:|----------:|
| 40–49    | None             |  0.4829030|  0.3629256|  0.6028804|
| 50–64    | None             |  0.4603852|  0.3769433|  0.5438272|
| 65+      | None             |  0.5160788|  0.2196597|  0.8124978|
| 40–49    | Private/Military |  0.7914360|  0.7633846|  0.8194874|
| 50–64    | Private/Military |  0.7802779|  0.7610562|  0.7994996|
| 65+      | Private/Military |  0.6871598|  0.6611642|  0.7131553|
| 40–49    | Public           |  0.7370242|  0.6725539|  0.8014945|
| 50–64    | Public           |  0.6818896|  0.6241168|  0.7396625|
| 65+      | Public           |  0.6486552|  0.6199508|  0.6773596|

``` r
cover_tot_strat = svyby(~mam_2, by = ~domain + age_cat + cover_cat, svytotal,
                        na.rm = TRUE, design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)

num = cover_tot_strat$num

ins_tab = cbind(ins_tab, num)

# mammogram by chronic conditions
lcond_chronic_pct_strat = svyby(~mam_2, 
                                by = ~domain + age_cat + lcond_chronic_cat,
                                svymean, na.rm = TRUE, design = des,
                                vartype = c("se", "ci"))

dis_tab = lcond_chronic_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

dis_tab %>% knitr::kable()
```

| age\_cat | lcond\_chronic\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|:--------------------|----------:|----------:|----------:|
| 40–49    | No                  |  0.7176907|  0.3453040|  1.0900773|
| 50–64    | No                  |  0.8619131|  0.7174233|  1.0064030|
| 65+      | No                  |  0.4720814|  0.2627660|  0.6813969|
| 40–49    | Yes                 |  0.6903587|  0.6133066|  0.7674109|
| 50–64    | Yes                 |  0.6848202|  0.6423921|  0.7272482|
| 65+      | Yes                 |  0.5381731|  0.5032309|  0.5731153|

``` r
lcond_chronic_tot_strat = svyby(~mam_2, 
                                by = ~domain + age_cat + lcond_chronic_cat,
                                svytotal, na.rm = TRUE, design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)

num = lcond_chronic_tot_strat$num

dis_tab = cbind(dis_tab, num)

# mammogram by race
race_pct_strat = svyby(~mam_2, by = ~domain + age_cat + race_cat, svymean, 
                       na.rm = TRUE, design = des, vartype = c("se", "ci"))

race_tab = race_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

race_tab %>% knitr::kable()
```

| age\_cat | race\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|:----------|----------:|----------:|----------:|
| 40–49    | AN/AI     |  0.6820006|  0.4637318|  0.9002694|
| 50–64    | AN/AI     |  0.5832300|  0.4209941|  0.7454660|
| 65+      | AN/AI     |  0.6798373|  0.4792802|  0.8803944|
| 40–49    | Asian     |  0.7285098|  0.6249686|  0.8320510|
| 50–64    | Asian     |  0.7388677|  0.6510686|  0.8266667|
| 65+      | Asian     |  0.6051615|  0.5083183|  0.7020048|
| 40–49    | Black     |  0.8109375|  0.7584382|  0.8634368|
| 50–64    | Black     |  0.7983266|  0.7574828|  0.8391704|
| 65+      | Black     |  0.6804674|  0.6261574|  0.7347773|
| 40–49    | White     |  0.7567230|  0.7267415|  0.7867046|
| 50–64    | White     |  0.7467055|  0.7263464|  0.7670646|
| 65+      | White     |  0.6680536|  0.6481251|  0.6879822|

``` r
race_tot_strat = svyby(~mam_2, by = ~domain + age_cat + race_cat, svytotal,
                       na.rm = TRUE, design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)

num = race_tot_strat$num

race_tab = cbind(race_tab, num)

# mammogram by ethnicity
eth_pct_strat = svyby(~mam_2, by = ~domain + age_cat + eth_cat, svymean, 
                      na.rm = TRUE, design = des, vartype = c("se", "ci"))

eth_tab = eth_pct_strat %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

eth_tab %>% knitr::kable()
```

| age\_cat | eth\_cat           |     mam\_2|      ci\_l|      ci\_u|
|:---------|:-------------------|----------:|----------:|----------:|
| 40–49    | Hispanic           |  0.7350049|  0.6860044|  0.7840053|
| 50–64    | Hispanic           |  0.7887929|  0.7425060|  0.8350797|
| 65+      | Hispanic           |  0.6757057|  0.6174945|  0.7339170|
| 40–49    | Non-Hispanic AN/AI |  0.8614933|  0.7233204|  0.9996662|
| 50–64    | Non-Hispanic AN/AI |  0.5788007|  0.3741846|  0.7834168|
| 65+      | Non-Hispanic AN/AI |  0.6618173|  0.4220170|  0.9016176|
| 40–49    | Non-Hispanic Asian |  0.7255322|  0.6191024|  0.8319621|
| 50–64    | Non-Hispanic Asian |  0.7358176|  0.6461917|  0.8254434|
| 65+      | Non-Hispanic Asian |  0.6088310|  0.5113248|  0.7063372|
| 40–49    | Non-Hispanic Black |  0.8174351|  0.7624666|  0.8724036|
| 50–64    | Non-Hispanic Black |  0.7971501|  0.7556416|  0.8386587|
| 65+      | Non-Hispanic Black |  0.6774079|  0.6234387|  0.7313772|
| 40–49    | Non-Hispanic White |  0.7584481|  0.7227714|  0.7941248|
| 50–64    | Non-Hispanic White |  0.7405940|  0.7183667|  0.7628213|
| 65+      | Non-Hispanic White |  0.6676896|  0.6471933|  0.6881859|

``` r
eth_tot_strat = svyby(~mam_2, by = ~domain + age_cat + eth_cat, svytotal, 
                      na.rm = TRUE, design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)

num = eth_tot_strat$num

eth_tab = cbind(eth_tab, num)

# total gotten mammogram
total = svyby(~mam_2, by = ~domain + age_cat, svymean, na.rm = TRUE, 
              design = des, vartype = c("se", "ci"))

tot_tab = total %>% 
  filter(domain == 1) %>% 
  select(-domain, -se) 

tot_tab %>% knitr::kable()
```

| age\_cat |     mam\_2|      ci\_l|      ci\_u|
|:---------|----------:|----------:|----------:|
| 40–49    |  0.7609720|  0.7348160|  0.7871280|
| 50–64    |  0.7510356|  0.7339199|  0.7681512|
| 65+      |  0.6669962|  0.6481919|  0.6858006|

``` r
tot_num = svyby(~mam_2, by = ~domain + age_cat, svytotal, na.rm = TRUE, 
                design = des) %>%
  filter(domain == 1) %>%
  select(-domain, -se) %>%
  rename(num = mam_2)

num = tot_num$num

tot_tab = cbind(tot_tab, num)
```

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
  mutate(mam_2 = round(mam_2*100, 2),
         ci_l = round(ci_l*100, 2),
         ci_u = round(ci_u*100, 2),
         CI = str_c(ci_l, ", ", ci_u)) %>%
  rename(Percent = mam_2,
         No = num) %>%
  select(-ci_l, -ci_u) %>%
  pivot_wider(names_from = age_cat, values_from = c(No, Percent, CI)) %>%
  janitor::clean_names() %>%
  select(type, level, no_40_49, percent_40_49, ci_40_49, no_50_64, percent_50_64, ci_50_64, everything())

# print percentages
tab_one %>% knitr::kable()
```

| type                        | level                        |  no\_40\_49|  percent\_40\_49| ci\_40\_49    |  no\_50\_64|  percent\_50\_64| ci\_50\_64    |    no\_65|  percent\_65| ci\_65       |
|:----------------------------|:-----------------------------|-----------:|----------------:|:--------------|-----------:|----------------:|:--------------|---------:|------------:|:-------------|
| Total                       | -                            |    10999732|            76.10| 73.48, 78.71  |    20982399|            75.10| 73.39, 76.82  |  14921885|        66.70| 64.82, 68.58 |
| Education                   | College graduate             |     4757007|            80.86| 77.07, 84.64  |     7720546|            81.09| 78.32, 83.87  |   4143292|        75.38| 71.73, 79.02 |
| Education                   | High school                  |     1803601|            68.18| 61.44, 74.92  |     4707140|            72.47| 68.48, 76.46  |   4354795|        63.36| 59.83, 66.89 |
| Education                   | Less than high school        |      936057|            69.99| 62.28, 77.7   |     1871371|            65.07| 59.29, 70.85  |   2011580|        57.20| 51.72, 62.67 |
| Education                   | Some college                 |     3450026|            76.28| 71.64, 80.92  |     6627479|            74.07| 71.06, 77.08  |   4357913|        69.10| 65.66, 72.53 |
| Family Income Poverty Ratio | &lt;200%                     |     2485996|            69.56| 64.49, 74.64  |     4240692|            64.96| 61.01, 68.9   |   3903320|        58.49| 54.93, 62.04 |
| Family Income Poverty Ratio | &gt;=200%, no further detail |      213805|            85.94| 75.07, 96.82  |      470065|            73.51| 62.37, 84.64  |    634373|        60.88| 51.51, 70.26 |
| Family Income Poverty Ratio | &gt;=500%                    |     3665112|            81.20| 77.01, 85.4   |     7236643|            81.05| 78.05, 84.06  |   3278944|        78.55| 74.17, 82.93 |
| Family Income Poverty Ratio | 200–299%                     |     1356108|            70.72| 62.14, 79.31  |     2200608|            70.89| 64.6, 77.19   |   2047422|        64.70| 59.91, 69.49 |
| Family Income Poverty Ratio | 300–399%                     |     1189352|            73.11| 65.42, 80.8   |     1986139|            77.07| 70.51, 83.62  |   1606226|        70.86| 65.08, 76.64 |
| Family Income Poverty Ratio | 400–499%                     |     1059318|            81.74| 73.82, 89.66  |     2966637|            79.17| 73.83, 84.51  |   2472892|        70.10| 65.65, 74.56 |
| Family Income Poverty Ratio | Unknown                      |           0|             0.00| 0, 0          |       30552|            99.62| 98.8, 100.44  |     11525|        20.35| -5.45, 46.15 |
| Usual Source of Care        | No                           |      504101|            50.57| 40.58, 60.56  |      523700|            38.10| 28.89, 47.31  |    256844|        48.40| 35.23, 61.57 |
| Usual Source of Care        | Other                        |           0|             0.00| 0, 0          |       10861|           100.00| 100, 100      |         0|         0.00| 0, 0         |
| Usual Source of Care        | Yes                          |    10495631|            78.02| 75.42, 80.63  |    20447838|            77.01| 75.29, 78.72  |  14665041|        67.14| 65.25, 69.03 |
| Insurance Type              | None                         |      554235|            48.29| 36.29, 60.29  |      720821|            46.04| 37.69, 54.38  |     39415|        51.61| 21.97, 81.25 |
| Insurance Type              | Private/Military             |     9174688|            79.14| 76.34, 81.95  |    18079005|            78.03| 76.11, 79.95  |   7638094|        68.72| 66.12, 71.32 |
| Insurance Type              | Public                       |     1228329|            73.70| 67.26, 80.15  |     2106415|            68.19| 62.41, 73.97  |   7230982|        64.87| 62, 67.74    |
| Chronic Disability          | No                           |       29027|            71.77| 34.53, 109.01 |       73435|            86.19| 71.74, 100.64 |     75170|        47.21| 26.28, 68.14 |
| Chronic Disability          | Yes                          |      986776|            69.04| 61.33, 76.74  |     3932671|            68.48| 64.24, 72.72  |   4018792|        53.82| 50.32, 57.31 |
| Ethnicity                   | Hispanic                     |     1694625|            73.50| 68.6, 78.4    |     2380132|            78.88| 74.25, 83.51  |   1173088|        67.57| 61.75, 73.39 |
| Ethnicity                   | Non-Hispanic AN/AI           |      122295|            86.15| 72.33, 99.97  |      135433|            57.88| 37.42, 78.34  |     99172|        66.18| 42.2, 90.16  |
| Ethnicity                   | Non-Hispanic Asian           |      770878|            72.55| 61.91, 83.2   |     1027102|            73.58| 64.62, 82.54  |    503846|        60.88| 51.13, 70.63 |
| Ethnicity                   | Non-Hispanic Black           |     1536545|            81.74| 76.25, 87.24  |     2689359|            79.72| 75.56, 83.87  |   1405257|        67.74| 62.34, 73.14 |
| Ethnicity                   | Non-Hispanic White           |     6875389|            75.84| 72.28, 79.41  |    14750373|            74.06| 71.84, 76.28  |  11740522|        66.77| 64.72, 68.82 |
| Race                        | AN/AI                        |      141050|            68.20| 46.37, 90.03  |      183490|            58.32| 42.1, 74.55   |    134705|        67.98| 47.93, 88.04 |
| Race                        | Asian                        |      808286|            72.85| 62.5, 83.21   |     1055123|            73.89| 65.11, 82.67  |    505512|        60.52| 50.83, 70.2  |
| Race                        | Black                        |     1618079|            81.09| 75.84, 86.34  |     2839354|            79.83| 75.75, 83.92  |   1455072|        68.05| 62.62, 73.48 |
| Race                        | White                        |     8432317|            75.67| 72.67, 78.67  |    16904432|            74.67| 72.63, 76.71  |  12826596|        66.81| 64.81, 68.8  |

Barplots of Usual Care

``` r
b = svyby(~mam_2, by = ~domain + age_cat + ausualpl_cat,
                          svymean, na.rm = TRUE, design = des) %>%
  filter(domain == 1,
         ausualpl_cat != "Other") %>%
  select(-domain)

b1 = b %>%
  filter(trimws(age_cat) == "40–49")

b2 = b %>%
  filter(trimws(age_cat) == "50–64")

b3 = b %>%
  filter(age_cat == "65+")



barplot(b1$mam_2, beside = FALSE,
        names.arg = c("No", "Yes"),
        main = "Recent Mammogram for Women 40-49 by Usual Care",
        ylim = c(0, 1))
```

![](mammogram_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
barplot(b2$mam_2, beside = FALSE,
        names.arg = c("No", "Yes"),
        main = "Recent Mammogram for Women 50-64 by Usual Care",
        ylim = c(0, 1))
```

![](mammogram_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
barplot(b3$mam_2, beside = FALSE,
        names.arg = c("No", "Yes"),
        main = "Recent Mammogran for Women 65+ by Usual Care",
        ylim = c(0, 1))
```

![](mammogram_files/figure-markdown_github/unnamed-chunk-4-3.png)

Models

``` r
mam_fit = svyglm(mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + as.factor(finc_cat) + as.factor(ausualpl_cat) + as.factor(cover_cat) + as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat), 
       design = des, subset = domain == 1, family = binomial(link = "logit"))
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
summary(mam_fit)
```

    ## 
    ## Call:
    ## svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(lcond_chronic_cat) + as.factor(race_cat) + as.factor(eth_cat), 
    ##     design = des, subset = domain == 1, family = binomial(link = "logit"))
    ## 
    ## Survey design:
    ## svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, 
    ##     nest = TRUE, data = mam_dat)
    ## 
    ## Coefficients:
    ##                                               Estimate Std. Error t value
    ## (Intercept)                                    1.01989    1.27059   0.803
    ## as.factor(age_cat)50–64                       -0.01805    0.22186  -0.081
    ## as.factor(age_cat)65+                         -0.63876    0.22027  -2.900
    ## as.factor(educ_cat)High school                -0.40253    0.17183  -2.343
    ## as.factor(educ_cat)Less than high school      -0.53606    0.20796  -2.578
    ## as.factor(educ_cat)Some college               -0.16474    0.18213  -0.905
    ## as.factor(finc_cat)>=200%, no further detail  -0.52446    0.28205  -1.859
    ## as.factor(finc_cat)>=500%                      0.45253    0.23074   1.961
    ## as.factor(finc_cat)200–299%                   -0.11642    0.17735  -0.656
    ## as.factor(finc_cat)300–399%                    0.22997    0.26026   0.884
    ## as.factor(finc_cat)400–499%                    0.13695    0.18915   0.724
    ## as.factor(ausualpl_cat)Other                 -11.43976    1.11362 -10.273
    ## as.factor(ausualpl_cat)Yes                     0.82685    0.33261   2.486
    ## as.factor(cover_cat)Private/Military           1.10223    0.33805   3.261
    ## as.factor(cover_cat)Public                     1.14534    0.35883   3.192
    ## as.factor(lcond_chronic_cat)Yes               -0.09220    0.34409  -0.268
    ## as.factor(race_cat)Asian                      -1.86985    1.67041  -1.119
    ## as.factor(race_cat)Black                      -1.50028    1.41508  -1.060
    ## as.factor(race_cat)White                      -1.11517    1.12120  -0.995
    ## as.factor(eth_cat)Non-Hispanic AN/AI          -1.21532    1.27334  -0.954
    ## as.factor(eth_cat)Non-Hispanic Asian          -0.09389    1.28175  -0.073
    ## as.factor(eth_cat)Non-Hispanic Black           0.06401    0.90497   0.071
    ## as.factor(eth_cat)Non-Hispanic White          -0.76891    0.22091  -3.481
    ##                                              Pr(>|t|)    
    ## (Intercept)                                  0.422954    
    ## as.factor(age_cat)50–64                      0.935224    
    ## as.factor(age_cat)65+                        0.004080 ** 
    ## as.factor(educ_cat)High school               0.019970 *  
    ## as.factor(educ_cat)Less than high school     0.010546 *  
    ## as.factor(educ_cat)Some college              0.366630    
    ## as.factor(finc_cat)>=200%, no further detail 0.064189 .  
    ## as.factor(finc_cat)>=500%                    0.051017 .  
    ## as.factor(finc_cat)200–299%                  0.512183    
    ## as.factor(finc_cat)300–399%                  0.377790    
    ## as.factor(finc_cat)400–499%                  0.469752    
    ## as.factor(ausualpl_cat)Other                  < 2e-16 ***
    ## as.factor(ausualpl_cat)Yes                   0.013607 *  
    ## as.factor(cover_cat)Private/Military         0.001274 ** 
    ## as.factor(cover_cat)Public                   0.001603 ** 
    ## as.factor(lcond_chronic_cat)Yes              0.788976    
    ## as.factor(race_cat)Asian                     0.264096    
    ## as.factor(race_cat)Black                     0.290118    
    ## as.factor(race_cat)White                     0.320924    
    ## as.factor(eth_cat)Non-Hispanic AN/AI         0.340826    
    ## as.factor(eth_cat)Non-Hispanic Asian         0.941666    
    ## as.factor(eth_cat)Non-Hispanic Black         0.943669    
    ## as.factor(eth_cat)Non-Hispanic White         0.000594 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 0.8295561)
    ## 
    ## Number of Fisher Scoring iterations: 11

``` r
summ(mam_fit)
```

    ## MODEL INFO:
    ## Observations: 2673
    ## Dependent Variable: mam_2
    ## Type: Analysis of complex survey design 
    ##  Family: binomial 
    ##  Link function: logit 
    ## 
    ## MODEL FIT:
    ## Pseudo-R² (Cragg-Uhler) = 0.06
    ## Pseudo-R² (McFadden) = 0.21
    ## AIC = 2745.63 
    ## 
    ## --------------------------------------------------------------------------
    ##                                                Est.   S.E.   t val.      p
    ## ------------------------------------------ -------- ------ -------- ------
    ## (Intercept)                                    1.02   1.27     0.80   0.42
    ## as.factor(age_cat)50â€“64                     -0.02   0.22    -0.08   0.94
    ## as.factor(age_cat)65+                         -0.64   0.22    -2.90   0.00
    ## as.factor(educ_cat)High                       -0.40   0.17    -2.34   0.02
    ## school                                                                    
    ## as.factor(educ_cat)Less than                  -0.54   0.21    -2.58   0.01
    ## high school                                                               
    ## as.factor(educ_cat)Some                       -0.16   0.18    -0.90   0.37
    ## college                                                                   
    ## as.factor(finc_cat)>=200%,                    -0.52   0.28    -1.86   0.06
    ## no further detail                                                         
    ## as.factor(finc_cat)>=500%                      0.45   0.23     1.96   0.05
    ## as.factor(finc_cat)200â€“299%                 -0.12   0.18    -0.66   0.51
    ## as.factor(finc_cat)300â€“399%                  0.23   0.26     0.88   0.38
    ## as.factor(finc_cat)400â€“499%                  0.14   0.19     0.72   0.47
    ## as.factor(ausualpl_cat)Other                 -11.44   1.11   -10.27   0.00
    ## as.factor(ausualpl_cat)Yes                     0.83   0.33     2.49   0.01
    ## as.factor(cover_cat)Private/Military           1.10   0.34     3.26   0.00
    ## as.factor(cover_cat)Public                     1.15   0.36     3.19   0.00
    ## as.factor(lcond_chronic_cat)Yes               -0.09   0.34    -0.27   0.79
    ## as.factor(race_cat)Asian                      -1.87   1.67    -1.12   0.26
    ## as.factor(race_cat)Black                      -1.50   1.42    -1.06   0.29
    ## as.factor(race_cat)White                      -1.12   1.12    -0.99   0.32
    ## as.factor(eth_cat)Non-Hispanic                -1.22   1.27    -0.95   0.34
    ## AN/AI                                                                     
    ## as.factor(eth_cat)Non-Hispanic                -0.09   1.28    -0.07   0.94
    ## Asian                                                                     
    ## as.factor(eth_cat)Non-Hispanic                 0.06   0.90     0.07   0.94
    ## Black                                                                     
    ## as.factor(eth_cat)Non-Hispanic                -0.77   0.22    -3.48   0.00
    ## White                                                                     
    ## --------------------------------------------------------------------------
    ## 
    ## Estimated dispersion parameter = 0.83

``` r
Anova(mam_fit, type = 3)
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: mam_2
    ##                              Df    Chisq Pr(>Chisq)    
    ## (Intercept)                   1   0.6443   0.422157    
    ## as.factor(age_cat)            2  25.6658  2.671e-06 ***
    ## as.factor(educ_cat)           3   9.1656   0.027168 *  
    ## as.factor(finc_cat)           5  11.2129   0.047319 *  
    ## as.factor(ausualpl_cat)       2 121.8344  < 2.2e-16 ***
    ## as.factor(cover_cat)          2  10.8012   0.004514 ** 
    ## as.factor(lcond_chronic_cat)  1   0.0718   0.788745    
    ## as.factor(race_cat)           3   1.5523   0.670259    
    ## as.factor(eth_cat)            4  13.0402   0.011081 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mam_fit2 = svyglm(mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + as.factor(finc_cat) + as.factor(ausualpl_cat) + as.factor(cover_cat) + as.factor(eth_cat), design = des, subset = domain == 1,
                  family = binomial(link = "logit"))
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial
    ## glm!

``` r
summary(mam_fit2)
```

    ## 
    ## Call:
    ## svyglm(formula = mam_2 ~ as.factor(age_cat) + as.factor(educ_cat) + 
    ##     as.factor(finc_cat) + as.factor(ausualpl_cat) + as.factor(cover_cat) + 
    ##     as.factor(eth_cat), design = des, subset = domain == 1, family = binomial(link = "logit"))
    ## 
    ## Survey design:
    ## svydesign(ids = ~PSU_P, strata = ~STRAT_P, weights = ~WTFA_SA, 
    ##     nest = TRUE, data = mam_dat)
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error t value
    ## (Intercept)                                  -0.30090    0.21149  -1.423
    ## as.factor(age_cat)50–64                      -0.05683    0.09711  -0.585
    ## as.factor(age_cat)65+                        -0.38801    0.09908  -3.916
    ## as.factor(educ_cat)High school               -0.35882    0.09215  -3.894
    ## as.factor(educ_cat)Less than high school     -0.54116    0.12335  -4.387
    ## as.factor(educ_cat)Some college              -0.23029    0.09735  -2.366
    ## as.factor(finc_cat)>=200%, no further detail  0.13415    0.16736   0.802
    ## as.factor(finc_cat)>=500%                     0.59396    0.10993   5.403
    ## as.factor(finc_cat)200–299%                   0.14725    0.10455   1.408
    ## as.factor(finc_cat)300–399%                   0.36447    0.11752   3.101
    ## as.factor(finc_cat)400–499%                   0.43360    0.11706   3.704
    ## as.factor(ausualpl_cat)Other                  0.89873    1.12004   0.802
    ## as.factor(ausualpl_cat)Yes                    1.06048    0.13502   7.854
    ## as.factor(cover_cat)Private/Military          0.92479    0.16572   5.580
    ## as.factor(cover_cat)Public                    0.87454    0.17188   5.088
    ## as.factor(eth_cat)Non-Hispanic AN/AI         -0.47325    0.44354  -1.067
    ## as.factor(eth_cat)Non-Hispanic Asian         -0.78028    0.18254  -4.274
    ## as.factor(eth_cat)Non-Hispanic Black         -0.16561    0.13811  -1.199
    ## as.factor(eth_cat)Non-Hispanic White         -0.57610    0.11730  -4.911
    ##                                              Pr(>|t|)    
    ## (Intercept)                                  0.155905    
    ## as.factor(age_cat)50–64                      0.558867    
    ## as.factor(age_cat)65+                        0.000113 ***
    ## as.factor(educ_cat)High school               0.000123 ***
    ## as.factor(educ_cat)Less than high school     1.63e-05 ***
    ## as.factor(educ_cat)Some college              0.018684 *  
    ## as.factor(finc_cat)>=200%, no further detail 0.423484    
    ## as.factor(finc_cat)>=500%                    1.40e-07 ***
    ## as.factor(finc_cat)200–299%                  0.160118    
    ## as.factor(finc_cat)300–399%                  0.002123 ** 
    ## as.factor(finc_cat)400–499%                  0.000256 ***
    ## as.factor(ausualpl_cat)Other                 0.422991    
    ## as.factor(ausualpl_cat)Yes                   8.57e-14 ***
    ## as.factor(cover_cat)Private/Military         5.65e-08 ***
    ## as.factor(cover_cat)Public                   6.63e-07 ***
    ## as.factor(eth_cat)Non-Hispanic AN/AI         0.286897    
    ## as.factor(eth_cat)Non-Hispanic Asian         2.63e-05 ***
    ## as.factor(eth_cat)Non-Hispanic Black         0.231484    
    ## as.factor(eth_cat)Non-Hispanic White         1.54e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 0.9953984)
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summ(mam_fit2)
```

    ## MODEL INFO:
    ## Observations: 9422
    ## Dependent Variable: mam_2
    ## Type: Analysis of complex survey design 
    ##  Family: binomial 
    ##  Link function: logit 
    ## 
    ## MODEL FIT:
    ## Pseudo-R² (Cragg-Uhler) = 0.02
    ## Pseudo-R² (McFadden) = 0.06
    ## AIC = 10151.01 
    ## 
    ## -------------------------------------------------------------------------
    ##                                               Est.   S.E.   t val.      p
    ## ------------------------------------------ ------- ------ -------- ------
    ## (Intercept)                                  -0.30   0.21    -1.42   0.16
    ## as.factor(age_cat)50â€“64                    -0.06   0.10    -0.59   0.56
    ## as.factor(age_cat)65+                        -0.39   0.10    -3.92   0.00
    ## as.factor(educ_cat)High                      -0.36   0.09    -3.89   0.00
    ## school                                                                   
    ## as.factor(educ_cat)Less than                 -0.54   0.12    -4.39   0.00
    ## high school                                                              
    ## as.factor(educ_cat)Some                      -0.23   0.10    -2.37   0.02
    ## college                                                                  
    ## as.factor(finc_cat)>=200%,                    0.13   0.17     0.80   0.42
    ## no further detail                                                        
    ## as.factor(finc_cat)>=500%                     0.59   0.11     5.40   0.00
    ## as.factor(finc_cat)200â€“299%                 0.15   0.10     1.41   0.16
    ## as.factor(finc_cat)300â€“399%                 0.36   0.12     3.10   0.00
    ## as.factor(finc_cat)400â€“499%                 0.43   0.12     3.70   0.00
    ## as.factor(ausualpl_cat)Other                  0.90   1.12     0.80   0.42
    ## as.factor(ausualpl_cat)Yes                    1.06   0.14     7.85   0.00
    ## as.factor(cover_cat)Private/Military          0.92   0.17     5.58   0.00
    ## as.factor(cover_cat)Public                    0.87   0.17     5.09   0.00
    ## as.factor(eth_cat)Non-Hispanic               -0.47   0.44    -1.07   0.29
    ## AN/AI                                                                    
    ## as.factor(eth_cat)Non-Hispanic               -0.78   0.18    -4.27   0.00
    ## Asian                                                                    
    ## as.factor(eth_cat)Non-Hispanic               -0.17   0.14    -1.20   0.23
    ## Black                                                                    
    ## as.factor(eth_cat)Non-Hispanic               -0.58   0.12    -4.91   0.00
    ## White                                                                    
    ## -------------------------------------------------------------------------
    ## 
    ## Estimated dispersion parameter = 1

``` r
Anova(mam_fit2, type = 3)
```

    ## Analysis of Deviance Table (Type III tests)
    ## 
    ## Response: mam_2
    ##                         Df   Chisq Pr(>Chisq)    
    ## (Intercept)              1  2.0243     0.1548    
    ## as.factor(age_cat)       2 25.2488  3.291e-06 ***
    ## as.factor(educ_cat)      3 22.2496  5.788e-05 ***
    ## as.factor(finc_cat)      5 35.6070  1.138e-06 ***
    ## as.factor(ausualpl_cat)  2 62.3093  2.949e-14 ***
    ## as.factor(cover_cat)     2 31.2365  1.648e-07 ***
    ## as.factor(eth_cat)       4 36.4525  2.335e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Age cateogry, education level, financial status, usual care, insurance type, and ethnicity are significant predictors of having a recent mammogram. 
The OR for having a recent mammogram when comparing 50-64 year old women to 40-49 year old women is 0.9448. 
The OR for having a recent mammogram when comparing 65+ women to 40-49 year old women is 0.6784. 
The OR for having a recent mammogram when comparing women who only have a high school degree to women who have a college degree is 0.6985. 
The OR for having a recent mammogram when comparing women who have less than a high school degree to women who have a college degree is 0.5821. 
The OR for having a recent mammogram when comparing women who have some college education to women who have a college degree is 0.7943. 
The OR for having a recent mammogram when comparing women who are only known as having a poverty ratio of &gt;200% to women who have a poverty ratio &lt;200% is 1.1436. 
The OR for having a recent mammogram when comparing women who have a poverty ratio between 200-299% to women who have a poverty ratio &lt;200% is 1.1586. 
The OR for having a recent mammogram when comparing women who have a poverty ratio between 300-399% to women who have a poverty ratio &lt;200% is 1.4398. 
The OR for having a recent mammogram when comparing women who have a poverty ratio between 400-499% to women who have a poverty ratio &lt;200% is 1.5428. 
The OR for having a recent mammogram when comparing women who have a poverty ratio &gt;=500% to women who have a poverty ratio &lt;200% is 1.8111. 
The OR for having a recent mammogram when comparing having a source of usual care to not having a source of usual care is 2.8878. 
The OR for having a recent mammogram when comparing having private/military insurance to none is 2.5213. 
The OR for having a recent mammogram when comparing having public insurance to none is 2.3978. 
The OR for having a recent mammogram when comparing Non-Hispanic AN/AI women to Hispanic women is 0.623. 
The OR for having a recent mammogram when comparing Non-Hispanic Asian women to Hispanic women is 0.4583. 
The OR for having a recent mammogram when comparing Non-Hispanic Black women to Hispanic women is 0.8474. 
The OR for having a recent mammogram when comparing Non-Hispanic White women to Hispanic women is 0.5621. 

The groups least likely to have had a recent mammogram are women aged 65+, women who have less than a high school degree, women who have a poverty ratio &lt;200%, women who do not have a usual source of care, women who have no insurance, and Non-Hispanic women.
